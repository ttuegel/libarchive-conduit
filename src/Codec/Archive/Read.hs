{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Archive.Read where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Codec.Archive.Util

foreign import ccall "archive.h archive_read_new"
    archiveReadNew :: IO (Ptr Archive)

foreign import ccall "archive.h archive_read_free"
    archiveReadFree :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_read_support_filter_all"
    archiveReadSupportFilterAll :: Ptr Archive -> IO ()

foreign import ccall "archive.h archive_read_support_format_all"
    archiveReadSupportFormatAll :: Ptr Archive -> IO ()

foreign import ccall "archive.h archive_read_open_filename"
    archiveReadOpenFilename :: Ptr Archive -> CString -> CSize -> IO CInt

foreign import ccall "archive.h archive_read_next_header"
    archiveReadNextHeader :: Ptr Archive -> Ptr (Ptr Entry) -> IO CInt

foreign import ccall "archive.h archive_read_data"
    archiveReadData :: Ptr Archive -> CString -> CSize -> IO CSize

foreign import ccall "archive.h archive_entry_pathname"
    archiveEntryPathname :: Ptr Entry -> IO CString

foreign import ccall "archive.h archive_entry_size"
    archiveEntrySize :: Ptr Entry -> IO CSize

readArchive :: FilePath -> IO (Ptr Archive)
readArchive path = do
    p <- archiveReadNew
    archiveReadSupportFilterAll p
    archiveReadSupportFormatAll p
    _ <- withCString path $ \cpath ->
        archiveReadOpenFilename p cpath (64 * 1024) >>= checkArchiveError p
    return p

getNextEntry :: Ptr Archive -> IO (Maybe (FilePath, ByteString))
getNextEntry archive = alloca $ \pentry -> do
    eof <- archiveReadNextHeader archive pentry >>= checkArchiveError archive
    if eof then return Nothing
      else do
        entry <- peek pentry
        path <- archiveEntryPathname entry >>= peekCString
        size <- archiveEntrySize entry
        dat <- allocaArray (fromIntegral size) $ \dat -> do
            size' <- archiveReadData archive dat size
            when (size' < 0) $ throwArchiveException archive
            B.packCStringLen (dat, fromIntegral size')
        return $ Just (path, dat)
