{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Archive.Internal where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Foreign
import Foreign.C.String
import Foreign.C.Types

data Archive

data Entry

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

newArchive :: IO (Ptr Archive)
newArchive = do
    p <- archiveReadNew
    archiveReadSupportFilterAll p
    archiveReadSupportFormatAll p
    return p

getNextEntry :: Ptr Archive -> IO (Maybe (FilePath, ByteString))
getNextEntry archive = alloca $ \pentry -> do
    status <- archiveReadNextHeader archive pentry
    case status of
        0 -> do
            entry <- peek pentry
            path <- archiveEntryPathname entry >>= peekCString
            size <- archiveEntrySize entry
            dat <- do
                dat <- mallocArray $ fromIntegral size
                _ <- archiveReadData archive dat size
                B.packCString dat
            return $ Just (path, dat)
        _ -> return Nothing
