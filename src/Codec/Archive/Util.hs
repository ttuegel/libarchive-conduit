{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Archive.Util where

import Control.Exception
import Data.Typeable
import Foreign
import Foreign.C

data Archive

data Entry

data ArchiveException = ArchiveException String
  deriving (Show, Typeable)

instance Exception ArchiveException

checkArchiveError :: Ptr Archive -> CInt -> IO Bool
checkArchiveError archive code
    | code >= 0 = return $ code == 1
    | otherwise = throwArchiveException archive

foreign import ccall "archive.h archive_error_string"
    archiveErrorString :: Ptr Archive -> IO CString

throwArchiveException :: Ptr Archive -> IO a
throwArchiveException archive = do
    pstr <- archiveErrorString archive
    str <- peekCString pstr
    throw $ ArchiveException str
