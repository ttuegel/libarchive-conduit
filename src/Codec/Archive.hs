module Codec.Archive where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit

import Codec.Archive.Internal

archiveSource :: MonadResource m => FilePath -> Source m (FilePath, ByteString)
archiveSource path = bracketP newArchive free go
  where
    free = void . archiveReadFree
    go p = do
        entry <- liftIO $ getNextEntry p
        case entry of
            Nothing -> return ()
            Just (path, dat) -> yield (path, dat) >> go p
