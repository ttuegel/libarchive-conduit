module Codec.Archive
       ( sourceArchive
       , ArchiveException(..)
       ) where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit

import Codec.Archive.Internal

-- | Stream an archive from disk as a 'Source' of paths in the archive and their
-- contents. The archive may be in any format supported by libarchive. The
-- contents of each file is presented as a strict 'ByteString' because
-- libarchive only supports streaming archives in order; if lazy 'ByteString's
-- were used, the evaluation order could become inconsistent. Throws an
-- 'ArchiveException' if an error occurs.
sourceArchive :: MonadResource m
              => FilePath  -- ^ path to archive
              -> Source m (FilePath, ByteString)
              -- ^ stream of paths in archive and their contents
sourceArchive path = bracketP (readArchive path) free go
  where
    free = void . archiveReadFree
    go p = do
        mentry <- liftIO $ getNextEntry p
        case mentry of
            Nothing -> return ()
            Just entry -> yield entry >> go p
