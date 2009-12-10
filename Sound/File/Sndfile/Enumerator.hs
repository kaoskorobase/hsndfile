{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module Sound.File.Sndfile.Enumerator (
    enumHandle
) where

import Sound.File.Sndfile.Buffer (hGetBuf, Sample)
import Sound.File.Sndfile.Exception
import Sound.File.Sndfile.Interface

import Data.Iteratee.Base.StreamChunk (ReadableChunk(..))
import Data.Iteratee.Base
import Foreign (allocaBytes, sizeOf)
-- import Data.Iteratee.Binary()

-- |The enumerator of a file Handle.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
enumHandle :: forall a s el . (Sample el, ReadableChunk s el) => Handle -> EnumeratorGM s el IO a
enumHandle h i = allocaBytes (fromIntegral bufferSize) $ loop i
    where
        frameSize  = sizeOf (undefined :: el) * (channels.hInfo) h -- size of frame in bytes
        bufferSize = 4096 - (mod 4096 frameSize)                   -- buffer size in bytes
        numFrames  = bufferSize `div` frameSize                    -- number of frames to read
        loop iter p = do
            n <- (try $ hGetBuf h p numFrames) :: IO (Either Exception Int)
            case n of
                Left _   -> enumErr "IO error" iter
                Right 0  -> return iter
                Right n' -> do
                    s   <- readFromPtr p (fromIntegral n' * frameSize)
                    igv <- runIter iter (Chunk s)
                    check p igv
        check _p (Done x _)        = return . return $ x
        check p  (Cont i' Nothing) = loop i' p
        check _p (Cont _ (Just e)) = return $ throwErr e
