{-# LANGUAGE ScopedTypeVariables #-}

module Sound.File.Sndfile.Buffer (
    module Sound.File.Sndfile.Buffer.Sample
  , Buffer(..)
  , hGetBuffer
  , hGetContents
  , readFile
  , hGetContentChunks
  , readFileChunks
) where

import Control.Exception (bracket)
import Foreign
import Prelude hiding (readFile)
import Sound.File.Sndfile.Interface
import Sound.File.Sndfile.Buffer.Sample
import System.IO.Unsafe (unsafeInterleaveIO)

class Buffer a e where
    fromForeignPtr :: ForeignPtr e -> Count -> IO (a e)
    toForeignPtr :: a e -> IO (ForeignPtr e, Int, Int)
    -- unsafeFromPtr :: Ptr e -> Count -> IO (a e)
    -- unsafeToPtr :: a e -> IO (Ptr e, Count)

-- |Return an array with the requested number of items. The 'count' parameter
-- must be an integer product of the number of channels or an error will
-- occur.
-- hReadSamples :: forall ia ma . (Sample ia ma e m, Num e, Monad m) => Handle -> Count -> m (Maybe (IBuffer e))
-- hReadSamples h n = do
--     b  <- newBuffer (0, n-1)
--     n' <- hGetSamples h b n
--     if n' == 0
--         then return Nothing
--         else do
--             when (n' < n) (unsafeWriteRange 0 b (n', n-1))
--             unsafeFreeze b >>= return . Just

-- |Return an array with the requested number of frames of data.
-- The resulting array size is equal to the product of the number of frames
-- `n' and the number of channels in the soundfile.
hGetBuffer :: forall a e . (Sample e, Storable e, Buffer a e) => Handle -> Count -> IO (Maybe (a e))
hGetBuffer h n = do
    p <- mallocBytes (sizeOf (undefined :: e) * numChannels * n)
    n' <- hGetBuf h p n
    if n' == 0
        then return Nothing
        else newForeignPtr_ p >>= flip fromForeignPtr (n * numChannels) >>= return . Just
    where
        numChannels = (channels.hInfo) h

-- hReadFrames :: (Sample ia ma e m, Num e, Monad m) => Handle -> Count -> m (Maybe (IBuffer e))
-- hReadFrames h n = do
--     b  <- newBuffer (0, si)
--     n' <- hGetFrames h b n
--     if n' == 0
--         then return Nothing
--         else do
--             when (n' < n) (unsafeWriteRange 0 b (f2s n', si))
--             unsafeFreeze b >>= return . Just
--     where
--         f2s = (* channels (hInfo h))
--         si  = (f2s n) - 1

hGetContents :: (Sample e, Buffer a e) => Handle -> IO (Info, Maybe (a e))
hGetContents h = (,) info `fmap` hGetBuffer h (frames info)
    where info = hInfo h

readFile :: (Sample e, Buffer a e) => FilePath -> IO (Info, Maybe (a e))
readFile path = do
    bracket
      (openFile path ReadMode defaultInfo)
      (hClose)
      (hGetContents)

hGetContentChunks :: (Sample e, Buffer a e) => Count -> Handle -> IO (Info, [a e])
hGetContentChunks n h = (,) (hInfo h) `fmap` lazyread
 where
     {-# NOINLINE lazyread #-}
     lazyread = unsafeInterleaveIO loop
     loop = do
         r <- hGetBuffer h n
         case r of
             Just b -> do
                 bs <- lazyread
                 return (b:bs)
             Nothing -> return []

readFileChunks :: (Sample e, Buffer a e) => Count -> FilePath -> IO (Info, [a e])
readFileChunks n path = do
    bracket
      (openFile path ReadMode defaultInfo)
      (hClose)
      (hGetContentChunks n)

-- interact :: (MArray a e m, Sample e m) => (e -> e) -> MBuffer e -> Handle -> Handle -> m ()
-- interact f buffer hIn hOut = do
--     s <- liftM rangeSize $ getBounds buffer
--     n <- hGetSamples hIn buffer s
--     when (n > 0) $ do
--         modifyArray f buffer 0 n
--         hPutSamples hOut buffer n
--         interact f buffer hIn hOut
