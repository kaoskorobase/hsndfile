{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
-- | This module provides the 'Buffer' type class that abstracts the array type that is being used for I\/O. For concrete instances see for example the /hsndfile-vector/ package <http://hackage.haskell.org/package/hsndfile-vector>.
module Sound.File.Sndfile.Buffer (
    module Sound.File.Sndfile.Buffer.Sample
  , Buffer(..)
  , hGetBuffer
  , hGetContents
  , readFile
  , hGetContentChunks
  , readFileChunks
  , hPutBuffer
  , writeFile
) where

import Control.Exception (bracket)
import Foreign
import Prelude hiding (readFile, writeFile)
import Sound.File.Sndfile.Exception (throw)
import Sound.File.Sndfile.Interface
import Sound.File.Sndfile.Buffer.Sample (Sample(..))
import System.IO.Unsafe (unsafeInterleaveIO)

-- | Buffer class for I\/O on soundfile handles.
class Buffer a e where
    -- | Construct a buffer from a 'ForeignPtr', a start index and the element count.
    fromForeignPtr :: ForeignPtr e -> Int -> Int -> IO (a e)
    -- | Retrieve from a buffer a 'ForeignPtr' pointing to its data, a start index and an element count.
    toForeignPtr   :: a e -> IO (ForeignPtr e, Int, Int)

-- | Return an buffer with the requested number of frames of data.
-- The resulting buffer size is equal to the product of the number of frames `n' and the number of channels in the soundfile.
hGetBuffer :: forall a e . (Sample e, Storable e, Buffer a e) => Handle -> Count -> IO (Maybe (a e))
hGetBuffer h n = do
    p <- mallocBytes (sizeOf (undefined :: e) * numChannels * n)
    n' <- hGetBuf h p n
    if n' == 0
        then return Nothing
        else do
            fp <- newForeignPtr_ p
            Just `fmap` fromForeignPtr fp 0 (n * numChannels)
    where
        numChannels = (channels.hInfo) h

-- | Return the contents of a handle open for reading in a single buffer.
hGetContents :: (Sample e, Buffer a e) => Handle -> IO (Info, Maybe (a e))
hGetContents h = (,) info `fmap` hGetBuffer h (frames info)
    where info = hInfo h

-- | Return the contents of a handle open for reading as a lazy list of buffers.
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

-- | Return the contents of a file in a single buffer.
readFile :: (Sample e, Buffer a e) => FilePath -> IO (Info, Maybe (a e))
readFile path = do
    bracket
      (openFile path ReadMode defaultInfo)
      (hClose)
      (hGetContents)

-- | Return the contents of a file as a lazy list of buffers.
readFileChunks :: (Sample e, Buffer a e) => Count -> FilePath -> IO (Info, [a e])
readFileChunks n path = do
    bracket
      (openFile path ReadMode defaultInfo)
      (hClose)
      (hGetContentChunks n)

-- | Write the contents of a buffer to a handle open for writing.
-- Return the number of frames written.
hPutBuffer :: forall a e . (Sample e, Storable e, Buffer a e) => Handle -> a e -> IO Count
hPutBuffer h buffer = do
    (fp, i, n) <- toForeignPtr buffer
    if n `mod` numChannels /= 0
        then throw 0 "hPutBuffer: invalid buffer size (not a multiple of channel count)"
        else do
            withForeignPtr fp $ \ptr -> do
                let p = plusPtr ptr (sizeOf (undefined :: e) * i) :: Ptr e
                hPutBuf h p (n `div` numChannels)
    where
        numChannels = channels $ hInfo h

-- | Write the contents of a buffer to a file.
-- Return the number of frames written.
writeFile :: (Sample e, Buffer a e) => Info -> FilePath -> a e -> IO Count
writeFile info path buffer = do
    bracket
        (openFile path WriteMode info)
        (hClose)
        (flip hPutBuffer buffer)
