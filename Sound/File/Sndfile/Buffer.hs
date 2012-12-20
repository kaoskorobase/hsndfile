{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
-- | This module provides the 'Buffer' type class that abstracts the array type that is being used for I\/O. For concrete instances see for example the /hsndfile-vector/ package <http://hackage.haskell.org/package/hsndfile-vector>.
module Sound.File.Sndfile.Buffer (
    module Sound.File.Sndfile.Buffer.Sample
  , Buffer(..)
  , hGetBuffer
  , hGetContents
  , readFile
  , hPutBuffer
  , writeFile
) where

import Control.Exception (bracket)
import Control.Monad
import Foreign
import Prelude hiding (readFile, writeFile)
import Sound.File.Sndfile.Interface
import Sound.File.Sndfile.Buffer.Sample (Sample(..))

-- | Buffer class for I\/O on soundfile handles.
class Buffer a e where
    -- | Construct a buffer from a 'ForeignPtr', a start index and the element count.
    fromForeignPtr :: ForeignPtr e -> Int -> Int -> IO (a e)
    -- | Retrieve from a buffer a 'ForeignPtr' pointing to its data, a start index and an element count.
    toForeignPtr   :: a e -> IO (ForeignPtr e, Int, Int)

-- | Return an buffer with the requested number of frames of data.
--
-- The resulting buffer size is equal to the product of the number of frames `n' and the number of channels in the soundfile.
hGetBuffer :: forall a e . (Sample e, Storable e, Buffer a e) => Handle -> Count -> IO (Maybe (a e))
hGetBuffer h n = do
    fp <- mallocForeignPtrArray (nc * n)
    n' <- withForeignPtr fp $ flip (hGetBuf h) n
    if n' == 0
        then return Nothing
        else liftM Just $ fromForeignPtr fp 0 (nc * n')
    where
        nc = channels (hInfo h)

-- | Return the contents of a handle open for reading in a single buffer.
hGetContents :: (Sample e, Buffer a e) => Handle -> IO (Info, Maybe (a e))
hGetContents h = (,) info `fmap` hGetBuffer h (frames info)
    where info = hInfo h

-- | Return the contents of a file in a single buffer.
readFile :: (Sample e, Buffer a e) => FilePath -> IO (Info, Maybe (a e))
readFile path = do
    bracket
      (openFile path ReadMode defaultInfo)
      (hClose)
      (hGetContents)

-- | Write the contents of a buffer to a handle open for writing.
--
-- Return the number of frames written.
hPutBuffer :: forall a e . (Sample e, Storable e, Buffer a e) => Handle -> a e -> IO Count
hPutBuffer h buffer = do
    (fp, i, n) <- toForeignPtr buffer
    if n `mod` numChannels /= 0
        then error "hPutBuffer: invalid buffer size (not a multiple of channel count)"
        else withForeignPtr fp $ \ptr ->
              hPutBuf h (ptr `advancePtr` i) (n `div` numChannels)
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
