{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sound.File.Sndfile.Buffer
(
    MBuffer(..),
    checkSampleBounds, checkFrameBounds,
    hReadSamples, hReadFrames,
    interact,
    IOFunc,
    sf_read_double, sf_readf_double,
    sf_write_double, sf_writef_double,
    sf_read_float, sf_readf_float,
    sf_write_float, sf_writef_float
) where

import Control.Monad (liftM, when)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.MArray (Ix, MArray, getBounds, newArray_)
import Data.Ix (rangeSize)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CLLong)
import Prelude hiding (interact)
import Sound.File.Sndfile.Interface

checkSampleBounds :: (Monad m) => Count -> Int -> Count -> m ()
checkSampleBounds size channels count
    | (count `mod` channels) /= 0 = throw (Exception ("invalid channel/count combination " ++ (show count)))
    | (count < 0) || (count > size) = throw (Exception "index out of bounds")
    | otherwise = return ()

checkFrameBounds :: (Monad m) => Count -> Int -> Count -> m ()
checkFrameBounds size channels count
    | (size `mod` channels) /= 0 = throw (Exception "invalid buffer size")
    | (count < 0) || (count > (size `quot` channels)) = throw (Exception "index out of bounds")
    | otherwise = return ()

type IOFunc a = HandlePtr -> Ptr a -> CLLong -> IO CLLong

foreign import ccall unsafe "sf_read_double"   sf_read_double   :: IOFunc Double
foreign import ccall unsafe "sf_readf_double"  sf_readf_double  :: IOFunc Double
foreign import ccall unsafe "sf_write_double"  sf_write_double  :: IOFunc Double
foreign import ccall unsafe "sf_writef_double" sf_writef_double :: IOFunc Double

foreign import ccall unsafe "sf_read_float"   sf_read_float   :: IOFunc Float
foreign import ccall unsafe "sf_readf_float"  sf_readf_float  :: IOFunc Float
foreign import ccall unsafe "sf_write_float"  sf_write_float  :: IOFunc Float
foreign import ccall unsafe "sf_writef_float" sf_writef_float :: IOFunc Float

-- |The class MBuffer is used for polymorphic I\/O on a 'Handle', and is
-- parameterized on the mutable array type, the element type and the monad
-- results are returned in.
--
-- It is important to note that the data type used by the calling program and
-- the data format of the file do not need to be the same. For instance, it is
-- possible to open a 16 bit PCM encoded WAV file and read the data in
-- floating point format. The library seamlessly converts between the two
-- formats on-the-fly; the Haskell interface only supports reading and writing
-- 'Double' or 'Float' values.
--
-- When converting between integer data and floating point data, the following
-- rules apply: The default behaviour when reading floating point data
-- ('hGetSamples' or 'hGetFrames') from a file with integer data is
-- normalisation. Regardless of whether data in the file is 8, 16, 24 or 32
-- bit wide, the data will be read as floating point data in the range
-- [-1.0, 1.0]. Similarly, data in the range [-1.0, 1.0] will be written to an
-- integer PCM file so that a data value of 1.0 will be the largest allowable
-- integer for the given bit width. This normalisation can be turned on or off
-- using the command interface [TODO: implementation missing in Haskell].
--
-- 'hGetSamples' and 'hGetFrames' return the number of items read. Unless the
-- end of the file was reached during the read, the return value should equal
-- the number of items requested. Attempts to read beyond the end of the file
-- will not result in an error but will cause the read functions to return
-- less than the number of items requested or 0 if already at the end of the
-- file.
class (MArray a e m) => MBuffer a e m where
    -- |Fill the destination array with the requested number of items. The 'count'
    -- parameter must be an integer product of the number of channels or an error
    -- will occur.
    hGetSamples :: Handle -> a Index e -> Count -> m Count
    -- |Fill the destination array with the requested number of frames of data.
    -- The array must be large enough to hold the product of frames and the number
    -- of channels or an error will occur.
    hGetFrames  :: Handle -> a Index e -> Count -> m Count
    -- |Write 'count' samples from the source array to the stream. The 'count'
    -- parameter must be an integer product of the number of channels or an error
    -- will occur.
    --
    -- 'hPutSamples' returns the number of items written (which should be the same
    -- as the 'count' parameter).
    hPutSamples :: Handle -> a Index e -> Count -> m Count
    -- |Write 'count' frames from the source array to the stream.
    -- The array must be large enough to hold the product of frames and the number
    -- of channels or an error will occur.
    --
    -- 'hPutFrames' returns the number of frames written (which should be the same
    -- as the 'count' parameter).
    hPutFrames  :: Handle -> a Index e -> Count -> m Count

-- TODO: Optimize unsafeWriteRange
unsafeWriteRange :: (MArray a e m) => a Int e -> (Int, Int) -> e -> m ()
unsafeWriteRange _ (i0, i) _ | i0 > i = return ()
unsafeWriteRange a (i0, i) e          = unsafeWrite a i0 e >> unsafeWriteRange a (i0+1,i) e

-- |Return an array with the requested number of items. The 'count' parameter
-- must be an integer product of the number of channels or an error will
-- occur.
hReadSamples :: (MBuffer a e m, Num e) => Handle -> Count -> m (Maybe (a Index e))
hReadSamples h n = do
    b  <- newArray_ (0, n-1)
    n' <- hGetSamples h b n
    if n' == 0
        then return Nothing
        else do
            when (n' < n) (unsafeWriteRange b (n',n-1) 0)
            return (Just b)

-- |Return an array with the requested number of frames of data.
-- The resulting array size is equal to the product of the number of frames
-- `n' and the number of channels in the soundfile.
hReadFrames :: (MBuffer a e m, Num e) => Handle -> Count -> m (Maybe (a Index e))
hReadFrames h n = do
    b  <- newArray_ (0, si)
    n' <- hGetFrames h b n
    if n' == 0
        then return Nothing
        else do
            when (n' < n) (unsafeWriteRange b (f2s n', si) 0)
            return (Just b)
    where
        f2s = (* channels (hInfo h))
        si  = (f2s n) - 1

modifyArray :: (MArray a e m, Ix i) => (e -> e) -> a i e -> Int -> Int -> m ()
modifyArray f a i n
    | i >= n = return ()
    | otherwise = do
        e <- unsafeRead a i
        unsafeWrite a i (f e)
        modifyArray f a (i+1) n

interact :: (MBuffer a e m) => (e -> e) -> a Index e -> Handle -> Handle -> m ()
interact f buffer hIn hOut = do
    s <- liftM rangeSize $ getBounds buffer
    n <- hGetSamples hIn buffer s
    when (n > 0) $ do
        modifyArray f buffer 0 n
        hPutSamples hOut buffer n
        interact f buffer hIn hOut

-- EOF
