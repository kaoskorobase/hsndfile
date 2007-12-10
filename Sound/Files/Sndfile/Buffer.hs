{-# LANGUAGE MultiParamTypeClasses #-}

module Sound.Files.Sndfile.Buffer (
    MBuffer(..),
    checkSampleBounds, checkFrameBounds
) where

import C2HS
import Data.Array.MArray (MArray, getBounds)
--import Data.Array.IArray (IArray)
import Data.Ix (rangeSize)
import Sound.Files.Sndfile.Interface

checkSampleBounds :: (Monad m) => Count -> Int -> Count -> m ()
checkSampleBounds size channels count
    | (count `mod` channels) /= 0 = throw (Exception "invalid channel/count combination")
    | (count < 0) || (count >= size) = throw (Exception "index out of bounds")
    | otherwise = return ()

checkFrameBounds :: (Monad m) => Count -> Int -> Count -> m ()
checkFrameBounds size channels count
    | (size `mod` channels) /= 0 = throw (Exception "invalid buffer size")
    | (count < 0) || (count >= (size `quot` channels)) = throw (Exception "index out of bounds")
    | otherwise = return ()

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

-- EOF
