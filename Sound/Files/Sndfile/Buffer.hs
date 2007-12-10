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

class (MArray a e m) => MBuffer a e m where
    -- |Get samples from handle.
    hGetSamples :: Handle -> a Index e -> Count -> m Count
    -- |Get frames from handle.
    hGetFrames  :: Handle -> a Index e -> Count -> m Count
    -- |Write samples to handle.
    hPutSamples :: Handle -> a Index e -> Count -> m Count
    -- |Write frames to handle.
    hPutFrames  :: Handle -> a Index e -> Count -> m Count

-- EOF
