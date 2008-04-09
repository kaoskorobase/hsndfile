{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

module Sound.File.Sndfile.Buffer.IOCArray where

import C2HS
import Control.Monad (liftM)
import Data.Array.IOCArray
import Sound.File.Sndfile.Buffer
import Sound.File.Sndfile.Interface

type IOFunc a = HandlePtr -> Ptr a -> CLLong -> IO CLLong

{-# INLINE hIO #-}
hIO :: (Storable a) =>
           (Count -> Int -> Count -> IO ())
        -> IOFunc a
        -> Handle -> (IOCArray Index a) -> Count
        -> IO Count
hIO checkBounds ioFunc (Handle info handle) buffer count = do
    size <- liftM rangeSize $ getBounds buffer
    checkBounds size (channels info) count
    result <- withIOCArray buffer $
                \ptr -> liftM fromIntegral $ ioFunc handle ptr (cIntConv count)
    checkHandle handle
    touchIOCArray buffer
    return $ cIntConv result

foreign import ccall unsafe "sf_read_double"  sf_read_double  :: IOFunc Double
foreign import ccall unsafe "sf_write_double" sf_write_double :: IOFunc Double

instance MBuffer IOCArray Double IO where
    hGetSamples = hIO checkSampleBounds sf_read_double
    hGetFrames  = hIO checkFrameBounds  sf_read_double
    hPutSamples = hIO checkSampleBounds sf_write_double
    hPutFrames  = hIO checkFrameBounds  sf_write_double

foreign import ccall unsafe "sf_read_float"  sf_read_float  :: IOFunc Float
foreign import ccall unsafe "sf_write_float" sf_write_float :: IOFunc Float

instance MBuffer IOCArray Float IO where
    hGetSamples = hIO checkSampleBounds sf_read_float
    hGetFrames  = hIO checkFrameBounds  sf_read_float
    hPutSamples = hIO checkSampleBounds sf_write_float
    hPutFrames  = hIO checkFrameBounds  sf_write_float

-- EOF
