{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

module Sound.Files.Sndfile.Buffer.Storable where

import C2HS
import Control.Monad (liftM)
import Data.Array.Storable
import Sound.Files.Sndfile.Buffer
import Sound.Files.Sndfile.Interface

type IOFunc a = HandlePtr -> Ptr a -> CLLong -> IO CLLong

{-# INLINE hIOSamples #-}
hIOSamples :: (Storable a) =>
                 IOFunc a
              -> Handle -> (StorableArray Index a) -> Count
              -> IO Count
hIOSamples cFunc (Handle info handle) buffer count = do
    size <- liftM rangeSize $ getBounds buffer
    checkSampleBounds size (channels info) count
    result <- withStorableArray buffer $
                \ptr -> liftM fromIntegral $ cFunc handle ptr (cIntConv count)
    checkHandle handle
    touchStorableArray buffer
    return $ cIntConv result

{-# INLINE hIOFrames #-}
hIOFrames :: (Storable a) =>
                IOFunc a
             -> Handle -> (StorableArray Index a) -> Count
             -> IO Count
hIOFrames cFunc (Handle info handle) buffer count = do
    let nc = channels info
    size <- getBounds buffer >>= (\bounds -> return $ cIntConv $ (rangeSize bounds) `quot` nc)
    checkFrameBounds size (channels info) count
    result <- withStorableArray buffer $
                \ptr -> liftM fromIntegral $ cFunc handle ptr (cIntConv count)
    checkHandle handle
    touchStorableArray buffer
    return $ cIntConv result

foreign import ccall unsafe "sf_read_double" sf_read_double :: IOFunc Double
foreign import ccall unsafe "sf_write_double" sf_write_double :: IOFunc Double

instance MBuffer StorableArray Double IO where
    hGetSamples = hIOSamples sf_read_double
    hGetFrames  = hIOFrames  sf_read_double
    hPutSamples = hIOSamples sf_write_double
    hPutFrames  = hIOFrames  sf_write_double

foreign import ccall unsafe "sf_read_float" sf_read_float :: IOFunc Float
foreign import ccall unsafe "sf_write_float" sf_write_float :: IOFunc Float

instance MBuffer StorableArray Float IO where
    hGetSamples = hIOSamples sf_read_float
    hGetFrames  = hIOFrames  sf_read_float
    hPutSamples = hIOSamples sf_write_float
    hPutFrames  = hIOFrames  sf_write_float

-- EOF