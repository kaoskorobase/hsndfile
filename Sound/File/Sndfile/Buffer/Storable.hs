{-# OPTIONS_GHC -fglasgow-exts #-}

module Sound.File.Sndfile.Buffer.Storable where

import C2HS
import Control.Monad (liftM)
import Data.Array.Storable
import Sound.File.Sndfile.Buffer
import Sound.File.Sndfile.Interface

{-# INLINE hIO #-}
hIO :: (Storable a) =>
           (Count -> Int -> Count -> IO ())
        -> IOFunc a
        -> Handle -> (StorableArray Index a) -> Count
        -> IO Count
hIO checkBounds ioFunc (Handle info handle) buffer count = do
    size <- liftM rangeSize $ getBounds buffer
    checkBounds size (channels info) count
    result <- withStorableArray buffer $
                \ptr -> liftM fromIntegral $ ioFunc handle ptr (cIntConv count)
    checkHandle handle
    touchStorableArray buffer
    return $ cIntConv result

instance MBuffer StorableArray Double IO where
    hGetSamples = hIO checkSampleBounds sf_read_double
    hGetFrames  = hIO checkFrameBounds  sf_readf_double
    hPutSamples = hIO checkSampleBounds sf_write_double
    hPutFrames  = hIO checkFrameBounds  sf_writef_double

instance MBuffer StorableArray Float IO where
    hGetSamples = hIO checkSampleBounds sf_read_float
    hGetFrames  = hIO checkFrameBounds  sf_readf_float
    hPutSamples = hIO checkSampleBounds sf_write_float
    hPutFrames  = hIO checkFrameBounds  sf_writef_float

-- EOF
