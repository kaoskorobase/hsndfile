{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.File.Sndfile.Buffer.Internal
(
    IOFunc
  , hBufIO
  , sf_readf_int16
  , sf_writef_int16
  , sf_readf_int32
  , sf_writef_int32
  , sf_readf_float
  , sf_writef_float
  , sf_readf_double
  , sf_writef_double
) where

import Data.Int (Int16, Int32)
import Foreign.Ptr     				(Ptr)
import Foreign.C.Types 				(CLLong(..))
import Sound.File.Sndfile.Interface (Count, Handle(..), HandlePtr)

type IOFunc a = HandlePtr -> Ptr a -> CLLong -> IO CLLong

hBufIO :: IOFunc a -> Handle -> Ptr a -> Count -> IO Count
hBufIO f h ptr = fmap fromIntegral . f (hPtr h) ptr . fromIntegral

foreign import ccall unsafe "sf_readf_short"   sf_readf_int16  :: IOFunc Int16
foreign import ccall unsafe "sf_writef_short"  sf_writef_int16 :: IOFunc Int16

foreign import ccall unsafe "sf_readf_int"     sf_readf_int32  :: IOFunc Int32
foreign import ccall unsafe "sf_writef_int"    sf_writef_int32 :: IOFunc Int32

foreign import ccall unsafe "sf_readf_float"   sf_readf_float   :: IOFunc Float
foreign import ccall unsafe "sf_writef_float"  sf_writef_float  :: IOFunc Float

foreign import ccall unsafe "sf_readf_double"  sf_readf_double  :: IOFunc Double
foreign import ccall unsafe "sf_writef_double" sf_writef_double :: IOFunc Double
