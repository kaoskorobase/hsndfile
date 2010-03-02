{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.File.Sndfile.Buffer.Internal
(
    checkSampleBounds
  , checkFrameBounds
  , IOFunc
  , hBufIO
  , sf_read_word16
  , sf_readf_word16
  , sf_write_word16
  , sf_writef_word16
  , sf_read_word32
  , sf_readf_word32
  , sf_write_word32
  , sf_writef_word32
  , sf_read_float
  , sf_readf_float
  , sf_write_float
  , sf_writef_float
  , sf_read_double
  , sf_readf_double
  , sf_write_double
  , sf_writef_double
) where

import Data.Word					(Word16, Word32)
import Foreign.Ptr     				(Ptr)
import Foreign.C.Types 				(CLLong)
import Sound.File.Sndfile.Exception (throw)
import Sound.File.Sndfile.Interface

checkSampleBounds :: (Monad m) => Count -> Int -> Count -> m ()
checkSampleBounds size channels count
    | (count `mod` channels) /= 0   = throw 0 ("invalid channel/count combination " ++ (show count))
    | (count < 0) || (count > size) = throw 0 ("index out of bounds")
    | otherwise = return ()

checkFrameBounds :: (Monad m) => Count -> Int -> Count -> m ()
checkFrameBounds size channels count
    | (size `mod` channels) /= 0                      = throw 0 ("invalid buffer size")
    | (count < 0) || (count > (size `quot` channels)) = throw 0 ("index out of bounds")
    | otherwise = return ()

type IOFunc a = HandlePtr -> Ptr a -> CLLong -> IO CLLong

hBufIO :: IOFunc a -> Handle -> Ptr a -> Count -> IO Count
hBufIO f (Handle info h) ptr n =
    if (n `mod` (channels info)) /= 0
        then throw 0 ("invalid buffer size")
        else do fromIntegral `fmap` f h ptr (fromIntegral n)

foreign import ccall unsafe "sf_read_short"    sf_read_word16   :: IOFunc Word16
foreign import ccall unsafe "sf_readf_short"   sf_readf_word16  :: IOFunc Word16
foreign import ccall unsafe "sf_write_short"   sf_write_word16  :: IOFunc Word16
foreign import ccall unsafe "sf_writef_short"  sf_writef_word16 :: IOFunc Word16

foreign import ccall unsafe "sf_read_int"      sf_read_word32   :: IOFunc Word32
foreign import ccall unsafe "sf_readf_int"     sf_readf_word32  :: IOFunc Word32
foreign import ccall unsafe "sf_write_int"     sf_write_word32  :: IOFunc Word32
foreign import ccall unsafe "sf_writef_int"    sf_writef_word32 :: IOFunc Word32

foreign import ccall unsafe "sf_read_float"    sf_read_float    :: IOFunc Float
foreign import ccall unsafe "sf_readf_float"   sf_readf_float   :: IOFunc Float
foreign import ccall unsafe "sf_write_float"   sf_write_float   :: IOFunc Float
foreign import ccall unsafe "sf_writef_float"  sf_writef_float  :: IOFunc Float

foreign import ccall unsafe "sf_read_double"   sf_read_double   :: IOFunc Double
foreign import ccall unsafe "sf_readf_double"  sf_readf_double  :: IOFunc Double
foreign import ccall unsafe "sf_write_double"  sf_write_double  :: IOFunc Double
foreign import ccall unsafe "sf_writef_double" sf_writef_double :: IOFunc Double
