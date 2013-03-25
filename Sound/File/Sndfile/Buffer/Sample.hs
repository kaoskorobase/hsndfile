module Sound.File.Sndfile.Buffer.Sample (
    Sample(..)
) where

import Data.Int (Int16, Int32)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import Prelude hiding (interact)
import Sound.File.Sndfile.Buffer.Internal
import Sound.File.Sndfile.Interface

-- |The class Sample is used for polymorphic I\/O on a 'Handle', and is parameterized with the element type that is to be read from a file.
--
-- It is important to note that the data type used by the calling program and the data format of the file do not need to be the same. For instance, it is possible to open a 16 bit PCM encoded WAV file and read the data in floating point format. The library seamlessly converts between the two formats on-the-fly; the Haskell interface currently supports reading and writing 'Double' or 'Float' floating point values, as well as 'Int16' and 'Int32' integer values.
--
-- When converting between integer data and floating point data, the following rules apply: The default behaviour when reading floating point data from a file with integer data is normalisation. Regardless of whether data in the file is 8, 16, 24 or 32 bit wide, the data will be read as floating point data in the range [-1.0, 1.0]. Similarly, data in the range [-1.0, 1.0] will be written to an integer PCM file so that a data value of 1.0 will be the largest allowable integer for the given bit width. This normalisation can be turned on or off using the command interface (/implementation missing in Haskell/).
--
-- 'hGetSamples' and 'hGetFrames' return the number of items read. Unless the end of the file was reached during the read, the return value should equal the number of items requested. Attempts to read beyond the end of the file will not result in an error but will cause the read functions to return less than the number of items requested or 0 if already at the end of the file.

class Storable e => Sample e where
    -- | Read a buffer of frames.
    hGetBuf :: Handle -> Ptr e -> Count -> IO Count
    -- | Write a buffer of frames.
    hPutBuf :: Handle -> Ptr e -> Count -> IO Count

instance Sample Int16 where
    hGetBuf = hBufIO sf_readf_int16
    hPutBuf = hBufIO sf_writef_int16

instance Sample Int32 where
    hGetBuf = hBufIO sf_readf_int32
    hPutBuf = hBufIO sf_writef_int32

instance Sample Float where
    hGetBuf = hBufIO sf_readf_float
    hPutBuf = hBufIO sf_writef_float

instance Sample Double where
    hGetBuf = hBufIO sf_readf_double
    hPutBuf = hBufIO sf_writef_double
