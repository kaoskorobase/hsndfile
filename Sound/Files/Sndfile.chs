{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

-- | "Sound.Files.Sndfile" provides a Haskell interface to the ubiquitous
-- libsndfile library by Erik de Castro Lopo (visit the author's website at
-- <http://www.mega-nerd.com/libsndfile/>).

module Sound.Files.Sndfile
(
    -- *Types
    Count, Index,
    -- *Stream format
    Format(..), defaultFormat,
    HeaderFormat(..),
    SampleFormat(..),
    EndianFormat(..),
    -- *Stream info
    Info(..), defaultInfo, checkFormat,
    -- *Stream handle operations
    Handle, hInfo, hIsSeekable,
    IOMode(..), openFile, hFlush, hClose,
    SeekMode(..), hSeek, hSeekRead, hSeekWrite,
    -- *I\/O functions
    hGetSamplesDouble, hGetSamplesFloat,
    hGetFramesDouble, hGetFramesFloat,
    hReadFramesDouble, hReadFramesFloat,
    hPutSamplesDouble, hPutSamplesFloat,
    hPutFramesDouble, hPutFramesFloat,
    -- *Exception handling
    Exception, errorString, catch,
    -- * Header string field access
    StringType(..), getString, setString
) where

import C2HS
import Control.Monad (liftM, when)
import Control.Exception (catchDyn, throwDyn)
import Data.Array.Storable
import Data.Bits
import Data.Int (Int64)
import Data.Typeable (Typeable)
--import Foreign.C.Types (CChar)
import Prelude hiding (catch)
--import System.IO (SeekMode(..))
import System.IO.Unsafe (unsafePerformIO)

#include <sndfile.h>

{#context lib="libsndfile" prefix="sf"#}

-- ====================================================================
-- Enumerations

-- libsndfile doesn't use typed enums, so we need to supply them ourselves
-- do some renaming while we're at it

-- ====================================================================
-- Types and data structures

type Count = Int64 -- ^ Type for expressing sample counts.
type Index = Int64 -- ^ Type for expressing sample indices.

-- | Header format.
{#enum HeaderFormat {underscoreToCase} deriving (Eq, Show)#}
#c
enum HeaderFormat
{
    HEADER_FORMAT_NONE  = 0,

    HEADER_FORMAT_WAV   = SF_FORMAT_WAV,
    HEADER_FORMAT_AIFF  = SF_FORMAT_AIFF,
    HEADER_FORMAT_AU    = SF_FORMAT_AU,
    HEADER_FORMAT_RAW   = SF_FORMAT_RAW,
    HEADER_FORMAT_PAF   = SF_FORMAT_PAF,
    HEADER_FORMAT_SVX   = SF_FORMAT_SVX,
    HEADER_FORMAT_NIST  = SF_FORMAT_NIST,
    HEADER_FORMAT_VOC   = SF_FORMAT_VOC,
    HEADER_FORMAT_IRCAM = SF_FORMAT_IRCAM,
    HEADER_FORMAT_W64   = SF_FORMAT_W64,
    HEADER_FORMAT_MAT4  = SF_FORMAT_MAT4,
    HEADER_FORMAT_MAT5  = SF_FORMAT_MAT5,
    HEADER_FORMAT_PVF   = SF_FORMAT_PVF,
    HEADER_FORMAT_XI    = SF_FORMAT_XI,
    HEADER_FORMAT_HTK   = SF_FORMAT_HTK,
    HEADER_FORMAT_SDS   = SF_FORMAT_SDS,
    HEADER_FORMAT_AVR   = SF_FORMAT_AVR,
    HEADER_FORMAT_WAVEX = SF_FORMAT_WAVEX,
    HEADER_FORMAT_SD2   = SF_FORMAT_SD2,
    HEADER_FORMAT_FLAC  = SF_FORMAT_FLAC,
    HEADER_FORMAT_CAF   = SF_FORMAT_CAF
};
#endc

-- | Sample format.
{#enum SampleFormat {underscoreToCase} deriving (Eq, Show)#}
#c
enum SampleFormat
{
    SAMPLE_FORMAT_NONE              = 0,

    SAMPLE_FORMAT_PCM_S8            = SF_FORMAT_PCM_S8,
    SAMPLE_FORMAT_PCM_16            = SF_FORMAT_PCM_16,
    SAMPLE_FORMAT_PCM_24            = SF_FORMAT_PCM_24,
    SAMPLE_FORMAT_PCM_32            = SF_FORMAT_PCM_32,
                                    
    SAMPLE_FORMAT_PCM_U8            = SF_FORMAT_PCM_U8,
                                    
    SAMPLE_FORMAT_FLOAT             = SF_FORMAT_FLOAT,
    SAMPLE_FORMAT_DOUBLE            = SF_FORMAT_DOUBLE,
                                    
    SAMPLE_FORMAT_ULAW              = SF_FORMAT_ULAW,
    SAMPLE_FORMAT_ALAW              = SF_FORMAT_ALAW,
    SAMPLE_FORMAT_IMA_ADPCM         = SF_FORMAT_IMA_ADPCM,
    SAMPLE_FORMAT_MS_ADPCM          = SF_FORMAT_MS_ADPCM,
                                    
    SAMPLE_FORMAT_GSM610            = SF_FORMAT_GSM610,
    SAMPLE_FORMAT_VOX_ADPCM         = SF_FORMAT_VOX_ADPCM,
                                    
    SAMPLE_FORMAT_G721_32           = SF_FORMAT_G721_32,
    SAMPLE_FORMAT_G723_24           = SF_FORMAT_G723_24,
    SAMPLE_FORMAT_G723_40           = SF_FORMAT_G723_40,
                                    
    SAMPLE_FORMAT_DWVW_12           = SF_FORMAT_DWVW_12,
    SAMPLE_FORMAT_DWVW_16           = SF_FORMAT_DWVW_16,
    SAMPLE_FORMAT_DWVW_24           = SF_FORMAT_DWVW_24,
    SAMPLE_FORMAT_DWVW_N            = SF_FORMAT_DWVW_N,
                             
    SAMPLE_FORMAT_FORMAT_DPCM_8     = SF_FORMAT_DPCM_8,
    SAMPLE_FORMAT_FORMAT_DPCM_16    = SF_FORMAT_DPCM_16
};
#endc

-- | Endianness.
{#enum EndianFormat {underscoreToCase} deriving (Eq, Show)#}
#c
enum EndianFormat
{
    ENDIAN_FILE     = SF_ENDIAN_FILE,
    ENDIAN_LITTLE   = SF_ENDIAN_LITTLE,
    ENDIAN_BIG      = SF_ENDIAN_BIG,
    ENDIAN_CPU      = SF_ENDIAN_CPU
};
#endc

-- only used internally
{#enum FormatMask {underscoreToCase} deriving (Eq)#}
#c
enum FormatMask
{
    FORMAT_SUB_MASK     = SF_FORMAT_SUBMASK,
    FORMAT_TYPE_MASK    = SF_FORMAT_TYPEMASK,
    FORMAT_END_MASK     = SF_FORMAT_ENDMASK
};
#endc

-- |Stream format specification, consisting of header, sample and endianness
-- formats.
--
-- Not all combinations of header, sample and endianness formats are valid;
-- valid combinamtions can be checked with the 'checkFormat' function.
data Format = Format {
    headerFormat :: HeaderFormat,
    sampleFormat :: SampleFormat,
    endianFormat :: EndianFormat
} deriving (Eq, Show)

-- |The 'Info' structure is for passing data between the calling function and
-- the library when opening a stream for reading or writing.
data Info = Info {
    frames :: Count,    -- ^Number of frames in file
    samplerate :: Int,  -- ^Audio sample rate
    channels :: Int,    -- ^Number of channels
    format :: Format,   -- ^Header and sample format
    sections :: Int,    -- ^Number of sections
    seekable :: Bool    -- ^'True' when stream is seekable (e.g. local files)
} deriving (Eq, Show)

-- |Default \'empty\' format, useful when opening files for reading with 'ReadMode'.
defaultFormat :: Format
defaultFormat = Format HeaderFormatNone SampleFormatNone EndianFile

-- |Default \'empty\' info, useful when opening files for reading with 'ReadMode'.
defaultInfo :: Info
defaultInfo   = Info 0 0 0 defaultFormat 0 False

-- Convert CInt to Format
hsFormat :: CInt -> Format
hsFormat i =
   let hf = cToEnum (i .&. (cFromEnum FormatTypeMask) .&. complement (cFromEnum FormatEndMask))
       sf = cToEnum (i .&. (cFromEnum FormatSubMask))
       ef = cToEnum (i .&. (cFromEnum FormatEndMask))
   in
       Format {
           headerFormat = hf,
           sampleFormat = sf,
           endianFormat = ef
       }

-- Convert Format to CInt
cFormat :: Format -> CInt
cFormat (Format hf sf ef) = (cFromEnum hf) .|. (cFromEnum sf) .|. (cFromEnum ef)

-- Storable instance for Info
instance Storable (Info) where
    alignment x = alignment (undefined :: CInt) -- hmm
    sizeOf x = {#sizeof INFO#}
    -- Unmarshall Info from C representation
    peek ptr = do
        frames     <- liftM fromIntegral $ {#get SF_INFO.frames#} ptr
        samplerate <- liftM fromIntegral $ {#get SF_INFO.samplerate#} ptr
        channels   <- liftM fromIntegral $ {#get SF_INFO.channels#} ptr
        format     <- liftM hsFormat     $ {#get SF_INFO.format#} ptr
        sections   <- liftM fromIntegral $ {#get SF_INFO.sections#} ptr
        seekable   <- liftM toBool       $ {#get SF_INFO.seekable#} ptr
        return $ Info {
            frames = frames,
            samplerate = samplerate,
            channels = channels,
            format = format,
            sections = sections,
            seekable = seekable
        }
    -- Marshall Info to C representation
    poke ptr info =
        do
            {#set SF_INFO.frames#} ptr     $ fromIntegral $ frames info
            {#set SF_INFO.samplerate#} ptr $ fromIntegral $ samplerate info
            {#set SF_INFO.channels#} ptr   $ fromIntegral $ channels info
            {#set SF_INFO.format#} ptr     $ cFormat      $ format info
            {#set SF_INFO.sections#} ptr   $ fromIntegral $ sections info
            {#set SF_INFO.seekable#} ptr   $ fromBool     $ seekable info

-- ====================================================================
-- Exceptions

-- |Values of type 'Exception' are thrown by the library when an error occurs.
--
-- Use 'catch' to catch only exceptions of this type.
data Exception = Exception String deriving (Typeable, Show)

-- |Return the error string associated with the 'Exception'.
errorString :: Exception -> String
errorString (Exception s) = s

-- |Catch values of type 'Exception'.
catch :: IO a -> (Exception -> IO a) -> IO a
catch = catchDyn

raiseError :: Handle -> IO ()
raiseError handle = liftM (throwDyn . Exception) $ (peekCString $ {#call pure sf_strerror#} (hPtr handle))

checkHandle :: HandlePtr -> IO HandlePtr
checkHandle handle = do
    code <- liftM fromIntegral $ {#call unsafe sf_error#} handle
    when (code /= 0) $ raiseError (Handle defaultInfo handle)
    return handle

-- ====================================================================
-- format info

-- |This function allows the caller to check if a set of parameters in the
-- 'Info' struct is valid before calling 'openFile' ('WriteMode').
--
-- 'checkFormat' returns 'True' if the parameters are valid and 'False' otherwise.

{-# NOINLINE checkFormat #-}
checkFormat :: Info -> Bool
checkFormat info =
    unsafePerformIO (with info (liftM cToBool . {#call unsafe sf_format_check#} . castPtr))

-- ====================================================================
-- handle operations

-- | Abstract file handle.
data Handle = Handle {
    hInfo :: Info,      -- ^Return the stream 'Info' associated with the 'Handle'.
    hPtr :: HandlePtr
}
type HandlePtr = Ptr ()

-- | I\/O mode.
{#enum IOMode {} deriving (Eq, Show)#}
#c
enum IOMode
{
    ReadMode        = SFM_READ,
    WriteMode       = SFM_WRITE,
    ReadWriteMode   = SFM_RDWR
};
#endc

hIsSeekable :: Handle -> IO Bool
hIsSeekable = return . seekable . hInfo

-- |When opening a file for read ('ReadMode'), the format field should be set
-- to 'defaultFormat' before calling 'openFile'. The only exception to this is
-- the case of RAW files, where the caller has to set the samplerate, channels
-- and format fields to valid values. All other fields of the structure are
-- filled in by the library.
--
-- When opening a file for write ('WriteMode'), the caller must fill in the
-- structure members samplerate, channels, and format.
--
-- Every call to 'openFile' should be matched with a call to 'hClose' to free
-- up memory allocated during the call to 'openFile'.
--
-- On success, the 'openFile' function returns a 'Handle' which should be
-- passed as the first parameter to all subsequent libsndfile calls dealing
-- with that audio stream. On fail, the 'openFile' function signals an
-- 'Exception'.

openFile :: FilePath -> IOMode -> Info -> IO Handle
openFile filePath ioMode info =
    withCString filePath (\cFilePath ->
        with info (\cInfo -> do
            cHandle <- {#call unsafe sf_open#}
                            cFilePath (cFromEnum ioMode) (castPtr cInfo)
                            >>= checkHandle
            newInfo <- peek cInfo
            return $ Handle newInfo cHandle))

-- |The 'hClose' function closes the stream, deallocates its internal buffers
-- and returns () on success or signals an 'Exception' otherwise.
hClose :: Handle -> IO ()
hClose (Handle _ handle) = do
    {#call unsafe sf_close#} handle
    checkHandle nullPtr
    return ()

-- |If the stream is opened with 'WriteMode' or 'ReadWriteMode', call the
-- operating system\'s function to force the writing of all file cache buffers
-- to disk. If the file is opened with 'ReadMode' no action is taken.
hFlush :: Handle -> IO ()
hFlush (Handle _ handle) = {#call unsafe sf_write_sync#} handle

-- ====================================================================
-- I/O helpers

-- TODO: include offset and count arguments

bufferSize b = liftM $ uncurry (-) . getBounds
clipFrame offset count frames
    | offset <= 0 = (0, min count frames)
    | offset >= frames = (frames-1, 0)
    | otherwise = (offset, min count (frames - offset))

--offset count buffersize

{-# INLINE hIOSamples #-}
hIOSamples :: (Integral n, Ix i, Integral i, Storable a') =>
                (HandlePtr -> (Ptr a) -> n -> (IO n)) ->
                Handle -> (StorableArray i a') ->
                --Count -> Count ->
                IO Count
hIOSamples cFunc (Handle _ handle) buffer = do
    size <- liftM (cIntConv . rangeSize) $ getBounds buffer
    result <- withStorableArray buffer (\ptr -> liftM fromIntegral $ cFunc handle (castPtr ptr) size)
    checkHandle handle
    touchStorableArray buffer
    return $ cIntConv result

{-# INLINE hIOFrames #-}
hIOFrames :: (Integral n, Integral i, Ix i, Storable a') =>
             (HandlePtr -> (Ptr a) -> n -> (IO n)) ->
             Handle -> (StorableArray i a') -> IO Count
hIOFrames cFunc (Handle info handle) buffer = do
    size <- getBounds buffer >>= (\bounds -> return $ cIntConv $ quot (rangeSize bounds) (fromIntegral $ channels info))
    result <- withStorableArray buffer (\ptr -> liftM fromIntegral $ cFunc handle (castPtr ptr) size)
    checkHandle handle
    touchStorableArray buffer
    return $ cIntConv result

-- ====================================================================
-- reading

hGetSamplesDouble :: (Integral i, Ix i) => Handle -> StorableArray i Double -> IO Count
hGetSamplesDouble = hIOSamples {#call unsafe sf_read_double#}

hGetSamplesFloat :: (Integral i, Ix i) => Handle -> StorableArray i Float -> IO Count
hGetSamplesFloat = hIOSamples {#call unsafe sf_read_float#}

hGetFramesDouble :: (Integral i, Ix i) => Handle -> (StorableArray i Double) -> IO Count
hGetFramesDouble = hIOFrames {#call unsafe sf_readf_double#}

hGetFramesFloat :: (Integral i, Ix i) => Handle -> (StorableArray i Float) -> IO Count
hGetFramesFloat = hIOFrames {#call unsafe sf_readf_float#}

hReadFramesDouble :: Handle -> Count -> IO (StorableArray Index Double)
hReadFramesDouble handle@(Handle info _) frames = do
    let n = frames * (fromIntegral $ channels info)
    buffer <- newArray_ (0, n-1)
    hGetFramesDouble handle buffer
    return buffer

hReadFramesFloat :: Handle -> Count -> IO (StorableArray Index Float)
hReadFramesFloat handle@(Handle info _) frames = do
    let n = frames * (fromIntegral $ channels info)
    buffer <- newArray_ (0, n-1)
    hGetFramesFloat handle buffer
    return buffer

-- ====================================================================
-- writing

hPutSamplesDouble :: (Integral i, Ix i) => Handle -> (StorableArray i Double) -> IO Count
hPutSamplesDouble = hIOFrames {#call unsafe sf_write_double#}

hPutSamplesFloat :: (Integral i, Ix i) => Handle -> (StorableArray i Float) -> IO Count
hPutSamplesFloat = hIOFrames {#call unsafe sf_write_float#}

hPutFramesDouble :: (Integral i, Ix i) => Handle -> (StorableArray i Double) -> IO Count
hPutFramesDouble = hIOFrames {#call unsafe sf_writef_double#}

hPutFramesFloat :: (Integral i, Ix i) => Handle -> (StorableArray i Float) -> IO Count
hPutFramesFloat = hIOFrames {#call unsafe sf_writef_float#}

-- ====================================================================
-- seeking

{#enum SeekMode {} deriving (Eq, Show)#}
#c
enum SeekMode
{
    AbsoluteSeek    = SEEK_SET,
    RelativeSeek    = SEEK_CUR,
    SeekFromEnd     = SEEK_END
};
#endc

-- Helper function for seeking, modifying either the read pointer, the write
-- pointer, or both.
{-# INLINE hSeek' #-}
hSeek' :: Maybe IOMode -> Handle -> SeekMode -> Count -> IO Count
hSeek' ioMode (Handle _ handle) seekMode frames = do
    n <- liftM fromIntegral $
            {#call unsafe sf_seek#}
                handle
                (cIntConv frames)
                ((cFromEnum seekMode) .|. (case ioMode of
                                                Nothing -> 0
                                                Just m -> cFromEnum m))
    checkHandle handle
    return n

-- |The file seek functions work much like 'System.IO.hseek' with the
-- exception that the non-audio data is ignored and the seek only moves within
-- the audio data section of the file. In addition, seeks are defined in
-- number of (multichannel) frames. Therefore, a seek in a stereo file from
-- the current position forward with an offset of 1 would skip forward by one
-- sample of both channels.
--
-- like lseek(), the whence parameter can be any one of the following three
-- values:
--
-- * 'AbsoluteSeek' - The offset is set to the start of the audio data plus
--   offset (multichannel) frames.
--
-- * 'RelativeSeek' - The offset is set to its current location plus offset
--   (multichannel) frames.
--
-- * 'SeekFromEnd' - The offset is set to the end of the data plus offset
--   (multichannel) frames.
--
-- Internally, libsndfile keeps track of the read and write locations using
-- separate read and write pointers. If a file has been opened with a mode of
-- 'ReadWriteMode', calling either 'hSeekRead' or 'hSeekWrite' allows the read
-- and write pointers to be modified separately. 'hSeek' modifies both the
-- read and the write pointer.
--         
-- Note that the frames offset can be negative and in fact should be when
-- SeekFromEnd is used for the whence parameter.
--         
-- 'hSeek' will return the offset in (multichannel) frames from the start of
-- the audio data, or signal an error when an attempt is made to seek beyond
-- the start or end of the file.

hSeek :: Handle -> SeekMode -> Count -> IO Count
hSeek = hSeek' Nothing

--hSeek (Handle _ handle) seekMode frames = do
--    n <- liftM fromIntegral $ {#call unsafe sf_seek#} handle (cIntConv frames) (cFromEnum seekMode)
--    checkHandle handle
--    return n

-- |Like 'hSeek', but only the read pointer is modified.
hSeekRead :: Handle -> SeekMode -> Count -> IO Count
hSeekRead = hSeek' (Just ReadMode)

-- |Like 'hSeek', but only the write pointer is modified.
hSeekWrite :: Handle -> SeekMode -> Count -> IO Count
hSeekWrite = hSeek' (Just WriteMode)

-- ====================================================================
-- string access

-- |Header string field types.
{#enum StringType {underscoreToCase} deriving (Eq, Show)#}
#c
enum StringType
{
    STR_TITLE       = SF_STR_TITLE,
    STR_COPYRIGHT   = SF_STR_COPYRIGHT,
    STR_SOFTWARE    = SF_STR_SOFTWARE,
    STR_ARTIST      = SF_STR_ARTIST,
    STR_COMMENT     = SF_STR_COMMENT,
    STR_DATE        = SF_STR_DATE
};
#endc

-- |The 'getString' function returns the specificed string from the stream
-- header in the 'Maybe' monad if it exists and 'Nothing' otherwise.
getString :: Handle -> StringType -> IO (Maybe String)
getString (Handle _ handle) t = do
    ptr <- {#call unsafe sf_get_string#} handle (cFromEnum t)
    if ptr == (nullPtr :: Ptr CChar)
        then return Nothing
        else liftM Just $ peekCString =<< (return ptr)


-- |The 'setString' function sets the string data associated with the
-- respective 'StringType'.
setString :: Handle -> StringType -> String -> IO ()
setString (Handle _ handle) t s =
    withCString s (\cs -> do
        {#call unsafe sf_set_string#} handle (cFromEnum t) cs
        checkHandle handle
        return ())

-- EOF
