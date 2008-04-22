-- | "Sound.File.Sndfile" provides a Haskell interface to the ubiquitous
-- libsndfile library by Erik de Castro Lopo (visit the author's website at
-- <http://www.mega-nerd.com/libsndfile/>).

module Sound.File.Sndfile
(
    -- *Types
    Count, Index,
    -- *Stream format
    Format(..),
    HeaderFormat(..),
    SampleFormat(..),
    EndianFormat(..),
    defaultFormat,
    -- *Stream info
    Info(..), duration, defaultInfo, checkFormat,
    -- *Stream handle operations
    Handle, hInfo, hIsSeekable,
    IOMode(..), openFile, getFileInfo, hFlush, hClose,
    SeekMode(..), hSeek, hSeekRead, hSeekWrite,
    -- *I\/O functions
    MBuffer(..), interact,
    --IBuffer(..),
    -- *Exception handling
    Exception, errorString, catch,
    -- *Header string field access
    StringType(..), getString, setString
) where

import Prelude hiding (catch, interact)
import Sound.File.Sndfile.Buffer
import Sound.File.Sndfile.Buffer.Storable ()
import Sound.File.Sndfile.Buffer.IOCArray ()
import Sound.File.Sndfile.Interface

-- EOF
