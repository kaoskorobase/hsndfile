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
    SeekMode(..), hSeek, hSeekRead, hSeekWrite
    -- *I\/O functions
  , Sample(..)
  , Buffer(..)
  , hGetBuffer
  , hGetContents, readFile
  , hGetContentChunks, readFileChunks
    -- interact,
    -- *Exception handling
  , Exception(..), catch
    -- *Header string field access
  , StringType(..), getString, setString
    -- *Enumerators and Iteratees
  , module Sound.File.Sndfile.Enumerator
) where

import Prelude hiding (catch, interact, readFile)
import Sound.File.Sndfile.Buffer
-- import Sound.File.Sndfile.Buffer.Storable ()
-- import Sound.File.Sndfile.Buffer.IOCArray ()
import Sound.File.Sndfile.Enumerator
import Sound.File.Sndfile.Exception
import Sound.File.Sndfile.Interface

-- EOF
