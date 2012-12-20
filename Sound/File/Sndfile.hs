-- | "Sound.File.Sndfile" provides a Haskell interface to the libsndfile
--   library by Erik de Castro Lopo (<http://www.mega-nerd.com/libsndfile/>).
--
--   The API is modeled after the original /C/ API, but type and function
--   identifiers follow Haskell naming conventions.
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
  , hPutBuffer, writeFile
    -- *Exception handling
  , Exception(..), catch
    -- *Header string field access
  , StringType(..), getString, setString

  , module Prelude
) where

import Prelude hiding (catch, readFile, writeFile)
import Sound.File.Sndfile.Buffer
import Sound.File.Sndfile.Exception
import Sound.File.Sndfile.Interface
