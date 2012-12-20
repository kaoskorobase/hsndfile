{-# LANGUAGE DeriveDataTypeable #-}
module Sound.File.Sndfile.Exception (
  Exception(..)
, fromErrorCode
) where

import qualified Control.Exception  as E
import Data.Typeable                (Typeable)
import Prelude hiding               (catch)

-- |Values of type 'Exception' are thrown by the library when an error occurs.
--
-- Use 'catch' to catch only exceptions of this type.
data Exception =
    Exception           { errorString :: String }
  | UnrecognisedFormat  { errorString :: String }
  | SystemError         { errorString :: String }
  | MalformedFile       { errorString :: String }
  | UnsupportedEncoding { errorString :: String }
  deriving (Typeable, Show)

instance E.Exception (Exception)

-- | Construct 'Exception' from error code and message.
fromErrorCode :: Int -> String -> Exception
fromErrorCode 1 = UnrecognisedFormat
fromErrorCode 2 = SystemError
fromErrorCode 3 = MalformedFile
fromErrorCode 4 = UnsupportedEncoding
fromErrorCode _ = Exception
