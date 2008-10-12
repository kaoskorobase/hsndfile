{-# OPTIONS_GHC -fglasgow-exts #-}

module Sound.File.Sndfile.Exception (
    Exception(..),
    catch, throw
) where

import Control.Exception (catchDyn, throwDyn)
import Data.Typeable (Typeable)
import Prelude hiding (catch)

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

-- | Construct 'Exception' from error code and string.
fromErrorCode :: Int -> String -> Exception
fromErrorCode 1 = UnrecognisedFormat
fromErrorCode 2 = SystemError
fromErrorCode 3 = MalformedFile
fromErrorCode 4 = UnsupportedEncoding
fromErrorCode _ = Exception

-- |Catch values of type 'Exception'.
catch :: IO a -> (Exception -> IO a) -> IO a
catch = catchDyn

-- | Throw 'Exception' according to error code and string
throw :: Int -> String -> a
throw code str = throwDyn (fromErrorCode code str)