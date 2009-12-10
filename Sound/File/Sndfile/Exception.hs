module Sound.File.Sndfile.Exception (
    Exception(..),
    catch, try, throw
) where

import Control.Exception            (SomeException(..), catch, fromException, toException, try)
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

instance E.Exception (Exception) where
    toException   = SomeException
    fromException = const Nothing

-- | Construct 'Exception' from error code and string.
fromErrorCode :: Int -> String -> Exception
fromErrorCode 1 = UnrecognisedFormat
fromErrorCode 2 = SystemError
fromErrorCode 3 = MalformedFile
fromErrorCode 4 = UnsupportedEncoding
fromErrorCode _ = Exception

-- |Catch values of type 'Exception'.
-- catch :: IO a -> (Exception -> IO a) -> IO a
-- catch = E.catch

-- | Throw 'Exception' according to error code and string
throw :: Int -> String -> a
throw code str = E.throw (fromErrorCode code str)
