{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- | This module provides a 'Sound.File.Sndfile.Buffer' instance for 'Data.Vector.Storable.Vector', wrapped in a newtype. See "Sound.File.Sndfile.Buffer.Vector.Examples" for some example code.
module Sound.File.Sndfile.Buffer.Vector (
    Buffer
  , toBuffer
  , fromBuffer
  , withBuffer
) where

import qualified Data.Vector.Storable as SV
import           Foreign.Storable (Storable)
import qualified Sound.File.Sndfile.Buffer as SF

-- | Newtype wrapper for 'Data.Vector.Storable.Vector'.
newtype Buffer a = Buffer {
    -- | Extract the 'Data.Vector.Storable.Vector' from a 'Buffer'.
    fromBuffer :: SV.Vector a
    }

-- | Construct a 'Buffer' from a 'Data.Vector.Storable.Vector'.
toBuffer :: SV.Vector a -> Buffer a
toBuffer = Buffer

withBuffer :: (SV.Vector a -> SV.Vector b) -> Buffer a -> Buffer b
withBuffer f = toBuffer . f . fromBuffer

instance Storable a => SF.Buffer Buffer a where
    fromForeignPtr p i n = return $ toBuffer $ SV.unsafeFromForeignPtr p i n
    toForeignPtr         = return . SV.unsafeToForeignPtr . fromBuffer
