{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Sound.File.Sndfile.Buffer.Vector (
    Buffer
  , toBuffer
  , fromBuffer
) where

import qualified Data.Vector.Storable as SV
import           Foreign.Storable (Storable)
import qualified Sound.File.Sndfile.Buffer as SF

newtype Buffer a = Buffer { fromBuffer :: SV.Vector a }

toBuffer :: SV.Vector a -> Buffer a
toBuffer = Buffer

instance Storable a => SF.Buffer Buffer a where
    fromForeignPtr p i n = return $ toBuffer $ SV.unsafeFromForeignPtr p i n
    toForeignPtr         = return . SV.unsafeToForeignPtr . fromBuffer
