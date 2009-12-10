{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}

module Sound.File.Sndfile.Wrapped.StorableVector (
  Vector(..)
) where

import qualified Data.Iteratee.Base.StreamChunk as SC
import qualified Data.StorableVector as SV
import qualified Data.ListLike as LL
import           Data.Monoid
import           Foreign

-- |Wrap a Data.ByteString ByteString
newtype Vector a = Vector { unWrap :: SV.Vector a }

instance Storable a => Monoid (Vector a) where
  mempty        = Vector SV.empty
  mappend a1 a2 = Vector (SV.append (unWrap a1) (unWrap a2))

instance Storable a => LL.FoldableLL (Vector a) a where
  foldl f z = SV.foldl f z . unWrap
  foldr f z = SV.foldr f z . unWrap

-- Thanks to Echo Nolan for indicating that the bytestring must copy
-- data to a new ptr to preserve referential transparency.
-- instance SC.ReadableChunk WrappedByteString Word8 where
--   readFromPtr buf l = let csl = (castPtr buf, l) in
--                       liftM WrapBS $ SV.packCStringLen csl
-- 
-- instance SC.ReadableChunk WrappedByteString Char where
--   readFromPtr buf l = let csl = (castPtr buf, l) in
--                       liftM WrapBS $ BC.packCStringLen csl
-- 

instance Storable a => LL.ListLike (Vector a) a where
    length        = SV.length . unWrap
    null          = SV.null . unWrap
    singleton     = Vector . SV.singleton
    cons a        = Vector . SV.cons a . unWrap
    head          = SV.head . unWrap
    tail          = Vector . SV.tail . unWrap
    findIndex p   = SV.findIndex p . unWrap
    splitAt i s   = let (a1, a2) = SV.splitAt i $ unWrap s
                  in (Vector a1, Vector a2)
    dropWhile p   = Vector . SV.dropWhile p . unWrap
    fromList      = Vector . SV.pack
    toList        = SV.unpack . unWrap
    rigidMap f    = Vector . SV.map f . unWrap

-- instance SC.StreamChunk WrappedByteString Word8 where
--   cMap          = bwmap
-- 
-- bwmap :: (SC.StreamChunk s' el') =>
--   (Word8 -> el')
--   -> WrappedByteString Word8
--   -> s' el'
-- bwmap f xs = step xs
--   where
--   step bs
--     | LL.null bs = mempty
--     | True     = f (LL.head bs) `LL.cons` step (LL.tail bs)
-- 
-- -- Now the Char instance
-- 
-- instance Monoid (WrappedByteString Char) where
--     mempty = WrapBS SV.empty
--     mappend a1 a2 = WrapBS (SV.append (unWrap a1) (unWrap a2))
-- 
-- instance LL.FoldableLL (WrappedByteString Char) Char where
--   foldl f z = BC.foldl f z . unWrap
--   foldr f z = BC.foldr f z . unWrap
-- 
-- instance LL.ListLike (WrappedByteString Char) Char where
--   length        = BC.length . unWrap
--   null          = BC.null . unWrap
--   singleton     = WrapBS . BC.singleton
--   cons a        = WrapBS . BC.cons a . unWrap
--   head          = BC.head . unWrap
--   tail          = WrapBS . BC.tail . unWrap
--   findIndex p   = BC.findIndex p . unWrap
--   splitAt i s   = let (a1, a2) = BC.splitAt i $ unWrap s
--                   in (WrapBS a1, WrapBS a2)
--   dropWhile p   = WrapBS . BC.dropWhile p . unWrap
--   fromList      = WrapBS . BC.pack
--   toList        = BC.unpack . unWrap
--   rigidMap f    = WrapBS . BC.map f . unWrap
-- 
-- instance SC.StreamChunk WrappedByteString Char where
--   cMap          = bcmap
-- 
-- bcmap :: (SC.StreamChunk s' el') =>
--   (Char -> el')
--    -> WrappedByteString Char
--    -> s' el'
-- bcmap f xs = step xs
--   where
--   step bs
--     | LL.null bs = mempty
--     | True     = f (LL.head bs) `LL.cons` step (LL.tail bs)
