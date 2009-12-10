{-# LANGUAGE MultiParamTypeClasses
           , GeneralizedNewtypeDeriving
           , FlexibleInstances
           , ScopedTypeVariables
           , TypeSynonymInstances #-}

module Sound.File.Sndfile.Wrapped.StorableVector (
  Vector(..)
) where

import qualified Data.Iteratee.Base.StreamChunk as SC
import           Data.Iteratee.Base.LooseMap
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SV
import qualified Data.ListLike as LL
import           Data.Monoid
import           Foreign

-- |Wrap a Data.StorableVector
newtype Vector a = Vector { unWrap :: SV.Vector a }

wrap :: SV.Vector a -> Vector a
{-# INLINE wrap #-}
wrap = Vector

instance Storable a => Monoid (Vector a) where
  mempty        = wrap SV.empty
  mappend a1 a2 = wrap (unWrap a1 `SV.append` unWrap a2)

instance Storable a => LL.FoldableLL (Vector a) a where
    foldl f z  = SV.foldl f z . unWrap
    foldl' f z = SV.foldl' f z . unWrap
    foldl1 f   = SV.foldl1 f . unWrap
    foldr f z  = SV.foldr f z . unWrap
    foldr1 f   = SV.foldr1 f . unWrap

instance Storable a => LL.ListLike (Vector a) a where
    length        = SV.length . unWrap
    null          = SV.null . unWrap
    singleton     = wrap . SV.singleton
    cons a        = wrap . SV.cons a . unWrap
    head          = SV.head . unWrap
    tail          = wrap . SV.tail . unWrap
    findIndex p   = SV.findIndex p . unWrap
    splitAt i s   = let (a1, a2) = SV.splitAt i $ unWrap s
                    in (wrap a1, wrap a2)
    dropWhile p   = wrap . SV.dropWhile p . unWrap
    fromList      = wrap . SV.pack
    toList        = SV.unpack . unWrap
    rigidMap f    = wrap . SV.map f . unWrap

vmap :: (SC.StreamChunk s' el', Storable el) => (el -> el') -> Vector el -> s' el'
vmap f xs = step xs
    where
        step bs
            | SC.null bs = mempty
            | True       = f (SC.head bs) `SC.cons` step (SC.tail bs)

instance (Storable a) => SC.StreamChunk Vector a where
    cMap = vmap

instance (Storable el, Storable el') => LooseMap Vector el el' where
    looseMap f = wrap . SV.map f . unWrap

instance forall el. (Storable el) => SC.ReadableChunk Vector el where
    readFromPtr p l | rem l s /= 0 = error $ "Error reading stream: invalid number of bytes: " ++ (show l) ++ " size: " ++ show s
                    | otherwise    = wrap `fmap` (SV.create n $ \newp -> copyArray newp p n)
        where s = sizeOf (undefined :: el)
              n = l `div` s
