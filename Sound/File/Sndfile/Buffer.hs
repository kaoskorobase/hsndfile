module Sound.File.Sndfile.Buffer (
    module Sound.File.Sndfile.Buffer.Sample
) where

import Sound.File.Sndfile.Buffer.Sample

-- |Return an array with the requested number of items. The 'count' parameter
-- must be an integer product of the number of channels or an error will
-- occur.
-- hReadSamples :: forall ia ma . (Sample ia ma e m, Num e, Monad m) => Handle -> Count -> m (Maybe (IBuffer e))
-- hReadSamples h n = do
--     b  <- newBuffer (0, n-1)
--     n' <- hGetSamples h b n
--     if n' == 0
--         then return Nothing
--         else do
--             when (n' < n) (unsafeWriteRange 0 b (n', n-1))
--             unsafeFreeze b >>= return . Just

-- |Return an array with the requested number of frames of data.
-- The resulting array size is equal to the product of the number of frames
-- `n' and the number of channels in the soundfile.
-- hReadFrames :: (Sample ia ma e m, Num e, Monad m) => Handle -> Count -> m (Maybe (IBuffer e))
-- hReadFrames h n = do
--     b  <- newBuffer (0, si)
--     n' <- hGetFrames h b n
--     if n' == 0
--         then return Nothing
--         else do
--             when (n' < n) (unsafeWriteRange 0 b (f2s n', si))
--             unsafeFreeze b >>= return . Just
--     where
--         f2s = (* channels (hInfo h))
--         si  = (f2s n) - 1

-- foldlChunks' :: (a -> b -> a) -> a -> Stream b -> a
-- foldlChunks' f z = go z
--   where go a _ | a `seq` False = undefined
--  go a Empty = a
--  go a (Stream c cs) = let a' = f a c in a' `seq` go a' cs
-- {-# INLINE foldlChunks' #-}

-- hGetContentFrames :: Sample e IO => Count -> Handle -> IO [IBuffer e]
-- hGetContentFrames n h = lazyread
--  where
--      lazyread = unsafeInterleaveIO loop
--      loop = do
--          d <- hReadFrames h n
--          case d of
--              Just arr -> do
--                  ds <- lazyread
--                  return (arr:ds)
--              Nothing -> return []

-- interact :: (MArray a e m, Sample e m) => (e -> e) -> MBuffer e -> Handle -> Handle -> m ()
-- interact f buffer hIn hOut = do
--     s <- liftM rangeSize $ getBounds buffer
--     n <- hGetSamples hIn buffer s
--     when (n > 0) $ do
--         modifyArray f buffer 0 n
--         hPutSamples hOut buffer n
--         interact f buffer hIn hOut
