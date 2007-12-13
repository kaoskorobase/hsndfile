{-
import Data.Array.Base (STUArray(..))
import Control.Monad.ST (ST(..), stToIO)
--import qualified Control.Monad.ST.Lazy as Lazy (ST)
import GHC.Prim (unsafeCoerce#, MutableByteArray#(..), RealWorld)
import GHC.ST (ST(..))
import GHC.IOBase       ( IO(..) )

{-# INLINE hIOSamples #-}
hIOSamples :: (HandlePtr -> MutableByteArray# s -> CLLong -> (IO CLLong)) ->
                Handle -> (STUArray s Int b) ->
                --Count -> Count ->
                Int ->
                ST s Count
--hIOSamples cFunc (Handle _ handle) buffer = do
--    size <- liftM (cIntConv . rangeSize) $ getBounds buffer
--    result <- withStorableArray buffer (\ptr -> liftM fromIntegral $ cFunc handle (castPtr ptr) size)
--    checkHandle handle
--    touchStorableArray buffer
--    return $ cIntConv result
--STUArray !i !i !Int (MutableByteArray# s)
--hIOSamples cFunc (Handle _ handle) buffer@(IOUArray (STUArray _ _ _ dst#)) = do
--    size <- liftM (cIntConv . rangeSize) $ getBounds buffer
--    ST $ \s1# ->
--        case unsafeCoerce# cFunc handle dst size s1# of { (# s2#, () #) ->
--        (# s2#, () #) }}
    --result <- withStorableArray buffer (\ptr -> liftM fromIntegral $ cFunc handle (castPtr ptr) size)
    --checkHandle handle
    --touchStorableArray buffer
    --return $ cIntConv result
hIOSamples cFunc (Handle _ handle) (STUArray l u n ptr) count
  | count == 0
  = return 0
  | count < 0 || count > n
  = return 0 --illegalBufferSize handle "hGetArray" count
  | otherwise = do
      ST $ \s1# ->
        case cFunc handle ptr (fromIntegral n) of { IO n1# ->
        case unsafeCoerce# n1# s1# of { (# s2#, n1# #) ->
        (# s2#, n1# #) }}
    --            checkHandle handle
    --            return (fromIntegral n1)
    --if bufferEmpty buf
    --   then readChunk fd is_stream ptr 0 count
    --   else do 
    --  let avail = w - r
    --  copied <- if (count >= avail)
    --              then do 
    --          memcpy_ba_baoff ptr raw (fromIntegral r) (fromIntegral avail)
    --          writeIORef ref buf{ bufWPtr=0, bufRPtr=0 }
    --          return avail
    --              else do 
    --          memcpy_ba_baoff ptr raw (fromIntegral r) (fromIntegral count)
    --          writeIORef ref buf{ bufRPtr = r + count }
    --          return count
    --
    --  let remaining = count - copied
    --  if remaining > 0 
    --     then do rest <- readChunk fd is_stream ptr copied remaining
    --         return (rest + copied)
    --     else return count

--{-# INLINE hIOFrames #-}
--hIOFrames :: (Integral n, Integral i, Ix i, Storable a') =>
--             (HandlePtr -> (Ptr a) -> n -> (IO n)) ->
--             Handle -> (StorableArray i a') -> IO Count
--hIOFrames cFunc (Handle info handle) buffer = do
--    size <- getBounds buffer >>= (\bounds -> return $ cIntConv $ quot (rangeSize bounds) (fromIntegral $ channels info))
--    result <- withStorableArray buffer (\ptr -> liftM fromIntegral $ cFunc handle (castPtr ptr) size)
--    checkHandle handle
--    touchStorableArray buffer
--    return $ cIntConv result

-- ====================================================================
-- reading

foreign import ccall unsafe "sf_read_double"
    sf_read_double :: HandlePtr -> MutableByteArray# s -> CLLong -> IO CLLong

hGetSamplesDouble :: Handle -> STUArray s Int Double -> Int -> ST s Count
hGetSamplesDouble = hIOSamples sf_read_double
-}