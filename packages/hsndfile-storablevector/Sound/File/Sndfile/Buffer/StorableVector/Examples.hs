{-# LANGUAGE ScopedTypeVariables #-}
-- | This module provides some examples for soundfile I\/O with 'Sound.File.Sndfile.Buffer.Vector.Buffer'. Click the /Source/ links to access the source code.
module Sound.File.Sndfile.Buffer.StorableVector.Examples where

import qualified Data.StorableVector as V
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.StorableVector as BV

-- | Read a sound file, normalize the contents and write it back.
--
-- The file is read into memory in its entirety, which may not be feasible for large files. No deinterleaving is needed in this case.
normalizeSoundFile :: FilePath -> FilePath -> IO ()
normalizeSoundFile inPath outPath = do
    (info, Just (x :: BV.Buffer Double)) <- SF.readFile inPath
    let n = V.maximum (V.map abs (BV.fromBuffer x))
        y = if n == 0 then x else BV.withBuffer (V.map (/n)) x
    SF.writeFile info outPath y
    return ()
