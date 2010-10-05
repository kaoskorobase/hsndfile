module Main where

import qualified Sound.File.Sndfile.Buffer.StorableVector.Examples as StorableVector
import qualified Sound.File.Sndfile.Buffer.Vector.Examples as Vector
import           System.Environment

main = do
    (inFile:outFile:args) <- getArgs
    let f = case args of
                ["StorableVector"] -> StorableVector.normalizeSoundFile
                ["Vector"]         -> Vector.normalizeSoundFile
                []                 -> Vector.normalizeSoundFile
                (x:_)              -> fail $ "Invalid buffer interface " ++ x
    f inFile outFile
