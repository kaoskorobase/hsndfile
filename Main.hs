{-# LANGUAGE FlexibleContexts #-}
import Sound.File.Sndfile as SF
import System.Environment
import Data.Array.Storable
import Data.List (foldl')
import Control.Monad
import Control.Monad.ST (ST(..), stToIO)
import Prelude hiding (catch)
import Foreign.Storable (Storable)

--printSamples :: Handle -> IO ()
--printSamples handle = do
--    arr <- newArray_ (0, 511) :: IO (StorableArray Index Double)
--    n <- hGetSamples handle arr 512
--    if n <= 0
--        then return ()
--        else do
--            putStrLn ("yeah")
--            printSamples handle

main :: IO ()
main = do
    [inFile, outFile] <- getArgs
    hIn <- SF.openFile inFile SF.ReadMode SF.defaultInfo
    let outFormat = (SF.hInfo hIn) { SF.format = SF.Format SF.HeaderFormatAiff SF.SampleFormatFloat SF.EndianFile }
    when (not $ SF.checkFormat outFormat) $ error "invalid format"
    hOut <- SF.openFile outFile SF.WriteMode outFormat
    let info = SF.hInfo hIn
    putStrLn $ "format: "      ++ (show $ SF.format info)
    putStrLn $ "sample rate: " ++ (show $ SF.samplerate info)
    putStrLn $ "channels: "    ++ (show $ SF.channels info)
    putStrLn $ "frames: "      ++ (show $ SF.frames info)
--  contents <- hReadFramesFloat handle (frames info) -- >>= getAssocs
--   putStrLn $ "peak: " ++ (show (foldl' max 0 (map (abs . snd) contents)))
--  putStrLn $ "peak: " ++ (show peak)
    buffer <- newArray_ (0, 511) :: IO (StorableArray SF.Index Double)
    SF.catch (SF.interact
                ((\x -> tanh(x*x)*10) :: Double -> Double)
                buffer
                hIn hOut)
            (\e -> putStrLn (show e))
    mapM_
        (uncurry $ SF.setString hOut)
        [(SF.StrArtist, "schtief"), (SF.StrTitle, "werk"), (SF.StrComment, "supercool")]
    mapM_ SF.hClose [hIn, hOut]

-- EOF
