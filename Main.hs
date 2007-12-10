import Sound.Files.Sndfile
import System.Environment
import Data.Array.Storable
import Data.List (foldl')
import Control.Monad
import Control.Monad.ST (ST(..), stToIO)

--interactSF :: (Double -> Double) -> Handle -> Handle -> IO ()
--interactSF f hIn hOut = do
--  rbuf <- (newArray_ (0, 511) :: IO (StorableArray Index Double))
--  interact_ rbuf
--  where interact_ rbuf = do
--          n <- hGetFramesDouble hIn rbuf
--          when (n > 0)
--               (do
--                 wbuf <- (mapArray f rbuf)
--                 hPutFramesDouble hOut wbuf
--                 interact_ rbuf)

printSamples :: Handle -> IO ()
printSamples handle = do
    arr <- newArray_ (0, 511) :: IO (StorableArray Index Double)
    n <- hGetSamples handle arr 512
    if n <= 0
        then return ()
        else do
            putStrLn ("yeah")
            printSamples handle

main :: IO ()
main = do
  [inFile] <- getArgs
  hIn <- openFile inFile ReadMode defaultInfo
  --let outFormat = (hInfo hIn) { format = Format HeaderFormatAiff SampleFormatFloat EndianFile }
  --when (not $ checkFormat outFormat) $ error "invalid format"
  --hOut <- openFile outFile WriteMode outFormat
  let info = hInfo hIn
  putStrLn $ "format: "      ++ (show $ format info)
  putStrLn $ "sample rate: " ++ (show $ samplerate info)
  putStrLn $ "channels: "    ++ (show $ channels info)
  putStrLn $ "frames: "      ++ (show $ frames info)
--  contents <- hReadFramesFloat handle (frames info) -- >>= getAssocs
--   putStrLn $ "peak: " ++ (show (foldl' max 0 (map (abs . snd) contents)))
--  putStrLn $ "peak: " ++ (show peak)
  --interactSF (\x -> tanh(x*x)*10)  hIn hOut
  --mapM_ (uncurry $ setString hOut) [(StrArtist, "schtief"), (StrTitle, "werk"), (StrComment, "supercool")]
  --mapM_ hClose [hIn, hOut]
  printSamples hIn
  
-- EOF
