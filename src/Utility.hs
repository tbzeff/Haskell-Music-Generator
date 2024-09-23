module Utility 
    (
        floatMod,
        playTime, numberOfBars,
        saveWave, saveW, playW
    ) where

-- https://www.youtube.com/watch?v=FYTZkE5BZ-0
-- https://github.com/tsoding/haskell-music/blob/master/Main.hs


import Codec.Audio.Wave
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import System.IO (Handle)
import Data.Foldable
import System.Process
import Text.Printf
import Data.Int (Int16)
import Defaults

floatMod :: Float -> Float -> Float
floatMod x y = x - y * (fromIntegral (floor (x / y) :: Int))

playTime :: [Pulse] -> Seconds
playTime pulse = fromIntegral (length pulse) / sampleRate

numberOfBars :: [Pulse] -> Float
numberOfBars pulse = totalDurationInSeconds / durationOfOneBarInSeconds
  where
    totalDurationInSeconds = fromIntegral (length pulse) / sampleRate
    durationOfOneBarInSeconds = beatDuration * fromIntegral timeSignatureBeats

toSample :: Float -> Int16
toSample = round . (* scaleFactor)
  where
    scaleFactor = fromIntegral (maxBound :: Int16)

myWave :: Wave
myWave = Wave
      { waveFileFormat   = WaveVanilla
      , waveSampleRate   = round sampleRate
      , waveSampleFormat = SampleFormatPcmInt 16
      , waveChannelMask  = Set.fromList [SpeakerFrontCenter]
      , waveDataOffset   = 0
      , waveDataSize     = 0
      , waveSamplesTotal = 0
      , waveOtherChunks  = []
      }

writeWave :: FilePath -> BS.ByteString -> IO ()
writeWave path audioData = do
    let waveDataCallback :: Handle -> IO ()
        waveDataCallback handle = writeByteStringToHandle handle audioData
    writeWaveFile path myWave waveDataCallback

writeByteStringToHandle :: Handle -> BS.ByteString -> IO ()
writeByteStringToHandle handle bs = BS.hPut handle bs

audioToByteString :: [Pulse] -> BL.ByteString
audioToByteString = B.toLazyByteString . foldMap (B.int16LE . toSample)

saveWave :: FilePath -> [Pulse] -> IO ()
saveWave fn pulse = writeWave ("audio\\" ++ fn ++ ".wav") (BL.toStrict $ audioToByteString pulse)

saveW :: FilePath -> [Pulse] -> IO ()
saveW filePath pulse = BL.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE pulse

playW :: [Pulse] -> IO () 
playW wave = do
  saveW outputFilePath wave
  _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath -- f32le -> 32-bit floating-point numbers, little endian
  return ()