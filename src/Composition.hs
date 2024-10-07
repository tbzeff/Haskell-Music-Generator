module Composition (
    createSignal, repl, addLayer, addLayers, triplet,
    thirty2ndNote, sixteenthNote, eighthNote, qrtrNote, halfNote, wholeNote,
    thirty2ndRest, sixteenthRest, eighthRest, qrtrRest, halfRest, wholeRest
) where

import Defaults
import Freq

createSignal :: SignalConfig -> [Note] -> [Signal]
createSignal config notes = concat $ map noteToSignal notes
    where f n = (pitchStandard config) * (2 ** (1.0 / 12.0)) ** n
          beatDuration = 60.0 / (bpm config)
          
          noteToSignal (Rest beats) = replicate (truncate (sampleRate * beats * beatDuration)) 0.0
          noteToSignal (Note n beats) = 
            let signalGenPair = getSignalGeneratorPair (freq config)
            in generateSignal signalGenPair (f n) (beats * beatDuration) (envelope config)
          noteToSignal (Chord notes) = addLayers $ map noteToSignal notes
          
          getSignalGeneratorPair Sin = (sinFreq, sinFreqWEnv)
          getSignalGeneratorPair Sqr = (sqrFreq, sqrFreqWEnv)
          getSignalGeneratorPair Saw = (sawFreq, sawFreqWEnv)
        
          generateSignal (waveGen, waveGenWEnv) frq duration maybeEnv =
            case maybeEnv of
                Just env -> map (* (volume config)) $ waveGenWEnv frq duration env
                Nothing -> map (* (volume config)) $ waveGen frq duration

thirty2ndNote :: Semitones -> Note
thirty2ndNote n = Note n 0.125

sixteenthNote :: Semitones -> Note 
sixteenthNote n = Note n 0.25

eighthNote :: Semitones -> Note
eighthNote n = Note n 0.5

qrtrNote :: Semitones -> Note
qrtrNote n = Note n 1.0

halfNote :: Semitones -> Note
halfNote n = Note n 2.0

wholeNote :: Semitones -> Note
wholeNote n = Note n 4.0

triplet :: Semitones -> Beats -> [Note]
triplet n beats = repl 3 $ [Note n (beats / 3)]

thirty2ndRest :: Note
thirty2ndRest = Rest 0.125

sixteenthRest :: Note
sixteenthRest = Rest 0.25

eighthRest :: Note
eighthRest = Rest 0.5

qrtrRest :: Note
qrtrRest = Rest 1.0

halfRest :: Note
halfRest = Rest 2.0

wholeRest :: Note
wholeRest = Rest 4.0

repl :: Integer -> [Note] -> [Note]
repl 0 _ = []
repl n arr = arr ++ (repl (n - 1) arr)

addLayer :: [Signal] -> [Signal] -> [Signal]
addLayer [] [] = []
addLayer [] (y:ys) = (y :: Signal) : addLayer [] ys
addLayer (x:xs) [] = (x :: Signal) : addLayer xs []
addLayer (x:xs) (y:ys) = ((x + y) :: Signal) : addLayer xs ys

addLayers :: Foldable t => t [Signal] -> [Signal]
addLayers layers = foldl addLayer [] layers