module Composition 
    (
        note, envNote, sinNote, envSinNote,
        sqrNote, envSqrNote, sawNote,
        envSawNote, rest, repl, 
        addLayer, addLayers, interval
    ) where

import Defaults
import Freq

note :: Semitones -> Beats -> [Pulse]
note n beats = freq (f n) (beats * beatDuration)

envNote :: Semitones -> Beats -> Maybe Envelope -> [Pulse]
envNote n beats menv =
    case menv of
        Just env -> envSqrNote n beats env -- custom env
        Nothing -> envSqrNote n beats defaultEnvelope -- default

sinNote :: Semitones -> Beats -> [Pulse]
sinNote n beats = sinFreq (f n) (beats * beatDuration)

envSinNote :: Semitones -> Beats -> Envelope -> [Pulse]
envSinNote n beats env = sinFreqWEnv (f n) (beats * beatDuration) env

sqrNote :: Semitones -> Beats -> [Pulse]
sqrNote n beats = sqrFreq (f n) (beats * beatDuration)

envSqrNote :: Semitones -> Beats -> Envelope -> [Pulse]
envSqrNote n beats env = sqrFreqWEnv (f n) (beats * beatDuration) env

sawNote :: Semitones -> Beats -> [Pulse]
sawNote n beats = sawFreq (f n) (beats * beatDuration)

envSawNote :: Semitones -> Beats -> Envelope -> [Pulse]
envSawNote n beats env = sawFreqWEnv (f n) (beats * beatDuration) env

rest :: Beats ->  [Pulse]
rest beats = repl (truncate (sampleRate * beats * beatDuration)) [0.0]

-- https://elijahoyekunle.com/technology/2018/10/07/Haskell-Repeat-List-Elements.html
repl :: Integer -> [Pulse] -> [Pulse]
repl 0 _ = []
repl n arr = arr ++ (repl (n - 1) arr)

addLayer :: [Pulse] -> [Pulse] -> [Pulse]
addLayer [] [] = []
addLayer [] (y:ys) = (y :: Pulse) : addLayer [] ys
addLayer (x:xs) [] = (x :: Pulse) : addLayer xs []
addLayer (x:xs) (y:ys) = ((x + y) :: Pulse) : addLayer xs ys

addLayers :: Foldable t => t [Pulse] -> [Pulse]
addLayers layers = foldl addLayer [] layers

interval :: Bool -> Semitones -> Semitones -> Beats -> Beats -> [Pulse]
interval ascnd n semi beats1 beats2
    | not ascnd = (note n beats1) ++ (note (n - semi) beats2)
    | otherwise = (note n beats1) ++ (note (n + semi) beats2) 

--ascm2 ::Semitones -> Beats -> [Pulse]
--ascm2 semi beats = interval True semi 1 beats beats
--
--ascM2 ::Semitones -> Beats -> [Pulse]
--ascM2 semi beats = interval True semi 2 beats beats
--
--ascm3 ::Semitones -> Beats -> [Pulse]
--ascm3 semi beats = interval True semi 3 beats beats
--
--ascM3 ::Semitones -> Beats -> [Pulse]
--ascM3 semi beats = interval True semi 4 beats beats
--
--ascP4 ::Semitones -> Beats -> [Pulse]
--ascP4 semi beats = interval True semi 5 beats beats
--
--ascd5 ::Semitones -> Beats -> [Pulse]
--ascd5 semi beats = interval True semi 6 beats beats 
--
--ascP5 ::Semitones -> Beats -> [Pulse]
--ascP5 semi beats = interval True semi 7 beats beats
--
--ascm6 ::Semitones -> Beats -> [Pulse]
--ascm6 semi beats = interval True semi 8 beats beats 
--
--ascM6 ::Semitones -> Beats -> [Pulse]
--ascM6 semi beats = interval True semi 9 beats beats
--
--ascm7 ::Semitones -> Beats -> [Pulse]
--ascm7 semi beats = interval True semi 10 beats beats 
--
--ascM7 ::Semitones -> Beats -> [Pulse]
--ascM7 semi beats = interval True semi 11 beats beats  
--
--ascOct ::Semitones -> Beats -> [Pulse]
--ascOct semi beats = interval True semi 12 beats beats
--
--desm2 ::Semitones -> Beats -> [Pulse]
--desm2 semi beats = interval False semi 1 beats beats
--
--desM2 ::Semitones -> Beats -> [Pulse]
--desM2 semi beats = interval False semi 2 beats beats
--
--desm3 ::Semitones -> Beats -> [Pulse]
--desm3 semi beats = interval False semi 3 beats beats
--
--desM3 ::Semitones -> Beats -> [Pulse]
--desM3 semi beats = interval False semi 4 beats beats
--
--desP4 ::Semitones -> Beats -> [Pulse]
--desP4 semi beats = interval False semi 5 beats beats
--
--desd5 ::Semitones -> Beats -> [Pulse]
--desd5 semi beats = interval False semi 6 beats beats 
--
--desP5 ::Semitones -> Beats -> [Pulse]
--desP5 semi beats = interval False semi 7 beats beats
--
--desm6 ::Semitones -> Beats -> [Pulse]
--desm6 semi beats = interval False semi 8 beats beats 
--
--desM6 ::Semitones -> Beats -> [Pulse]
--desM6 semi beats = interval False semi 9 beats beats
--
--desm7 ::Semitones -> Beats -> [Pulse]
--desm7 semi beats = interval False semi 10 beats beats 
--
--desM7 ::Semitones -> Beats -> [Pulse]
--desM7 semi beats = interval False semi 11 beats beats  
--
--desOct ::Semitones -> Beats -> [Pulse]
--desOct semi beats = interval False semi 12 beats beats


--funAscend :: [Pulse]
--funAscend =  repl 4 $ concat [ascm2 0 0.1, ascM2 0 0.1, ascm3 0 0.1, ascM3 0 0.1, ascP4 0 0.1, ascd5 0 0.1, ascP5 0 0.1, ascm6 0 0.1, ascM6 0 0.1, ascm7 0 0.1, ascM7 0 0.1, ascOct 0 0.1]
--
--funDescend :: [Pulse]
--funDescend =  repl 4 $ concat [desm2 0 0.1, desM2 0 0.1, desm3 0 0.1, desM3 0 0.1, desP4 0 0.1, desd5 0 0.1, desP5 0 0.1, desm6 0 0.1, desM6 0 0.1, desm7 0 0.1, desM7 0 0.1, desOct 0 0.1]
--
--wackiness :: [Pulse]
--wackiness = repl 2 $ asc ++ des
--    where
--        asc = concat [ascm2 0 0.1, ascM2 0 0.1, ascm3 0 0.1, ascM3 0 0.1, ascP4 0 0.1, ascd5 0 0.1, ascP5 0 0.1, ascm6 0 0.1, ascM6 0 0.1, ascm7 0 0.1, ascM7 0 0.1, ascOct 0 0.1]
--        des = concat [desm2 0 0.1, desM2 0 0.1, desm3 0 0.1, desM3 0 0.1, desP4 0 0.1, desd5 0 0.1, desP5 0 0.1, desm6 0 0.1, desM6 0 0.1, desm7 0 0.1, desM7 0 0.1, desOct 0 0.1]
--
--testDes :: [Pulse]
--testDes = concat [desm2 0 0.1, desM2 1 0.1, desm3 2 0.1, desM3 3 0.1, desP4 4 0.1, desd5 5 0.1, desP5 6 0.1, desm6 7 0.1, desM6 8 0.1, desm7 9 0.1, desM7 10 0.1, desOct 11 0.1]