module NewSong where

import Defaults
import Composition

bassVolume :: Float
bassVolume = 0.3

leadVolume :: Float
leadVolume = 0.5

bassNote :: Semitones -> Beats -> [Pulse] -- custom note w/ envelope
bassNote n beats = map (* bassVolume) $ envSqrNote n beats (Env 0.1 0.1 1.0 0.3)

leadNote :: Semitones -> Beats -> [Pulse]
leadNote n beats = map (* leadVolume) $ envSawNote n beats (Env 0.1 0.1 1.0 0.3)

cNote :: Bool -> Semitones -> Beats -> [Pulse]
cNote isBass n beats = if isBass then bassNote n beats else leadNote n beats

sixteenth :: Bool -> Semitones -> [Pulse]
sixteenth isBass n = cNote isBass n 0.25

eighth :: Bool -> Semitones -> [Pulse]
eighth isBass n = cNote isBass n 0.5

qrtr :: Bool -> Semitones -> [Pulse]
qrtr isBass n = cNote isBass n 1.0

half :: Bool -> Semitones -> [Pulse]
half isBass n = cNote isBass n 2.0

whole :: Bool -> Semitones -> [Pulse]
whole isBass n = cNote isBass n 4.0

triplet :: Bool -> Semitones -> Beats -> [Pulse]
triplet isBass n beats = repl 3 $ cNote isBass n (beats / 3)

bassline1 :: [Pulse]
bassline1 = 
    (repl 2 $ concat
    [
          qrtr True (-24)
        , cNote True (-17) 0.75
        , sixteenth True (-24)
        , rest 0.5
        , eighth True (-17)
        , eighth True (-14)
        , eighth True (-17)
    ]) ++ 
    (repl 2 $ concat
    [
          qrtr True (-26)
        , cNote True (-17) 0.75
        , sixteenth True (-26)
        , rest 0.5
        , eighth True (-21)
        , eighth True (-19)
        , eighth True (-17)
    ])

bassline2 :: [Pulse]
bassline2 =
    (repl 2 $ concat
    [
          qrtr True (-28)
        , cNote True (-17) 0.75
        , sixteenth True (-28)
        , rest 0.5
        , eighth True (-21)
        , eighth True (-19)
        , eighth True (-17)
    ]) ++ 
    (repl 2 $ concat
    [
          qrtr True (-24)
        , cNote True (-17) 0.75
        , sixteenth True (-24)
        , rest 0.5
        , eighth True (-17)
        , eighth True (-14)
        , eighth True (-17)
    ])

bassline :: [Pulse]
bassline = bassline1 ++ bassline2

melody :: [Pulse]
melody = 
    (repl 2 $ concat [
          eighth False (-2)
        , qrtr False (-5)
        , half False (-9)
        , rest 0.5
        , eighth False (-2)
        , qrtr False (-5)
        , qrtr False (-9)
        , eighth False (-7)
        , qrtr False (-5)
    ]) ++
    (concat [
          eighth False (-4)
        , qrtr False (-5)
        , half False (-9)
        , rest 0.5
        , eighth False (-4)
        , qrtr False (-5)
        , qrtr False (-9)
        , eighth False (-7)
        , qrtr False (-5)  
    ]) ++
    (concat [
          eighth False (-2)
        , qrtr False (-5)
        , half False (-9)
        , rest 0.5
        , eighth False (-2)
        , qrtr False (-5)
        , qrtr False (-9)
        , eighth False (-7)
        , qrtr False (-5)
    ])

