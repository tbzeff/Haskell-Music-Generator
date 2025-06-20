-- ChatGPT Generated this piece
module AmbientMusic (song) where

import Defaults
import Composition

padEnv :: Envelope
padEnv = Env 0.4 0.2 0.6 0.4

padConfig :: SignalConfig
padConfig = SignalConfig Sin 60 0.4 392.0 4 (Just padEnv)

chordProg1 :: [Note]
chordProg1 =
    [ Chord [wholeNote (-5), wholeNote (0), wholeNote (7)]
    , Rest 1.0
    , Chord [wholeNote (-3), wholeNote (2), wholeNote (9)]
    , Rest 1.0
    , Chord [wholeNote (-7), wholeNote (-2), wholeNote (5)]
    , Rest 1.0
    , Chord [wholeNote (-4), wholeNote (0), wholeNote (4)]
    , Rest 2.0
    ]

chordProg2 :: [Note]
chordProg2 =
    [ Chord [wholeNote (-12), wholeNote (-5), wholeNote (0)]
    , Rest 1.0
    , Chord [wholeNote (-10), wholeNote (-3), wholeNote (2)]
    , Rest 1.0
    , Chord [wholeNote (-8), wholeNote (-2), wholeNote (5)]
    , Rest 1.0
    , Chord [wholeNote (-7), wholeNote (-3), wholeNote (3)]
    , Rest 2.0
    ]

padTrack :: [Signal]
padTrack = createSignal padConfig $ chordProg1 ++ chordProg2 ++ chordProg1

melodyEnv :: Envelope
melodyEnv = Env 0.3 0.2 0.6 0.5

melodyConfig :: SignalConfig
melodyConfig = SignalConfig Sin 60 0.2 392.0 4 (Just melodyEnv)

melodyLine :: [Note]
melodyLine =
    [
        halfNote 12,
        Rest 0.5,
        halfNote 14,
        Rest 1.0,
        qrtrNote 17,
        qrtrNote 14,
        halfNote 12,
        Rest 2.0,

        qrtrNote 10,
        Rest 0.5,
        qrtrNote 9,
        halfNote 7,
        Rest 1.0
    ]

melody :: [Signal]
melody = createSignal melodyConfig (repl 4 melodyLine)

song :: [Signal]
song = addLayers [padTrack, melody]

