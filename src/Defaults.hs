module Defaults 
(   Envelope(..), Filter(..), SignalConfig(..), Freq(..), Note(..),
    Signal, Seconds, Samples, Hz, Semitones, Beats,
    outputFilePath, sampleRate, defaultEnvelope, defaultConfig
) where

type Signal = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float

data Envelope = Env Float Float Float Float | PEnv Float Float Float Float Float Float Float
    deriving (Show, Eq)

data Filter = FIRCoeffs [Float] | IIRCoeffs [Float] [Float]
    deriving (Show, Eq)

data Freq = Sin | Sqr | Saw deriving (Show, Eq)
data Note = Note Semitones Beats | Chord [Note] | Rest Beats deriving (Show, Eq)

data SignalConfig = SignalConfig {freq :: Freq, bpm :: Beats, volume :: Float, pitchStandard :: Hz, timeSignatureBeats :: Int, envelope :: Maybe Envelope}
    deriving (Show, Eq)

outputFilePath :: FilePath
outputFilePath = "output.bin"

sampleRate :: Samples
sampleRate = 48000.0

defaultEnvelope :: Envelope
defaultEnvelope = Env 0.25 0.25 0.5 0.25

defaultConfig :: SignalConfig
defaultConfig = SignalConfig Sin 60 1.0 440.0 4 (Just defaultEnvelope)

