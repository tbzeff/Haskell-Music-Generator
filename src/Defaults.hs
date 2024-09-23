module Defaults 
(   Envelope(..), Filter(..), Pulse, Seconds, Samples, Hz, Semitones, Beats,
    outputFilePath, volume, sampleRate, pitchStandard, 
    bpm, beatDuration, timeSignatureBeats, f, defaultEnvelope, cEnv
) where

type Pulse = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float

data Envelope = Env Float Float Float Float | PEnv Float Float Float Float Float Float
    deriving (Show, Eq)

data Filter = FIRCoeffs [Float] | IIRCoeffs [Float] [Float]
    deriving (Show, Eq)

outputFilePath :: FilePath
outputFilePath = "output.bin"

volume :: Float
volume = 1.0

sampleRate :: Samples
sampleRate = 48000.0

pitchStandard :: Hz
pitchStandard = 293.66 -- D, Octave 4

bpm :: Beats
bpm = 100.0

beatDuration :: Seconds -- seconds per beat
beatDuration = 60.0 / bpm

-- 4/4 time signature
timeSignatureBeats :: Int
timeSignatureBeats = 4

-- NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
f :: Semitones -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

-- Default envelope
defaultEnvelope :: Envelope
defaultEnvelope = Env 0.25 0.25 0.5 0.25

-- Custom envelope
cEnv :: Envelope
cEnv = Env 0.1 0.2 0.5 0.2

