module Envelopes
    (
        applyEnvelope, applyEnvelope'
    ) where

import Defaults

makeEnvSignal :: Envelope -> Seconds -> [Signal]
makeEnvSignal env duration = case env of
    (Env a d s r)  -> makeAmpEnv (Env a d s r) duration
    (PEnv std a d s r sf ef) -> makePitchEnv std a d s r sf ef duration
    _              -> []

makeAmpEnv :: Envelope -> Seconds -> [Signal]
makeAmpEnv (Env a d s r) duration = envelope
    where
        totalSamples = round (sampleRate * duration) :: Int
        
        attackSamples = round (a * fromIntegral totalSamples) :: Int
        decaySamples = round (d * fromIntegral totalSamples) :: Int
        releaseSamples = round (r * fromIntegral totalSamples) :: Int
        sustainSamples = totalSamples - attackSamples - decaySamples - releaseSamples
        
        attack = [fromIntegral i / fromIntegral attackSamples | i <- [0..attackSamples - 1]]
        decay = [1.0 - (1.0 - s) * (fromIntegral i / fromIntegral decaySamples) | i <- [0..decaySamples - 1]]
        
        sustain = replicate sustainSamples s
        release = [s * (1.0 - fromIntegral i / fromIntegral releaseSamples) | i <- [0..releaseSamples - 1]]
        
        envelope = attack ++ decay ++ sustain ++ release

makePitchEnv :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Seconds -> [Signal]
makePitchEnv pitchStandard a d s r sf ef duration =
    let totalSamples = round (sampleRate * duration)
        attackSamples = round (a * fromIntegral totalSamples)
        decaySamples = round (d * fromIntegral totalSamples)
        sustainSamples = round (s * fromIntegral totalSamples)
        releaseSamples = totalSamples - attackSamples - decaySamples - sustainSamples
        
        -- Frequency calculation based on semitones
        frequency n = pitchStandard * (2 ** (1.0 / 12.0)) ** n
        
        -- Convert semitones to frequencies
        startFreq = frequency sf
        endFreq = frequency ef
        
        linearInterpolate f0 f1 n i = f0 + (f1 - f0) * (fromIntegral i / fromIntegral n)
        
        attack = [linearInterpolate startFreq endFreq attackSamples i | i <- [0..attackSamples - 1]]
        decay = [linearInterpolate endFreq s decaySamples i | i <- [0..decaySamples - 1]]
        sustain = replicate sustainSamples s
        release = [linearInterpolate s endFreq releaseSamples i | i <- [0..releaseSamples - 1]]
    in attack ++ decay ++ sustain ++ release

applyEnvelope :: Envelope -> [Signal] -> [Signal]
applyEnvelope env signal = case env of
    (Env a d s r) -> 
        let envSignal = makeAmpEnv (Env a d s r) ((fromIntegral $ length signal) / sampleRate)
        in zipWith (*) signal envSignal
    (PEnv std a d s r sf ef) -> 
        let envSignal = makePitchEnv std a d s r sf ef ((fromIntegral $ length signal) / sampleRate)
        in zipWith (*) signal envSignal
    _ -> []

applyEnvelope' :: Envelope -> [[Signal]] -> [[Signal]]
applyEnvelope' env = map (applyEnvelope env)
