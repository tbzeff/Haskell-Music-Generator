module Envelopes
    (
        applyEnvelope, applyEnvelope'
    ) where

import Defaults

makeEnvPulse :: Envelope -> Seconds -> [Pulse]
makeEnvPulse env duration = case env of
    (Env a d s r)  -> makeAmpEnv a d s r duration
    (PEnv a d s r sf ef) -> makePitchEnv a d s r sf ef duration
    _              -> []
    
makeAmpEnv :: Float -> Float -> Float -> Float -> Seconds -> [Pulse]
makeAmpEnv a d s r duration = envelope
    where
        totalSamples = round (sampleRate * duration) :: Int
        
        -- Calculate number of samples for each phase
        attackSamples = round (a * fromIntegral totalSamples) :: Int
        decaySamples = round (d * fromIntegral totalSamples) :: Int
        releaseSamples = round (r * fromIntegral totalSamples) :: Int
        sustainSamples = totalSamples - attackSamples - decaySamples - releaseSamples
        
        -- Generate linear ramps for attack and decay phases
        attack = [fromIntegral i / fromIntegral attackSamples | i <- [0..attackSamples - 1]]
        decay = [1.0 - (1.0 - s) * (fromIntegral i / fromIntegral decaySamples) | i <- [0..decaySamples - 1]]
        
        -- Generate constant level for sustain phase and linear ramp for release phase
        sustain = replicate sustainSamples s
        release = [s * (1.0 - fromIntegral i / fromIntegral releaseSamples) | i <- [0..releaseSamples - 1]]
        
        envelope = attack ++ decay ++ sustain ++ release

makePitchEnv :: Float -> Float -> Float -> Float -> Float -> Float -> Seconds -> [Pulse] 
makePitchEnv a d s r sf ef duration = 
    let totalSamples = round (sampleRate * duration)
        attackSamples = round (a * fromIntegral totalSamples)
        decaySamples = round (d * fromIntegral totalSamples)
        sustainSamples = round (s * fromIntegral totalSamples)
        releaseSamples = totalSamples - attackSamples - decaySamples - sustainSamples
        peakFreq = f (sf + 12) -- 12 semitones higher than sf
        sustainFreq = (sf + ef) / 2 -- midpoint between sf and ef
        linearInterpolate f0 f1 n i = f0 + (f1 - f0) * (fromIntegral i / fromIntegral n)
        attack = [linearInterpolate sf peakFreq attackSamples i | i <- [0..attackSamples - 1]]
        decay = [linearInterpolate peakFreq sustainFreq decaySamples i | i <- [0..decaySamples - 1]]
        sustain = replicate sustainSamples sustainFreq
        release = [linearInterpolate sustainFreq ef releaseSamples i | i <- [0..releaseSamples - 1]]
    in attack ++ decay ++ sustain ++ release

applyEnvelope :: Envelope -> [Pulse] -> [Pulse]
applyEnvelope env freq = case env of
    (Env a d s r) -> 
        let pulse = makeEnvPulse env ((fromIntegral $ length freq) / sampleRate)
        in map (* volume) $ zipWith (*) freq pulse
    (PEnv a d s r sf ef) -> undefined
    _ -> []
    
applyEnvelope' :: Envelope -> [[Pulse]] -> [[Pulse]]
applyEnvelope' env = map (applyEnvelope env)