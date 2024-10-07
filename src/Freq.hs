module Freq
    (
        sinFreq, sqrFreq, sawFreq,
        sinFreqWEnv, sqrFreqWEnv, sawFreqWEnv
    ) where

import Defaults
import Envelopes

floatMod :: Float -> Float -> Float
floatMod x y = x - y * (fromIntegral (floor (x / y) :: Int))

sinOutput :: Hz -> Seconds -> [Signal]
sinOutput hz duration = 
    let step = (hz * 2 * pi) / sampleRate
    in map (sin . (* step)) [0.0 .. sampleRate * duration]

sqrOutput :: Hz -> Seconds -> [Signal]
sqrOutput hz duration = map signum $ sinOutput hz duration

sawOutput :: Hz -> Seconds -> [Signal]
sawOutput hz duration = 
    let totalSamples = round (sampleRate * duration)
        step = 2 / sampleRate * hz
        sawWave t = (floatMod (t * step) 2) - 1
    in map sawWave [0.0 .. fromIntegral totalSamples - 1]
     
sinFreq :: Hz -> Seconds -> [Signal]
sinFreq hz duration = sinOutput hz duration

sinFreqWEnv :: Hz -> Seconds -> Envelope -> [Signal]
sinFreqWEnv hz duration env = applyEnvelope env (sinOutput hz duration)
    
sqrFreq :: Hz -> Seconds -> [Signal]
sqrFreq hz duration = sqrOutput hz duration

sqrFreqWEnv :: Hz -> Seconds -> Envelope -> [Signal]
sqrFreqWEnv hz duration env = applyEnvelope env (sqrOutput hz duration)

sawFreq :: Hz -> Seconds -> [Signal]
sawFreq hz duration = sawOutput hz duration

sawFreqWEnv :: Hz -> Seconds -> Envelope -> [Signal]
sawFreqWEnv hz duration env = applyEnvelope env (sawOutput hz duration)


