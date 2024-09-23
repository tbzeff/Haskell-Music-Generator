module Freq
    (
        freq, sinFreq, sqrFreq, sawFreq,
        sinFreqWEnv, sqrFreqWEnv, sawFreqWEnv

    ) where

import Defaults
import Envelopes
import Utility (floatMod)

sinOutput :: Hz -> Seconds -> [Pulse]
sinOutput hz duration = 
    let step = (hz * 2 * pi) / sampleRate
    in map (sin . (* step)) [0.0 .. sampleRate * duration]

sqrOutput :: Hz -> Seconds -> [Pulse]
sqrOutput hz duration = map signum $ sinOutput hz duration

sawOutput :: Hz -> Seconds -> [Pulse]
sawOutput hz duration = 
    let totalSamples = round (sampleRate * duration)
        step = 2 / sampleRate * hz
        sawWave t = (floatMod (t * step) 2) - 1
    in map sawWave [0.0 .. fromIntegral totalSamples - 1]

freq :: Hz -> Seconds -> [Pulse]
freq hz duration = sinFreq hz duration
     
sinFreq :: Hz -> Seconds -> [Pulse]
sinFreq hz duration = sinOutput hz duration

sinFreqWEnv :: Hz -> Seconds -> Envelope -> [Pulse]
sinFreqWEnv hz duration env = applyEnvelope env (sinOutput hz duration)
    
sqrFreq :: Hz -> Seconds -> [Pulse]
sqrFreq hz duration = sqrOutput hz duration

sqrFreqWEnv :: Hz -> Seconds -> Envelope -> [Pulse]
sqrFreqWEnv hz duration env = applyEnvelope env (sqrOutput hz duration)

sawFreq :: Hz -> Seconds -> [Pulse]
sawFreq hz duration = sawOutput hz duration

sawFreqWEnv :: Hz -> Seconds -> Envelope -> [Pulse]
sawFreqWEnv hz duration env = applyEnvelope env (sawOutput hz duration)


