module Filters 
    (
        applyFilter, generateFIRCoeffs, generateIIRCoeffs
    ) where

import Defaults

applyFilter :: Filter -> [Signal] -> [Signal]
applyFilter (FIRCoeffs c) signal = map (\i -> sum $ zipWith (*) c (take (length c) $ drop i signal)) [0..(length signal - length c)] -- Finite ImSignal Response (FIR) Filter Implementation
applyFilter (IIRCoeffs a b) signal = iirFilter' signal [0,0..] -- Infinite Implulse Response (IIR) Filter Implementation
  where
    iirFilter' [] _ = []
    iirFilter' (x:xs) ys = let y = (sum $ zipWith (*) b (x : take (length b - 1) ys)) - (sum $ zipWith (*) (tail a) (take (length a - 1) ys))
                           in y : iirFilter' xs (y : ys)

-- Generate sinc function for low-pass filter
sincFunction :: Float -> Int -> [Float]
sincFunction cutoff numPoints = 
    [if x == 0 then 1 else (sin (pi * x)) / (pi * x) | x <- xValues]
    where
        half = fromIntegral numPoints / 2
        negHalf = -half
        xValues = map (\n -> n / half * cutoff) [negHalf..half]

-- Hamming window function
hammingWindow :: Int -> [Float]
hammingWindow numPoints = 
    [0.54 - 0.46 * cos (2 * pi * n / (fromIntegral numPoints - 1)) | n <- [0..(fromIntegral numPoints - 1)]]

-- Apply Hamming window to sinc function
applyHammingWindow :: [Float] -> [Float]
applyHammingWindow sinc = zipWith (*) sinc (hammingWindow (length sinc))

-- Generate FIR coefficients for a low-pass filter
generateFIRCoeffs :: Float -> Int -> Filter
generateFIRCoeffs cutoff numPoints = FIRCoeffs (applyHammingWindow $ sincFunction cutoff numPoints)

-- Function to generate coefficients for a high pass IIR filter
generateIIRCoeffs :: Float -> Filter
generateIIRCoeffs cutoff = IIRCoeffs [a0] [1.0, b1]
    where
        -- Pre-calculations
        theta = 2 * pi * cutoff / sampleRate
        gamma = cos(theta) / sin(theta)
        
        -- Coefficients for a first-order Butterworth high pass filter
        a0 = 1 / (1 + gamma)
        b1 = -a0
