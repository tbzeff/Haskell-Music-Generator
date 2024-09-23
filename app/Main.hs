module Main (main) where

import Utility
import Defaults
import Visualize
import Composition
import Freq
import Envelopes
import Filters
import Web.Browser
import HalloweenMusic

main :: IO ()
main = do 
    putStrLn "1. Save Halloween Protocol"
    putStrLn "2. Plot Protocol"
    choice <- getLine
    case choice of
        "1" -> saveHalloweenProtocol
        "2" -> plotProtocol
        _   -> putStrLn "Invalid input. Exiting."

saveHalloweenProtocol :: IO ()
saveHalloweenProtocol = do
    putStrLn "Saving the halloween-y song..."
    --saveWave ("audio\\halloween-bass") basstrack
    --saveWave ("audio\\halloween-lead") leadtrack
    saveWave ("audio\\halloweeny") song
    putStrLn "Done."

plotProtocol :: IO ()
plotProtocol= do
    putStrLn "How many semitones from A?"
    semis  <- getLine
    putStrLn "Duration (in beat length)?"
    dur    <- getLine
    putStrLn "Cutoff frequency?"
    cutoff <- getLine
    putStrLn "Filename? (Do not include extension)"
    fn     <- getLine
    
    let myNote = note (read semis) (read dur)
    let myEnv = Env 0.2 0.4 0.4 0.2
    let myEnvNote = applyEnvelope myEnv myNote
    let fir = generateFIRCoeffs (read cutoff) 100
    --let iir = generateIIRCoeffs (read cutoff)
    let myFilterNote = applyFilter fir myNote
    let myEnvFilterNote = applyFilter fir myEnvNote

    plotFreq ("plots\\" ++ fn) myNote 
    plotFreq ("plots\\env-" ++ fn) myEnvNote
    plotFreq ("plots\\filter-" ++ fn) myFilterNote
    plotFreq ("plots\\filter-env-" ++ fn) myEnvFilterNote

    saveWave ("audio\\" ++ fn) myNote
    saveWave ("audio\\env-" ++ fn) myEnvNote
    saveWave ("audio\\filter-" ++ fn) myFilterNote
    saveWave ("audio\\filter-env-" ++ fn) myEnvFilterNote
    
    succ1 <- openBrowser $ "G:\\tbzeph\\MyProjects\\Haskell\\my-music-generator\\plots\\" ++ fn ++ ".html" 
    succ2 <- openBrowser $ "G:\\tbzeph\\MyProjects\\Haskell\\my-music-generator\\plots\\" ++ ("env-" ++ fn) ++ ".html" 
    succ3 <- openBrowser $ "G:\\tbzeph\\MyProjects\\Haskell\\my-music-generator\\plots\\" ++ ("filter-" ++ fn) ++ ".html" 
    succ4 <- openBrowser $ "G:\\tbzeph\\MyProjects\\Haskell\\my-music-generator\\plots\\" ++ ("filter-env-" ++ fn) ++ ".html" 

    if (not succ1) || (not succ2) || (not succ3) || (not succ4)
    then putStrLn "At least one of the links failed to open."
    else putStrLn "Links opened successfully."
    
