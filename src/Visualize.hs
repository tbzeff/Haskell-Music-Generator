-- https://jonathanreeve.github.io/plotlyhs/

module Visualize where

import Defaults
import Graphics.Plotly
import Graphics.Plotly.Lucid
import Graphics.Plotly.Simple
import Lucid 
import Lucid.Html5
import Lens.Micro

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT


plotFreq :: String -> [Pulse] -> IO()
plotFreq filename signal = do 
    let time = take (length signal) [0, 1/sampleRate ..]
    let points = zip time signal
    -- let myTrace = line (aes & x .~ fst
    --                         & y .~ snd) points

    -- let myLayout = defLayout & _title .~ Just (T.pack "Audio Signal Visualization")
    --                          & _xaxis . _axistitle .~ Just (T.pack "Time (s)")
    --                          & _yaxis . _axistitle .~ Just (T.pack "Amplitude")

    LT.writeFile (filename ++ ".html") $ renderText $ doctypehtml_ $ do
        head_ $ do meta_ [charset_ (T.pack "utf-8")]
                   plotlyCDN
        body_ $ toHtml $ plotly (T.pack "plotID") [linePlot points] -- & layout .~ myLayout

