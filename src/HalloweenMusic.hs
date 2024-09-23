module HalloweenMusic where

import Defaults
import Freq
import Composition
import Utility

halloweenVolume :: Float
halloweenVolume = 0.2

halloweenNote :: Semitones -> Beats->  [Pulse]
halloweenNote n beats = map (* volume) $ envNote n beats (Just (Env 0.2 0.4 0.4 0.2))

eighth :: Semitones -> [Pulse]
eighth n = halloweenNote n 0.125 

qrtr :: Semitones -> [Pulse] 
qrtr n = halloweenNote n 0.25

half :: Semitones -> [Pulse]
half n = halloweenNote n 0.5

whole :: Semitones -> [Pulse]
whole n = halloweenNote n 1.0

triplet :: Semitones -> Beats -> [Pulse]
triplet n beats = repl 3 $ halloweenNote n (beats / 3)

myInterval :: Bool -> Semitones -> Semitones -> Beats -> Beats -> [Pulse]
myInterval ascnd n semi beats1 beats2
    | not ascnd = (halloweenNote n beats1) ++ (halloweenNote (n - semi) beats2)
    | otherwise = (halloweenNote n beats1) ++ (halloweenNote (n + semi) beats2) 

bassline1 :: [Pulse] 
bassline1 =
    (repl 3 $ 
        concat
        [
              halloweenNote (-12) 0.875
            , rest 0.125
            , eighth (-12)
            , rest 0.125
            , eighth (-12)
            , rest 0.125
            , eighth (-12)
            , rest 0.125
            , eighth (-12)            
            , rest 0.125     
        ]
    ) ++ (
        concat       
        [
              repl 4 $ eighth (-12)
            , repl 4 $ eighth (-11)
            , repl 4 $ eighth (-9)
            , repl 4 $ eighth (-11)
        ]
    )
    

bassline2 :: [Pulse]
bassline2 =
    (repl 3 $ 
        concat 
        [
              halloweenNote (-13) 0.875
            , rest 0.125
            , eighth (-13)
            , rest 0.125
            , eighth (-13)
            , rest 0.125
            , eighth (-13)
            , rest 0.125
            , eighth (-13)            
            , rest 0.125     
        ]
    ) ++ (
        concat       
        [
              repl 4 $ eighth (-13)
            , repl 4 $ eighth (-12)
            , repl 4 $ eighth (-10)
            , repl 4 $ eighth (-9)
        ]
     )

bassline' :: [Pulse]
bassline' = repl 2 $ bassline1 ++ bassline2

lead1 :: [Pulse]
lead1 =
    concat
        [
              myInterval True 0 7 0.5 1.0
            , myInterval False 8 1 0.5 1.5
            , rest 0.5
            , myInterval True 0 7 0.5 1.0
            , myInterval False 8 1 0.5 1.0
            , whole 0
        ]

lead2 :: [Pulse]
lead2 =
    concat
        [
              myInterval True (-1) 8 0.5 1.0
            , myInterval False 8 1 0.5 1.5
            , rest 0.5
            , myInterval True (-1) 8 0.5 1.0
            , myInterval False 8 1 0.5 1.0
            , whole 11
        ]

lead' :: [Pulse]
lead' = repl 2 $ lead1 ++ lead2

intro :: [Pulse]
intro = addLayers [bassline', lead']

preVerse :: [Pulse]
preVerse = 
    concat
        [
            addLayers [
                halloweenNote (-12) chordDur,
                halloweenNote (-9) chordDur, 
                halloweenNote (-5) chordDur, 
                halloweenNote 0 chordDur]
            , repl 4 $ eighth 0 
            , repl 4 $ eighth (-5) 
            , repl 4 $ eighth (-9) 
            , repl 4 $ eighth (-12) 
        ]
    where chordDur = 4.0

verseBass1 :: [Pulse]
verseBass1 = 
    concat 
    [
          qrtr (-12)
        , rest 0.25
        , qrtr (-12)
        , qrtr (-10) 
        , qrtr (-9)
        , rest 0.25
        , qrtr (-10)
        , rest 0.25
        , qrtr (-12)
        , rest 0.25
        , qrtr (-4)
        , rest 0.25
        , whole (-5)

        , qrtr (-12) 
        , rest 0.25
        , qrtr (-12) 
        , qrtr (-10)  
        , qrtr (-9) 
        , rest 0.25
        , qrtr (-10) 
        , rest 0.25
        , repl 4 $ eighth (-13)
        , repl 4 $ eighth (-17)
        , repl 4 $ eighth (-16)
        , repl 4 $ eighth (-13)
    ]

verseBass2 :: [Pulse]
verseBass2 = 
    concat 
    [
          qrtr (-12) 
        , rest 0.25
        , qrtr (-12) 
        , qrtr (-10)  
        , qrtr (-9) 
        , rest 0.25
        , qrtr (-10) 
        , rest 0.25
        , qrtr (-12) 
        , rest 0.25
        , qrtr (-4) 
        , rest 0.25
        , whole (-5)

        , repl 4 $ eighth (-0) 
        , repl 4 $ eighth (-5) 
        , repl 4 $ eighth (-9) 
        , repl 4 $ eighth (-12) 
        , qrtr (-9) 
        , qrtr (-10) 
        , half (-9)
        , qrtr (-12) 
        , rest 0.75
    ]

verseBass :: [Pulse]
verseBass = repl 2 $ verseBass1 ++ verseBass2

verseLead1 :: [Pulse]
verseLead1 = 
    concat 
    [
          qrtr 8 
        , qrtr 7 
        , qrtr 8 
        , rest 0.25
        , qrtr 7 
        , rest 0.25
        , qrtr 0 
        , qrtr 2 
        , qrtr 3 
        , rest 0.25
        , qrtr 2 
        , rest 0.25
        , whole 0

        , qrtr 8 
        , qrtr 7 
        , qrtr 8 
        , rest 0.25
        , qrtr 7 
        , rest 0.25
        , qrtr 0 
        , qrtr 2 
        , halloweenNote 3 1.75
        , rest 0.25
    ]

verseLead2 :: [Pulse]
verseLead2 = 
    concat 
    [
          qrtr 8 
        , qrtr 7 
        , qrtr 8 
        , rest 0.25
        , qrtr 7 
        , rest 0.25
        , qrtr 0 
        , qrtr 2 
        , qrtr 3 
        , rest 0.25
        , qrtr 2 
        , rest 0.25
        , whole 0 

        , qrtr 0 
        , qrtr 2 
        , qrtr 3 
        , qrtr 8 
        , qrtr 7 
        , qrtr 2 
        , qrtr 0 
        , qrtr (-1) 
        , half 12
        , half 11
        , qrtr 0
        , rest 0.75
    ]

verseLead :: [Pulse]
verseLead = repl 2 $ verseLead1 ++ verseLead2

verse :: [Pulse]
verse = addLayers [verseLead, verseBass]


break1Bass1 :: [Pulse]
break1Bass1 = 
    concat
    [
          qrtr (-7) 
        , qrtr (-9) 
        , qrtr (-7) 

        , qrtr (-13) 
        , qrtr (-13) 

        , qrtr (-7) 
        , qrtr (-9) 
        , half (-7) 

        , qrtr (-13) 
        , qrtr (-13) 
        , qrtr (-13) 

        , qrtr (-7) 
        , qrtr (-9) 
        , qrtr (-13) 
        , qrtr (-9) 

        , qrtr (-5) 
        , qrtr (-10) 
        , qrtr (-5) 

        , qrtr (-12) 
        , qrtr (-12) 

        , qrtr (-5) 
        , qrtr (-10) 
        , half (-5) 

        , qrtr (-12) 
        , qrtr (-12) 
        , qrtr (-12) 

        , qrtr (-5) 
        , qrtr (-10) 
        , qrtr (-12) 
        , qrtr (-10) 
    ]

break1Bass2 :: [Pulse]
break1Bass2 = 
    concat
    [
          qrtr (-7) 
        , qrtr (-9) 
        , qrtr (-7) 

        , qrtr (-13) 
        , qrtr (-13) 

        , qrtr (-7) 
        , qrtr (-9) 
        , half (-7) 

        , qrtr (-13) 
        , qrtr (-13) 
        , qrtr (-13) 

        , qrtr (-7) 
        , qrtr (-9) 
        , qrtr (-13) 
        , qrtr (-9) 

        , qrtr (-5) 
        , qrtr (-10) 
        , qrtr (-5) 

        , qrtr (-12) 
        , qrtr (-12) 

        , qrtr (-5) 
        , qrtr (-4) 
        , half (-2) 

        , qrtr (-12) 
        , qrtr (-12) 
        , qrtr (-12) 

        , qrtr (-2) 
        , qrtr 0 
        , qrtr (-2) 
        , qrtr (-4)   
    ]

break1Bass :: [Pulse] 
break1Bass = repl 2 $ break1Bass1 ++ break1Bass2

break1Lead1 :: [Pulse] 
break1Lead1 =
    concat
    [ 
          addLayers [qrtr 2, qrtr 5]
        , rest 0.5
        , addLayers [qrtr 2, qrtr 5]
       
        , rest 0.5
        , addLayers [half (-1), half 7]

        , rest 0.5
        , addLayers [half (-1), half 2]

        , addLayers [half 0, half 3]
        , addLayers [half (-1), half 2]

        , addLayers [qrtr 0, qrtr 3]
        , rest 0.5
        , addLayers [qrtr (-1), qrtr 2]

        , rest 0.5
        , addLayers [half 0, half 3, half 7, half 12]
        
        , rest 0.5
        , qrtr 12
        , qrtr 7
        
        , qrtr 5
        , qrtr 3
        , triplet 2 0.25
        , qrtr (-1)
    ]

break1Lead2 :: [Pulse]
break1Lead2 =
    concat
    [ 
          addLayers [qrtr 2, qrtr 5]
        , rest 0.5
        , addLayers [qrtr 2, qrtr 5]
       
        , rest 0.5
        , addLayers [half (-1), half 7]

        , rest 0.5
        , addLayers [half (-1), half 2]

        , addLayers [half 0, half 3]
        , addLayers [half (-1), half 2]

        , repl 6 $ eighth 12
        , repl 6 $ eighth 11
        , half 7

        , repl 6 $ eighth 8
        , repl 6 $ eighth 7
        , half 2
    ]

break1Lead :: [Pulse]
break1Lead = repl 2 $ break1Lead1 ++ break1Lead2

break1 :: [Pulse]
break1 = (addLayers [break1Bass, break1Lead])-- ++ rest 0.25

breakdownBass :: [Pulse]
breakdownBass =
    concat
    [ 
          repl 4 $ triplet (-13) 0.25 ++ rest 0.5
        , repl 2 $ triplet (-13) 0.25 ++ rest 0.25

        , repl 4 $ triplet (-12) 0.25 ++ rest 0.5
        , triplet (-10) 0.25 ++ rest 0.25 ++ triplet (-9) 0.25 ++ rest 0.25

        , repl 4 $ triplet (-13) 0.25 ++ rest 0.5
        , triplet (-9) 0.25 ++ rest 0.25 ++ triplet (-10) 0.25 ++ rest 0.25

        , repl 4 $ triplet (-12) 0.25 ++ rest 0.5
        , triplet (-4) 0.25 ++ rest 0.25 ++ triplet (-5) 0.25 ++ rest 0.25
    ] 
    ++ concat
    [
          repl 4 $ triplet (-13) 0.25 ++ rest 0.5
        , repl 2 $ triplet (-13) 0.25 ++ rest 0.25

        , repl 4 $ triplet (-12) 0.25 ++ rest 0.5
        , triplet (-10) 0.25 ++ rest 0.25 ++ triplet (-9) 0.25 ++ rest 0.25

        , repl 4 $ triplet (-10) 0.25 ++ rest 0.5
        , triplet (-7) 0.25 ++ rest 0.25 ++ triplet (-10) 0.25 ++ rest 0.25

        , triplet (-9) 0.25 ++ rest 0.5 ++ triplet (-10) 0.25 ++ rest 0.5
        , triplet (-12) 0.25 ++ rest 0.5 ++ triplet (-12) 0.25 ++ rest 0.5
        , repl 2 $ triplet (-12) 0.25 ++ rest 0.25
    ]

breakdownLead :: [Pulse]
breakdownLead = 
    concat
    [
          qrtr 8
        , qrtr 7
        , qrtr 5
        , qrtr 7
        , qrtr 5
        , qrtr 3
        , qrtr 2 
        , qrtr 3
        , qrtr 2
        , qrtr 0
        , qrtr (-1) 
        , qrtr 0 
        , qrtr (-1) 
        , qrtr (-4)
        , qrtr (-5)
        , qrtr (-4)

        , repl 2 $ triplet (-5) 0.25 ++ rest 0.5
        , triplet (-2) 0.25 ++ rest 0.5
        , triplet (-4) 0.25 ++ rest 0.5
        , triplet (-4) 0.25 ++ rest 0.25
        , triplet (-5) 0.25 ++ rest 0.25
    ] ++
    concat 
    [
          qrtr 8
        , qrtr 7
        , qrtr 5
        , qrtr 7
        , qrtr 5
        , qrtr 3
        , qrtr 2 
        , qrtr 3
        , qrtr 2
        , qrtr 0
        , qrtr (-1) 
        , qrtr 0 
        , qrtr (-1) 
        , qrtr (-4)
        , qrtr (-5)
        , qrtr (-4)

        , repl 6 $ eighth 0
        , repl 6 $ eighth 2
        , repl 4 $ eighth 3
        , repl 6 $ eighth 7
        , repl 6 $ eighth 5
        , repl 4 $ eighth 2
    ] ++
    concat
    [
          triplet 3 0.25 ++ rest 0.5
        , triplet 2 0.25 ++ rest 0.5
        , triplet (-1) 0.25 ++ rest 0.25
        , triplet (-4) 0.25 ++ rest 0.5
        , triplet (-1) 0.25 ++ rest 0.5
        , triplet (-4) 0.25 ++ rest 0.25
        , repl 4 $ triplet (-5) 0.25 ++ rest 0.5
        , triplet (-1) 0.25 ++ rest 0.25
        , triplet (-4) 0.25 ++ rest 0.25
        , repl 4 $ triplet 2 0.25 ++ rest 0.5
        , triplet 2 0.25 ++ rest 0.25
        , triplet (-1) 0.25 ++ rest 0.25
        , triplet 3 0.25 ++ rest 0.5
        , triplet 7 0.25 ++ rest 0.5
        , triplet 12 0.25 ++ rest 0.5
        , triplet 7 0.25 ++ rest 0.5
        , triplet 0 0.25 ++ rest 0.25 ++ triplet 0 0.25
    ]

breakdown :: [Pulse]
breakdown = addLayers [breakdownBass, breakdownLead] 

basstrack :: [Pulse]
basstrack = concat [bassline', preVerse, verseBass, break1Bass, breakdownBass]

leadtrack :: [Pulse]
leadtrack = concat [lead', [0 | n <- [0.. (length preVerse) - 1]], verseLead, break1Lead, breakdownLead]

song :: [Pulse]
song = concat [intro, preVerse, verse, break1, breakdown, intro, preVerse, verse]
