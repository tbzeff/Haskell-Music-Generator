module HalloweenMusic (song) where

import Defaults
import Composition

bassEnv :: Envelope
bassEnv = Env 0.2 0.4 0.4 0.2

leadEnv :: Envelope
leadEnv = Env 0.3 0.3 0.5 0.2

bassConfig :: SignalConfig
bassConfig = SignalConfig Sqr 82.5 0.3 293.66 4 (Just bassEnv)

leadConfig :: SignalConfig
leadConfig = SignalConfig Saw 82.5 0.5 293.66 4 (Just leadEnv)

myInterval :: Bool -> Semitones -> Semitones -> Beats -> Beats -> [Note]
myInterval ascnd n semi beats1 beats2
    | not ascnd = [(Note n beats1), (Note (n - semi) beats2)]
    | otherwise = [(Note n beats1), (Note (n + semi) beats2)]

bassline1 :: [Note] 
bassline1 =
    (repl 3 $ 
        [
              Note (-12) 0.875
            , Rest 0.125
            , thirty2ndNote (-12)
            , Rest 0.125
            , thirty2ndNote (-12)
            , Rest 0.125
            , thirty2ndNote (-12)
            , Rest 0.125
            , thirty2ndNote (-12)            
            , Rest 0.125     
        ]
    ) ++ (concat      
        [
              repl 4 $ [thirty2ndNote (-12)]
            , repl 4 $ [thirty2ndNote (-11)]
            , repl 4 $ [thirty2ndNote (-9)]
            , repl 4 $ [thirty2ndNote (-11)]
        ]
    )

bassline2 :: [Note]
bassline2 =
    (repl 3 $ 
        [
              Note (-13) 0.875
            , Rest 0.125
            , thirty2ndNote (-13)
            , Rest 0.125
            , thirty2ndNote (-13)
            , Rest 0.125
            , thirty2ndNote (-13)
            , Rest 0.125
            , thirty2ndNote (-13)            
            , Rest 0.125     
        ]
    ) ++ (concat
        [
              repl 4 $ [thirty2ndNote (-13)]
            , repl 4 $ [thirty2ndNote (-12)]
            , repl 4 $ [thirty2ndNote (-10)]
            , repl 4 $ [thirty2ndNote (-9)]
        ]
    )

bassline' :: [Signal]
bassline' = createSignal bassConfig $ repl 2 $ bassline1 ++ bassline2

lead1 :: [Note]
lead1 = concat
        [
              myInterval True 0 7 0.5 1.0
            , myInterval False 8 1 0.5 1.5
        ] ++ [eighthRest]
        ++ concat 
        [
              myInterval True 0 7 0.5 1.0
            , myInterval False 8 1 0.5 1.0
        ] ++ [qrtrNote 0]

lead2 :: [Note]
lead2 = concat
        [
              myInterval True (-1) 8 0.5 1.0
            , myInterval False 8 1 0.5 1.5
        ] ++ [eighthRest]
        ++ concat 
        [
              myInterval True (-1) 8 0.5 1.0
            , myInterval False 8 1 0.5 1.0
        ] ++ [qrtrNote 11]

lead' :: [Signal]
lead' = createSignal leadConfig $ repl 2 $ lead1 ++ lead2

intro :: [Signal]
intro = addLayers [bassline', lead']

preVerse :: [Signal]
preVerse = createSignal bassConfig $
        [
            Chord 
            [
                halfNote (-12),
                halfNote (-9), 
                halfNote (-5), 
                halfNote 0
            ]
        ] ++ concat 
        [
              repl 4 $ [thirty2ndNote 0]
            , repl 4 $ [thirty2ndNote (-5)]
            , repl 4 $ [thirty2ndNote (-9)]
            , repl 4 $ [thirty2ndNote (-12)]
        ]

verseBass1 :: [Note]
verseBass1 = 
    [
          sixteenthNote (-12)
        , sixteenthRest
        , sixteenthNote (-12)
        , sixteenthNote (-10) 
        , sixteenthNote (-9)
        , sixteenthRest
        , sixteenthNote (-10)
        , sixteenthRest
        , sixteenthNote (-12)
        , sixteenthRest
        , sixteenthNote (-4)
        , sixteenthRest
        , qrtrNote (-5)

        , sixteenthNote (-12) 
        , sixteenthRest
        , sixteenthNote (-12) 
        , sixteenthNote (-10)  
        , sixteenthNote (-9) 
        , sixteenthRest
        , sixteenthNote (-10) 
        , sixteenthRest
    ] ++ concat 
    [
          repl 4 $ [thirty2ndNote (-13)]
        , repl 4 $ [thirty2ndNote (-17)]
        , repl 4 $ [thirty2ndNote (-16)]
        , repl 4 $ [thirty2ndNote (-13)]
    ] 

verseBass2 :: [Note]
verseBass2 = 
    [
          sixteenthNote (-12) 
        , sixteenthRest 
        , sixteenthNote (-12) 
        , sixteenthNote (-10)  
        , sixteenthNote (-9) 
        , sixteenthRest 
        , sixteenthNote (-10) 
        , sixteenthRest 
        , sixteenthNote (-12) 
        , sixteenthRest 
        , sixteenthNote (-4) 
        , sixteenthRest 
        , qrtrNote (-5)
    ] ++ concat
    [
          repl 4 $ [thirty2ndNote (-0) ]
        , repl 4 $ [thirty2ndNote (-5) ]
        , repl 4 $ [thirty2ndNote (-9) ]
        , repl 4 $ [thirty2ndNote (-12) ]
    ] ++
    [
          sixteenthNote (-9) 
        , sixteenthNote (-10) 
        , eighthNote (-9)
        , sixteenthNote (-12) 
        , Rest 0.75
    ]

verseBass :: [Signal]
verseBass = createSignal bassConfig $ repl 2 $ verseBass1 ++ verseBass2

verseLead1 :: [Note]
verseLead1 = 
    [
          sixteenthNote 8 
        , sixteenthNote 7 
        , sixteenthNote 8 
        , sixteenthRest 
        , sixteenthNote 7 
        , sixteenthRest 
        , sixteenthNote 0 
        , sixteenthNote 2 
        , sixteenthNote 3 
        , sixteenthRest 
        , sixteenthNote 2 
        , sixteenthRest 
        , qrtrNote 0

        , sixteenthNote 8 
        , sixteenthNote 7 
        , sixteenthNote 8 
        , sixteenthRest 
        , sixteenthNote 7 
        , sixteenthRest 
        , sixteenthNote 0 
        , sixteenthNote 2 
        , Note 3 1.75
        , sixteenthRest 
    ]

verseLead2 :: [Note]
verseLead2 = 
    [
          sixteenthNote 8 
        , sixteenthNote 7 
        , sixteenthNote 8 
        , sixteenthRest 
        , sixteenthNote 7 
        , sixteenthRest 
        , sixteenthNote 0 
        , sixteenthNote 2 
        , sixteenthNote 3 
        , sixteenthRest 
        , sixteenthNote 2 
        , sixteenthRest 
        , qrtrNote 0 

        , sixteenthNote 0 
        , sixteenthNote 2 
        , sixteenthNote 3 
        , sixteenthNote 8 
        , sixteenthNote 7 
        , sixteenthNote 2 
        , sixteenthNote 0 
        , sixteenthNote (-1) 
        , eighthNote 12
        , eighthNote 11
        , sixteenthNote 0
        , Rest 0.75
    ]

verseLead :: [Signal]
verseLead = createSignal leadConfig $ repl 2 $ verseLead1 ++ verseLead2

verse :: [Signal]
verse = addLayers [verseLead, verseBass]

break1Bass1 :: [Note]
break1Bass1 = 
    [
          sixteenthNote (-7) 
        , sixteenthNote (-9) 
        , sixteenthNote (-7) 

        , sixteenthNote (-13) 
        , sixteenthNote (-13) 

        , sixteenthNote (-7) 
        , sixteenthNote (-9) 
        , eighthNote (-7) 

        , sixteenthNote (-13) 
        , sixteenthNote (-13) 
        , sixteenthNote (-13) 

        , sixteenthNote (-7) 
        , sixteenthNote (-9) 
        , sixteenthNote (-13) 
        , sixteenthNote (-9) 

        , sixteenthNote (-5) 
        , sixteenthNote (-10) 
        , sixteenthNote (-5) 

        , sixteenthNote (-12) 
        , sixteenthNote (-12) 

        , sixteenthNote (-5) 
        , sixteenthNote (-10) 
        , eighthNote (-5) 

        , sixteenthNote (-12) 
        , sixteenthNote (-12) 
        , sixteenthNote (-12) 

        , sixteenthNote (-5) 
        , sixteenthNote (-10) 
        , sixteenthNote (-12) 
        , sixteenthNote (-10) 
    ]

break1Bass2 :: [Note]
break1Bass2 = 
    [
          sixteenthNote (-7) 
        , sixteenthNote (-9) 
        , sixteenthNote (-7) 

        , sixteenthNote (-13) 
        , sixteenthNote (-13) 

        , sixteenthNote (-7) 
        , sixteenthNote (-9) 
        , eighthNote (-7) 

        , sixteenthNote (-13) 
        , sixteenthNote (-13) 
        , sixteenthNote (-13) 

        , sixteenthNote (-7) 
        , sixteenthNote (-9) 
        , sixteenthNote (-13) 
        , sixteenthNote (-9) 

        , sixteenthNote (-5) 
        , sixteenthNote (-10) 
        , sixteenthNote (-5) 

        , sixteenthNote (-12) 
        , sixteenthNote (-12) 

        , sixteenthNote (-5) 
        , sixteenthNote (-4) 
        , eighthNote (-2) 

        , sixteenthNote (-12) 
        , sixteenthNote (-12) 
        , sixteenthNote (-12) 

        , sixteenthNote (-2) 
        , sixteenthNote 0 
        , sixteenthNote (-2) 
        , sixteenthNote (-4)   
    ]

break1Bass :: [Signal] 
break1Bass = createSignal bassConfig $ repl 2 $ break1Bass1 ++ break1Bass2

break1Lead1 :: [Note] 
break1Lead1 =
    [ 
          Chord [sixteenthNote 2, sixteenthNote 5]
        , eighthRest
        , Chord [sixteenthNote 2, sixteenthNote 5]
       
        , eighthRest
        , Chord [eighthNote (-1), eighthNote 7]

        , eighthRest
        , Chord [eighthNote (-1), eighthNote 2]

        , Chord [eighthNote 0, eighthNote 3]
        , Chord [eighthNote (-1), eighthNote 2]

        , Chord [sixteenthNote 0, sixteenthNote 3]
        , eighthRest
        , Chord [sixteenthNote (-1), sixteenthNote 2]

        , eighthRest
        , Chord [eighthNote 0, eighthNote 3, eighthNote 7, eighthNote 12]
        
        , eighthRest
        , sixteenthNote 12
        , sixteenthNote 7
        
        , sixteenthNote 5
        , sixteenthNote 3
    ] ++ triplet 2 0.25 ++ [sixteenthNote (-1)]

break1Lead2 :: [Note]
break1Lead2 =
    [ 
          Chord [sixteenthNote 2, sixteenthNote 5]
        , eighthRest
        , Chord [sixteenthNote 2, sixteenthNote 5]
       
        , eighthRest
        , Chord [eighthNote (-1), eighthNote 7]

        , eighthRest
        , Chord [eighthNote (-1), eighthNote 2]

        , Chord [eighthNote 0, eighthNote 3]
        , Chord [eighthNote (-1), eighthNote 2]
    ] ++ concat
    [
          repl 6 $ [thirty2ndNote 12]
        , repl 6 $ [thirty2ndNote 11]
    ] ++ [eighthNote 7]
    ++ concat
    [
          repl 6 $ [thirty2ndNote 8]
        , repl 6 $ [thirty2ndNote 7]
    ] ++ [eighthNote 2]

break1Lead :: [Signal]
break1Lead = createSignal leadConfig $ repl 2 $ break1Lead1 ++ break1Lead2

break1 :: [Signal]
break1 = (addLayers [break1Bass, break1Lead])-- ++ sixteenthRest

breakdownBass :: [Signal]
breakdownBass = createSignal bassConfig $ concat
    [ 
          repl 4 $ triplet (-13) 0.25 ++ [eighthRest]
        , repl 2 $ triplet (-13) 0.25 ++ [sixteenthRest]

        , repl 4 $ triplet (-12) 0.25 ++ [eighthRest]
        , triplet (-10) 0.25 ++ [sixteenthRest] ++ triplet (-9) 0.25 ++ [sixteenthRest]

        , repl 4 $ triplet (-13) 0.25 ++ [eighthRest]
        , triplet (-9) 0.25 ++ [sixteenthRest] ++ triplet (-10) 0.25 ++ [sixteenthRest]

        , repl 4 $ triplet (-12) 0.25 ++ [eighthRest] 
        , triplet (-4) 0.25 ++ [sixteenthRest] ++ triplet (-5) 0.25 ++ [sixteenthRest]
    ] 
    ++ concat
    [
          repl 4 $ triplet (-13) 0.25 ++ [eighthRest]
        , repl 2 $ triplet (-13) 0.25 ++ [sixteenthRest]

        , repl 4 $ triplet (-12) 0.25 ++ [eighthRest]
        , triplet (-10) 0.25 ++ [sixteenthRest] ++ triplet (-9) 0.25 ++ [sixteenthRest]

        , repl 4 $ triplet (-10) 0.25 ++ [eighthRest ]
        , triplet (-7) 0.25 ++ [sixteenthRest] ++ triplet (-10) 0.25 ++ [sixteenthRest]

        , triplet (-9) 0.25 ++ [eighthRest] ++ triplet (-10) 0.25 ++ [eighthRest]
        , triplet (-12) 0.25 ++ [eighthRest] ++ triplet (-12) 0.25 ++ [eighthRest]
        , repl 2 $ triplet (-12) 0.25 ++ [sixteenthRest]
    ]

breakdownLead :: [Signal]
breakdownLead = createSignal leadConfig $
    [
          sixteenthNote 8
        , sixteenthNote 7
        , sixteenthNote 5
        , sixteenthNote 7
        , sixteenthNote 5
        , sixteenthNote 3
        , sixteenthNote 2 
        , sixteenthNote 3
        , sixteenthNote 2
        , sixteenthNote 0
        , sixteenthNote (-1) 
        , sixteenthNote 0 
        , sixteenthNote (-1) 
        , sixteenthNote (-4)
        , sixteenthNote (-5)
        , sixteenthNote (-4)
    ] ++ concat
    [
          repl 2 $ triplet (-5) 0.25 ++ [eighthRest]
        , triplet (-2) 0.25 ++ [eighthRest]
        , triplet (-4) 0.25 ++ [eighthRest]
        , triplet (-4) 0.25 ++ [sixteenthRest]
        , triplet (-5) 0.25 ++ [sixteenthRest]
    ] ++
    [
          sixteenthNote 8
        , sixteenthNote 7
        , sixteenthNote 5
        , sixteenthNote 7
        , sixteenthNote 5
        , sixteenthNote 3
        , sixteenthNote 2 
        , sixteenthNote 3
        , sixteenthNote 2
        , sixteenthNote 0
        , sixteenthNote (-1) 
        , sixteenthNote 0 
        , sixteenthNote (-1) 
        , sixteenthNote (-4)
        , sixteenthNote (-5)
        , sixteenthNote (-4)
    ] ++ concat
    [
          repl 6 $ [thirty2ndNote 0]
        , repl 6 $ [thirty2ndNote 2]
        , repl 4 $ [thirty2ndNote 3]
        , repl 6 $ [thirty2ndNote 7]
        , repl 6 $ [thirty2ndNote 5]
        , repl 4 $ [thirty2ndNote 2]
    ] ++ concat
    [
          triplet 3 0.25 ++ [eighthRest]
        , triplet 2 0.25 ++ [eighthRest]
        , triplet (-1) 0.25 ++ [sixteenthRest]
        , triplet (-4) 0.25 ++ [eighthRest]
        , triplet (-1) 0.25 ++ [eighthRest]
        , triplet (-4) 0.25 ++ [sixteenthRest]
        , repl 4 $ triplet (-5) 0.25 ++ [eighthRest]
        , triplet (-1) 0.25 ++ [sixteenthRest]
        , triplet (-4) 0.25 ++ [sixteenthRest]
        , repl 4 $ triplet 2 0.25 ++ [eighthRest]
        , triplet 2 0.25 ++ [sixteenthRest]
        , triplet (-1) 0.25 ++ [sixteenthRest]
        , triplet 3 0.25 ++ [eighthRest]
        , triplet 7 0.25 ++ [eighthRest]
        , triplet 12 0.25 ++ [eighthRest]
        , triplet 7 0.25 ++ [eighthRest]
        , triplet 0 0.25 ++ [sixteenthRest] ++ triplet 0 0.25
    ]

breakdown :: [Signal]
breakdown = addLayers [breakdownBass, breakdownLead]

song :: [Signal]
song = concat [intro, preVerse, verse, break1, breakdown, preVerse, verse]