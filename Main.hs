module Main where

import Step

nop = Arith $ \s -> [s]
pushI i = Arith $ \s -> [IntValue i:s]
pushB b = Arith $ \s -> [BoolValue b:s]
unopI op = Arith $ \(IntValue a:s) -> [IntValue (op a):s]
unopB op = Arith $ \(BoolValue a:s) -> [BoolValue (op a):s]
cmp op = Arith $ \(IntValue b:IntValue a:s) -> [BoolValue (op a b):s]

-- while(true) v++
incrementer v = compile [
  Label "loop" $ Get v,
  unopI (+1), 
  Set v, 
  Jmp "loop"
  ]

-- while(true) {lock(mon) {if(v1 <= v2) v1++}}
syncIncrementer v1 v2 mon = compile [
  Label "loop" $ 
    Enter mon,
    Get v1,
    Get v2,
    cmp (<=),
    JmpCond "ok",
    Jmp "leave",
  Label "ok" $ 
    Get v1,
    unopI (+1),
    Set v1,
  Label "leave" $ 
    Leave mon,
    Jmp "loop"
  ]

main1 = initState [("a", IntValue 1), ("b", IntValue 1)] [] (compile [
  Spawn "a" (incrementer "a"),
  Spawn "b" (incrementer "b")
  ])

main2 = initState [("a", IntValue 1), ("b", IntValue 1)] ["m"] (compile [
  Spawn "a" (syncIncrementer "a" "b" "m")
 ,Spawn "b" (syncIncrementer "b" "a" "m")
  ])

deadlock = initState [] ["a","b"] (compile [
  Spawn "ta" $ compile [
      Label "loop" $ Enter "a",
      Enter "b",
      Leave "b",
      Leave "a",
      Jmp "loop"
    ],
  Spawn "tb" $ compile [
      Label "loop" $ Enter "b",
      Enter "a",
      Leave "a",
      Leave "b",
      Jmp "loop"
    ]
  ])

-- spin in lock/unlock
petersonThread id myFlag otherFlag victim claim = compile [ Label "loop" $ nop, Block lock, Block unlock, Jmp "loop" ]
  where
    lock = [
        pushB True,
        Set myFlag, -- I'm interested
        pushI id,
        Set victim, -- You go first
      Label "wait" $ 
        Get otherFlag, -- if(!otherFlag) break;
        unopB not,
        JmpCond "leaveLock", 
        Get victim, -- if(victim != i) break;
        pushI id,
        cmp (/=),
        JmpCond "leaveLock",
        Jmp "wait",

      Label "leaveLock" $ 
        pushB True,
        Set claim
     ]

    unlock = [ 
        pushB False, 
        Set claim,
        pushB False, 
        Set myFlag
      ]

petersonDriver = initState [("flagA",BoolValue False), 
                            ("flagB", BoolValue False), 
                            ("victim", IntValue 0), 
                            ("claimA", BoolValue False), 
                            ("claimB", BoolValue False)] [] $ compile [
    Spawn "ta" (petersonThread 1 "flagA" "flagB" "victim" "claimA"),
    Spawn "tb" (petersonThread 1 "flagB" "flagA" "victim" "claimB")
  ]

{- 

*Main> let g = stateGraph petersonDriver 60
*Main> let nodes = Data.Map.elems $ sg_index2node g
*Main> let claimA n = case (st_vars n Data.Map.! "claimA") of {BoolValue True -> True; _ -> False}
*Main> let claimB n = case (st_vars n Data.Map.! "claimB") of {BoolValue True -> True; _ -> False}
*Main> [n | n <- nodes, claimA n && claimB n]
[]
*Main>

-}  
