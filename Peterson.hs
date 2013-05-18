module Peterson where

import Types
import ToDot
import StateGraph

nop = Arith $ \s -> [s]
pushI i = Arith $ \s -> [IntValue i:s]
pushB b = Arith $ \s -> [BoolValue b:s]
unopI op = Arith $ \(IntValue a:s) -> [IntValue (op a):s]
unopB op = Arith $ \(BoolValue a:s) -> [BoolValue (op a):s]
cmp op = Arith $ \(IntValue b:IntValue a:s) -> [BoolValue (op a b):s]

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

main = do
  let g = stateGraph petersonDriver 60
  putStrLn $ toDot g

