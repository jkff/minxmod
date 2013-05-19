module Deadlock where

import qualified Data.Map as M

import Types
import ToDot
import StateGraph
import CTL
import ProgramPred

nop = Arith $ \s -> [s]
pushI i = Arith $ \s -> [IntValue i:s]
pushB b = Arith $ \s -> [BoolValue b:s]
unopI op = Arith $ \(IntValue a:s) -> [IntValue (op a):s]
unopB op = Arith $ \(BoolValue a:s) -> [BoolValue (op a):s]
cmp op = Arith $ \(IntValue b:IntValue a:s) -> [BoolValue (op a b):s]

deadlock = initState [] ["a","b"] (compile [
  Spawn "ta" $ compile [
      Label "loop",
      Enter "a",
      Enter "b",
      Leave "b",
      Leave "a",
      Jmp "loop"
    ],
  Spawn "tb" $ compile [
      Label "loop",
      Enter "b",
      Enter "a",
      Leave "a",
      Leave "b",
      Jmp "loop"
    ]
  ])

g = stateGraph deadlock 60
formula = CTLNeg (CTLExistsNext CTLTrue) :: CTL Bool
badStateIndices = verifySG formula g
badStates = [sg_index2node g M.! i | i <- badStateIndices]

main = putStrLn $ toDot g
