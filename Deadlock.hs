module Deadlock where

import Types
import ToDot
import StateGraph

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

main = do
  let g = stateGraph deadlock 60
  putStrLn $ toDot g
