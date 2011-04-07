module Main where

import Step

incrementer v = compile [
  Label "loop" $ Get v,
  Arith $ \(IntValue a:s) -> [(IntValue (a+1):s)], 
  Set v, 
  Jmp "loop"
  ]

i = initState [("a", IntValue 1), ("b", IntValue 1)] [] (compile [
  Spawn "a" (incrementer "a"),
  Spawn "a" (incrementer "b")
  ])

