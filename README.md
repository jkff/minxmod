Lightweight verification of multithreaded programs.

See Step.hs - it is quite obvious.

See Main.hs for several more examples - Peterson lock, deadlock etc.

We can also record traces in addition to states.
It is not hard to add wait/notify (aka wait/pulseall), atomics, exceptions, thread interrupts.

Example:

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
      Spawn "b" (incrementer "b")
      ])


    *Main Data.List Control.Monad> mapM_ print $ nub $ map fst $ runStep (replicateM 15 stepState) i
    (fromList [("a",IntValue 4),("b",IntValue 1)],fromList [],[(Pid 0,"entry","<finished>"),(Pid 1,"a","1"),(Pid 2,"b","0")])
    (fromList [("a",IntValue 4),("b",IntValue 1)],fromList [],[(Pid 0,"entry","<finished>"),(Pid 1,"a","0"),(Pid 2,"b","1")])
    (fromList [("a",IntValue 4),("b",IntValue 1)],fromList [],[(Pid 0,"entry","<finished>"),(Pid 1,"a","3"),(Pid 2,"b","2")])
    (fromList [("a",IntValue 3),("b",IntValue 2)],fromList [],[(Pid 0,"entry","<finished>"),(Pid 1,"a","2"),(Pid 2,"b","3")])
    (fromList [("a",IntValue 3),("b",IntValue 2)],fromList [],[(Pid 0,"entry","<finished>"),(Pid 1,"a","1"),(Pid 2,"b","0")])
    (fromList [("a",IntValue 3),("b",IntValue 2)],fromList [],[(Pid 0,"entry","<finished>"),(Pid 1,"a","0"),(Pid 2,"b","1")])
    (fromList [("a",IntValue 3),("b",IntValue 2)],fromList [],[(Pid 0,"entry","<finished>"),(Pid 1,"a","3"),(Pid 2,"b","2")])
    (fromList [("a",IntValue 2),("b",IntValue 3)],fromList [],[(Pid 0,"entry","<finished>"),(Pid 1,"a","2"),(Pid 2,"b","3")])
    (fromList [("a",IntValue 2),("b",IntValue 3)],fromList [],[(Pid 0,"entry","<finished>"),(Pid 1,"a","1"),(Pid 2,"b","0")])
    (fromList [("a",IntValue 2),("b",IntValue 3)],fromList [],[(Pid 0,"entry","<finished>"),(Pid 1,"a","0"),(Pid 2,"b","1")])
    (fromList [("a",IntValue 2),("b",IntValue 3)],fromList [],[(Pid 0,"entry","<finished>"),(Pid 1,"a","3"),(Pid 2,"b","2")])
    (fromList [("a",IntValue 1),("b",IntValue 4)],fromList [],[(Pid 0,"entry","<finished>"),(Pid 1,"a","2"),(Pid 2,"b","3")])
    (fromList [("a",IntValue 1),("b",IntValue 4)],fromList [],[(Pid 0,"entry","<finished>"),(Pid 1,"a","1"),(Pid 2,"b","0")])
    (fromList [("a",IntValue 1),("b",IntValue 4)],fromList [],[(Pid 0,"entry","<finished>"),(Pid 1,"a","0"),(Pid 2,"b","1")])
    (fromList [("a",IntValue 4),("b",IntValue 1)],fromList [],[(Pid 0,"entry","1"),(Pid 1,"b","2")]

