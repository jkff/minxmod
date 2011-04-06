Lightweight verification of multithreaded programs.

Idea: have a simple stack-based nondeterministic programming language and explore the state space. Similar to what SPIN does.
    
    data Insn where
      Label :: String -> Insn -> Insn
      Jmp :: String -> Insn
      JmpCond :: String -> Insn
      NewVar :: String -> Value -> Insn
      NewMon :: String -> Insn
      Get :: VarId -> Insn -- push value
      Set :: VarId -> Insn -- pop value
      Arith :: ArithOp -> Insn
      Enter :: VarId -> Insn
      TryEnter :: VarId -> Insn -- push bool
      Leave :: VarId Mon -> Insn
      Spawn :: Prog -> Insn -- push pid
      Assert :: Bool -> Insn

    data Value = IntVal Int | BoolVal Bool

    data ArithOp =
      NDConst [Value] | -- nondeterministic constant
      Ignore | Dup | ...
      Add | Mul | Sub | Div | Mod | 
      And | Or | Not |

    data Prog where
      Prog { instructions :: [(Maybe String, AnyInsn)] }

    data ProgramState = ProgramState
      { 
        progs :: [(Pid, Prog)],

        monStates :: [(String, Bool)],
        insnPointers :: [(Pid, Int)],
        stacks :: [(Pid, [Value])]
        varStates :: [(String, Value)]
      }

    expand :: ProgramState -> [ProgramState]
    expand = ...(not hard to write)...

    allStates = initialState : concatMap expand allStates

We can also record traces in addition to states.

It is not hard to add wait/notify (aka wait/pulseall), atomics, exceptions, thread interrupts.
