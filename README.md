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


### Example programs

Peterson lock: Driver spawns 2 workers and checks that they never both own the lock. Workers spin in lock/unlock.

Java source from the internets:

    public void lock() {
        int i = THREAD_ID.get() % 2;
        int j = 1 - i;
        flag [i].set(true); // Im interested
        victim = i ; // you go first
        while ( flag[j].get() && victim == i) {}; // wait
    }
    
    public void unlock() {
        int i = THREAD_ID.get() % 2;
        flag[i].set(false); // Im not interested
    }

Driver:

    newVar own1 False
    newVar own2 False
    newVar flag1 False
    newVar flag2 False
    newVar victim 0
    spawn (worker 1)
    spawn (worker 2)
    label check: 
    get own1  
    get own2
    and
    not
    jmpcond check
    ndconst [false]
    assert

Worker(i):
  
    # lock
    ndconst [true]
    set flag(i) # I'm interested
    ndconst [i]
    set victim # you go first
    label wait:
    get flag(1-i)
    get victim
    cmpeq i
    and
    not
    jmpcond wait
    ndconst [true]
    set own(i)
    # unlock
    ndconst [false]
    set flag(i)
    ndconst [false]
    set own(i)

(of course this stuff can be simplified into higher-level constructs, but you get the idea)