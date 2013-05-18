module Types where

import qualified Data.Map as M

data Insn =
    Label String
  | Block [Insn]
  | Jmp String
  | JmpCond String
  | Get String
  | Set String
  | Arith ([Value] -> [[Value]])
  | Enter String
  | TryEnter String
  | Leave String
  | Spawn String Prog
  | Assert String

instance Show Insn where
  show (Label s) = "label " ++ s ++ ":"
  show (Block is) = "block " ++ show is
  show (Jmp s) = "jmp " ++ s
  show (JmpCond s) = "jmpcond " ++ s
  show (Get s) = "get " ++ s
  show (Set s) = "set " ++ s
  show (Arith _) = "arith <...>"
  show (Enter s) = "enter " ++ s
  show (TryEnter s) = "tryenter " ++ s
  show (Leave s) = "leave " ++ s
  show (Spawn s _) = "spawn " ++ s
  show (Assert s) = "assert " ++ s

data Value = IntValue Int | BoolValue Bool | PidValue Pid deriving (Ord, Eq)

instance Show Value where
  show (IntValue i) = show i
  show (BoolValue b) = show b
  show (PidValue p) = show p

data Pid = Pid Int deriving (Eq, Ord)

instance Show Pid where
  show (Pid p) = show p

data Prog = Prog 
  { 
    prog_insns :: [Insn] 
  }

data ProcState = Running
  { 
    proc_prog :: Prog,
    proc_ip :: Int,
    proc_stack :: [Value],
    proc_waitedMon :: Maybe String
  }
  | Finished

data MonState = MonFree | MonOccupied { mon_owner :: Pid, mon_depth :: Int {- , mon_waiters :: Queue Pid -} } deriving (Ord, Eq, Show)

data ProgramState = ProgramState
  { 
    st_procs :: M.Map Pid (String, ProcState),
    st_vars :: M.Map String Value,
    st_mons :: M.Map String MonState,
    st_lastStepped :: Maybe Pid
  }

instance Show ProgramState where
  show st = show (st_vars st, st_mons st, [(pid, name, showProc p) | (pid,(name,p)) <- M.toList (st_procs st)]) 
    where
      showProc Finished = "<finished>"
      showProc r@Running{proc_waitedMon=Nothing, proc_ip=ip} = show ip
      showProc r@Running{proc_waitedMon=Just m,  proc_ip=ip} = show ip ++ "?" ++ m

stateSig s = (st_vars s, st_mons s, [(pid, sigP p) | (pid,(name,p)) <- M.toList (st_procs s)] )
  where
    sigP Finished = Nothing
    sigP r@Running{} = Just (proc_ip r, proc_stack r, proc_waitedMon r)

instance Eq ProgramState where
  (==) a b = (stateSig a == stateSig b)
instance Ord ProgramState where
  compare a b = compare (stateSig a) (stateSig b)

initState :: [(String,Value)] -> [String] -> Prog -> ProgramState
initState vars mons entryPoint = ProgramState {
    st_procs = M.fromList [(Pid 0, ("entry", Running {proc_prog = entryPoint, proc_ip = 0, proc_stack = [], proc_waitedMon = Nothing}))],
    st_vars  = M.fromList vars,
    st_mons  = M.fromList [(m, MonFree) | m <- mons],
    st_lastStepped = Nothing
  }

compile :: [Insn] -> Prog
compile is = Prog {prog_insns = expandBlocks is}
  where
    expandBlocks = concatMap (\i -> case i of { Block is -> expandBlocks is ; j -> [j] })

isLocal :: Insn -> Bool
isLocal (Block insns) = all isLocal insns
isLocal Get{} = False
isLocal Set{} = False
isLocal Enter{} = False
isLocal TryEnter{} = False
isLocal Leave{} = False
isLocal Spawn{} = False
isLocal _ = True


