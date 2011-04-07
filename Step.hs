module Step where

import Data.List
import Control.Monad
import qualified Data.Map as M

data Insn =
    Label String Insn
  | Block [Insn]
  | NewVar String Value
  | NewMon String
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

data Value = IntValue Int | BoolValue Bool | PidValue Pid deriving (Eq, Show)

data Pid = Pid Int deriving (Eq, Ord, Show)

data Prog = Prog 
  { 
    prog_insns :: [Insn] 
  }

data ProcState = Running
  { 
    proc_prog :: Prog,
    proc_ip :: Int,
    proc_stack :: [Value]
  }
  | Finished

data MonState = MonFree | MonOccupied { mon_owner :: Pid, mon_depth :: Int {- , mon_waiters :: Queue Pid -} } deriving (Eq, Show)

data ProgramState = ProgramState
  { 
    st_procs :: M.Map Pid (String, ProcState),
    st_vars :: M.Map String Value,
    st_mons :: M.Map String MonState
  }

instance Show ProgramState where
  show st = show (st_vars st, st_mons st, [(pid, name, case p of { Finished -> "<finished>" ; _ -> show (proc_ip p)}) | (pid,(name,p)) <- M.toList (st_procs st)])  

instance Eq ProgramState where
  (==) a b = (sig a == sig b)
    where 
      sig s = (st_vars s, st_mons s, [(pid, sigP p) | (pid,(name,p)) <- M.toList (st_procs s)] )
      sigP Finished = Nothing
      sigP (Running _ ip stack) = Just (ip, stack)

initState :: [(String,Value)] -> [String] -> Prog -> ProgramState
initState vars mons entryPoint = ProgramState {
    st_procs = M.fromList [(Pid 0, ("entry", Running entryPoint 0 []))],
    st_vars  = M.fromList vars,
    st_mons  = M.fromList [(m, MonFree) | m <- mons]
  }

compile :: [Insn] -> Prog
compile is = Prog {prog_insns = expandBlocks is}
  where
    expandBlocks = concatMap (\i -> case i of { Block is -> expandBlocks is ; j -> [j] })

newtype StepM a = StepM { runStep :: ProgramState -> [(ProgramState, a)] }
instance Monad StepM where
  fail       = error
  return a   = StepM $ \s -> [(s,a)]
  sa >>= fsb = StepM $ \s -> concat [ runStep (fsb a) s' | (s', a) <- runStep sa s ]

instance Functor StepM where
  f `fmap` s = s >>= return . f

stepState :: StepM ()
stepState = do
  st <- getState
  nondet [stepInsn (pid, prog_insns p !! ip) | (pid, (_,Running p ip _)) <- M.toList (st_procs st)]

stepInsn :: (Pid, Insn) -> StepM ()
stepInsn (pid, Label _ i) = do
  stepInsn (pid, i)
stepInsn (pid, NewVar s v) = do
  newVar s v
stepInsn (pid, NewMon m) = do
  newMon m
stepInsn (pid, Jmp lab) = do
  stepJmp lab pid
stepInsn (pid, JmpCond lab) = do
  v <- top pid
  case v of
    BoolValue True -> stepJmp lab pid
    BoolValue False -> stepNext pid
    _ -> fail $ "Non-boolean in JmpCond: "++show v
stepInsn (pid, Get s) = do
  v <- getVar s
  stepPush v pid
  stepNext pid
stepInsn (pid, Set s) = do
  v <- top pid
  setVar s v
  stepNext pid
stepInsn (pid, Arith op) = do
  stk <- getStack pid
  let stks' = op stk
  nondet [setStack s' pid >> stepNext pid | s' <- stks']
stepInsn (pid, Enter m) = do
  b <- tryEnterMon pid m
  if b
    then stepNext pid
    else return ()
stepInsn (pid, TryEnter m) = do
  f <- tryEnterMon pid m
  stepPush (BoolValue f) pid
  stepNext pid
stepInsn (pid, Leave m) = do
  s <- getMonState m
  case s of
    MonFree -> do
      fail ("Double leave "++m)
    MonOccupied p d -> do
      if p==pid 
        then setMonState m (if d==1 then MonFree else MonOccupied p (d-1))
        else fail "Mon left by non-owner"
stepInsn (pid, Spawn name p) = do
  pid' <- stepSpawn name p
  stepPush (PidValue pid') pid
  stepNext pid
stepAssert (pid, Assert s) = do
  b <- top pid
  case b of
    BoolValue True -> stepNext pid
    BoolValue False -> fail $ "Assertion failed: "++s
    _ -> fail $ "Non-boolean in assert: "++show b

getState :: StepM ProgramState
getState = StepM $ \st -> [(st,st)]

setState :: ProgramState -> StepM ()
setState st = StepM $ \_ -> [(st,())]

modifyState :: (ProgramState -> ProgramState) -> StepM ()
modifyState f = getState >>= setState . f

modifyProc :: (ProcState -> ProcState) -> Pid -> StepM ()
modifyProc f pid = modifyState $ \st -> st { st_procs = M.adjust (\(name,s) -> (name,f s)) pid (st_procs st) }

nondet :: [StepM a] -> StepM a
nondet ss = StepM $ \st -> concat [runStep s st | s <- ss]


newVar :: String -> Value -> StepM ()
newVar s v = modifyState $ \st -> st { st_vars = M.insert s v (st_vars st) }

newMon :: String -> StepM ()
newMon m = modifyState $ \st -> st { st_mons = M.insert m MonFree (st_mons st) }

stepNext :: Pid -> StepM ()
stepNext = modifyProc $ \(Running p ip s) -> 
  if ip < length (prog_insns p) - 1 then Running p (ip + 1) s else Finished

stepJmp :: String -> Pid -> StepM ()
stepJmp lab = modifyProc f
  where
    f (Running p ip s) = Running p ip' s where (Just ip') = findIndex (\insn -> case insn of {Label n _ -> n == lab ; _ -> False}) (prog_insns p)

stepPush :: Value -> Pid -> StepM ()
stepPush v = modifyProc $ \(Running p ip s) -> Running p ip (v:s)

getVar :: String -> StepM Value 
getVar s = ((M.! s) . st_vars) `fmap` getState

setVar :: String -> Value -> StepM ()
setVar s v = modifyState $ \st -> st { st_vars = M.insert s v (st_vars st) }

getStack :: Pid -> StepM [Value]
getStack pid = (proc_stack . snd . (M.! pid) . st_procs) `fmap` getState

setStack :: [Value] -> Pid -> StepM ()
setStack s' = modifyProc $ \(Running p ip s) -> Running p ip s'

top :: Pid -> StepM Value
top pid = head `fmap` getStack pid

getMonState :: String -> StepM MonState
getMonState mon = ((M.! mon) . st_mons) `fmap` getState

setMonState :: String -> MonState -> StepM ()
setMonState mon s = do
  st <- getState
  setState (st { st_mons = M.insert mon s (st_mons st) })

tryEnterMon :: Pid -> String -> StepM Bool
tryEnterMon pid mon = do
  s <- getMonState mon
  case s of
    MonFree -> do
      setMonState mon (MonOccupied { mon_owner = pid, mon_depth = 1 })
      return True
    MonOccupied p d -> do
      if p == pid 
        then do
          setMonState mon (MonOccupied { mon_owner = pid, mon_depth = d + 1 })
          return True
        else do
          return False

stepSpawn :: String -> Prog -> StepM Pid
stepSpawn name prog = do
  let ps = Running prog 0 []
  st <- getState
  let pid' = Pid (1 + maximum [i | Pid i <- M.keys (st_procs st)])
  setState $ st { st_procs = M.insert pid' (name, ps) (st_procs st) }
  return pid'
