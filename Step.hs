module Step where

import Types

import Data.List
import Control.Monad
import qualified Data.Map as M

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
  -- TODO: "Not runnable" != "runnable but next state is the same".
  let isRunnable pid Finished = False;
      isRunnable pid Running{ proc_waitedMon = Nothing } = True;
      isRunnable pid Running{ proc_waitedMon = Just m  } = case getMonState m st of MonFree -> True; _ -> False
  let runnableProcs = [x | x@(pid, (_,pst)) <- M.toList (st_procs st), isRunnable pid pst]
  nondet [stepInsn (pid, prog_insns p !! ip) | (pid, (_,Running p ip _ _)) <- runnableProcs]

stepInsn :: (Pid, Insn) -> StepM ()
stepInsn (pid, Label _ i) = do
  stepInsn (pid, i)
stepInsn (pid, Jmp lab) = do
  stepJmp lab pid
stepInsn (pid, JmpCond lab) = do
  v <- stepPop pid
  case v of
    BoolValue True -> stepJmp lab pid
    BoolValue False -> stepNext pid
    _ -> fail $ "Non-boolean in JmpCond: "++show v
stepInsn (pid, Get s) = do
  v <- getVar s
  stepPush v pid
  stepNext pid
stepInsn (pid, Set s) = do
  v <- stepPop pid
  setVar s v
  stepNext pid
stepInsn (pid, Arith op) = do
  stk <- getStack pid
  let stks' = op stk
  nondet [setStack s' pid >> stepNext pid | s' <- stks']
stepInsn (pid, Enter m) = do
  b <- tryEnterMon pid m
  if b
    then do
      clearWaitedMon m pid
      stepNext pid
    else do
      setWaitedMon m pid
stepInsn (pid, TryEnter m) = do
  f <- tryEnterMon pid m
  stepPush (BoolValue f) pid
  stepNext pid
stepInsn (pid, Leave m) = do
  s <- getMonStateM m
  case s of
    MonFree -> do
      fail ("Double leave "++m)
    MonOccupied p d -> do
      if p==pid 
        then do
          setMonState m (if d==1 then MonFree else MonOccupied p (d-1))
          stepNext pid
        else fail "Mon left by non-owner"
stepInsn (pid, Spawn name p) = do
  pid' <- stepSpawn name p
  stepPush (PidValue pid') pid
  stepNext pid
stepInsn (pid, Assert s) = do
  b <- stepPop pid
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


stepNext :: Pid -> StepM ()
stepNext = modifyProc $ \r@Running {proc_prog=p, proc_ip=ip, proc_waitedMon=Nothing} -> 
  if ip < length (prog_insns p) - 1 then r{proc_ip=ip+1} else Finished

stepJmp :: String -> Pid -> StepM ()
stepJmp lab = modifyProc f
  where
    f r@Running{proc_waitedMon=Nothing} = r{ proc_ip = ip' }
      where 
        p = proc_prog r
        (Just ip') = findIndex (\insn -> case insn of {Label n _ -> n == lab ; _ -> False}) (prog_insns p)

stepPush :: Value -> Pid -> StepM ()
stepPush v = modifyProc $ \r@Running{proc_stack=s} -> r{proc_stack=v:s}

getVar :: String -> StepM Value 
getVar s = ((M.! s) . st_vars) `fmap` getState

setVar :: String -> Value -> StepM ()
setVar s v = modifyState $ \st -> st { st_vars = M.insert s v (st_vars st) }

getStack :: Pid -> StepM [Value]
getStack pid = (proc_stack . snd . (M.! pid) . st_procs) `fmap` getState

setStack :: [Value] -> Pid -> StepM ()
setStack s' = modifyProc $ \r -> r{proc_stack=s'}

stepPop :: Pid -> StepM Value
stepPop pid = do
  (h:t) <- getStack pid
  setStack t pid
  return h

getMonState :: String -> ProgramState -> MonState
getMonState mon = ((M.! mon) . st_mons)

getMonStateM :: String -> StepM MonState
getMonStateM mon = getMonState mon `fmap` getState

setMonState :: String -> MonState -> StepM ()
setMonState mon s = do
  st <- getState
  setState (st { st_mons = M.insert mon s (st_mons st) })

tryEnterMon :: Pid -> String -> StepM Bool
tryEnterMon pid mon = do
  s <- getMonStateM mon
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

setWaitedMon :: String -> Pid -> StepM ()
setWaitedMon mon = modifyProc $ \p -> p{proc_waitedMon = Just mon}

clearWaitedMon :: String -> Pid -> StepM ()
clearWaitedMon mon = modifyProc $ \p -> p{proc_waitedMon = Nothing}

stepSpawn :: String -> Prog -> StepM Pid
stepSpawn name prog = do
  let ps = Running {proc_prog=prog, proc_ip=0, proc_stack=[], proc_waitedMon=Nothing}
  st <- getState
  let pid' = Pid (1 + maximum [i | Pid i <- M.keys (st_procs st)])
  setState $ st { st_procs = M.insert pid' (name, ps) (st_procs st) }
  return pid'
