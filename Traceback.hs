module Traceback where

import Data.List
import qualified Data.Map as M
import Data.Functor ((<$>))

import Step
import Types
import StateGraph

data SimpleTraceEvent = LocalSpan { ste_pid :: Pid, ste_insns :: [(Int, Insn, String)] }
                      | ContextSwitch { ste_newPid :: Pid }
                      deriving (Show)

traceback :: StateGraph -> Int -> [Event]
traceback g i = case M.lookup i (sg_node2prev g) of
  Just p  -> traceback g p ++ sg_edges g M.! (p, i)
  Nothing -> []

simplifyTraceback :: [Event] -> [SimpleTraceEvent]
simplifyTraceback (e@(InsnExecuted pid _ _ _):es) = simplify' pid [] (e:es)
  where
    simplify' pid span [] = [LocalSpan pid (reverse span)]
    simplify' pid span (e@(InsnExecuted pid' ip insn comment):es)
      | pid == pid' = simplify' pid ((ip, insn, comment):span) es
      | otherwise   = LocalSpan pid (reverse span) : ContextSwitch pid' : simplify' pid' [] (e:es)

printTraceback :: StateGraph -> Int -> String
printTraceback g i = intercalate "\n" $ map printEntry st
  where
    st = simplifyTraceback (traceback g i)
    printEntry (ContextSwitch p) = "---- switch to " ++ show p ++ " ----"
    printEntry (LocalSpan pid insns) = intercalate "\n" $ map printInsn insns
      where
        printInsn (ip, insn, comment) = show pid ++ "\t| " ++ show ip ++ "\t| " ++ show insn ++ " ; " ++ comment
