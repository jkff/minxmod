module Traceback where

import Data.List (unfoldr)
import qualified Data.Map as M
import Data.Functor ((<$>))

import Types
import StateGraph

data Event = InsnExecuted { e_pid :: Pid, e_ip :: Int, e_insn :: Insn } deriving (Show)

data SimpleTraceEvent = LocalSpan { ste_pid :: Pid, ste_insns :: [(Int, Insn)] }
                      | ContextSwitch { ste_newPid :: Pid }
                      deriving (Show)

sg_node2prev' :: StateGraph -> M.Map Int (Int, Event)
sg_node2prev' = undefined

traceback :: StateGraph -> Int -> [Event]
traceback g i = unfoldr source i
  where source i = genEvent <$> M.lookup i (sg_node2prev' g)
        genEvent (p, e) = (e, p)

simplifyTraceback :: [Event] -> [SimpleTraceEvent]
simplifyTraceback (e@(InsnExecuted pid ip insn):es) = simplify' pid [] (e:es)
  where
    simplify' pid span [] = [LocalSpan pid (reverse span)]
    simplify' pid span (e@(InsnExecuted pid' ip insn):es)
      | pid == pid' = simplify' pid ((ip, insn):span) es
      | otherwise   = LocalSpan pid (reverse span) : ContextSwitch pid' : simplify' pid' [] (e:es)
