module ToDot where

import Types
import StateGraph

import qualified Data.Map as M
import Data.List

toDot :: StateGraph -> String
toDot g = "digraph g {\n" ++ 
          concat [show i ++ " [label = \"" ++ label i ++ "\"" ++ style ++ "]\n" 
                 | i <- [0..n-1],
                   let style = if M.findWithDefault False i (sg_node2open g)
                               then ", style=dashed"
                               else ""] ++
          concat [show i ++ " -> " ++ show j ++ attr ++ "\n" 
                 | i <- [0..n-1], i `M.member` sg_node2out g, 
                   j <- sg_node2out g M.! i,
                   let attr = if M.findWithDefault (-1) j (sg_node2prev g) == i
                              then " [style=bold, color=red, weight=10]"
                              else " [constraint=false]"] ++
          "}"
  where
    n = M.size (sg_index2node g)
    label n = labelToDot (sg_index2node g M.! n)

labelToDot (ProgramState {st_procs=p, st_vars=v, st_mons=m}) =
  "V: "++join [v ++ ":" ++ show val 
              | (v,val) <- M.toList v]++"\\n"++
  "M: "++join [m ++ ":" ++ show pid ++ "/" ++ show depth 
              | (m, MonOccupied pid depth) <- M.toList m]++"\\n"++
  "P: "++join [show pid ++ "=" ++ n ++ ":" ++ show ip ++ show stk ++ 
                 maybe "" ("?"++) wm
              | (pid,(n,Running {proc_ip=ip, proc_stack=stk, proc_waitedMon=wm})) <- M.toList p ]
  where
    join = concat . intersperse ","


