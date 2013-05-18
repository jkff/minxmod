module ToDot where

import Types
import StateGraph

import qualified Data.Map as M
import Data.List

safeLookup :: (Ord k, Show k) => k -> M.Map k v -> String -> v
safeLookup k map mapName = case M.lookup k map of
  Nothing -> error $ "Key " ++ show k ++ " not in " ++ mapName
  Just v -> v

toDot :: StateGraph -> String
toDot g = "digraph g {\n" ++ 
          concat [show i ++ " [label = \"" ++ label i ++ "\"" ++ style ++ "]\n" 
                 | i <- M.keys (sg_index2node g),
                   let style = case safeLookup i (sg_node2out g) "sg_node2out" of
                                 Nothing -> ", style=dashed"
                                 _       -> ""] ++
          concat [show i ++ " -> " ++ show j ++ attr ++ "\n" 
                 | i <- M.keys (sg_index2node g),
                   j <- case safeLookup i (sg_node2out g) "sg_node2out" of {Just js -> js; Nothing -> []},
                   let attr = if M.findWithDefault (-1) j (sg_node2prev g) == i
                              then " [style=bold, color=red, weight=10]"
                              else " [constraint=false]"] ++
          concat [show a ++ " -> " ++ show b ++ " [label = \"" ++ events (a,b) ++ "\"]\n"
                 | v@(a,b) <- M.keys (sg_edges g)] ++
          "}"
  where
    label n = labelToDot $ safeLookup n (sg_index2node g) "index2node"
    events v = foldr (++) "\\n" $ map show $ safeLookup v (sg_edges g) "edges"

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


