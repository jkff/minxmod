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
toDot g = unlines [
  "digraph g {",
  "node [shape=box]",
  unlines [
    unwords [
       show i,
       "[label =", quote (label i), 
       style, "]"
       ]
    | i <- M.keys (sg_index2node g),
      let style = case safeLookup i (sg_node2out g) "sg_node2out" of
            Nothing -> ", style=dashed"
            _ -> ""],
  unlines [ 
    unwords [
       show i, "->", show j, attr 
    ] |
    i <- M.keys (sg_index2node g),
    j <- case safeLookup i (sg_node2out g) "sg_node2out" of {Just js -> js; Nothing -> []},
    let attr =
            if M.findWithDefault (-1) j (sg_node2prev g) == i
            then "[style=bold, color=red, weight=10]"
            else "[constraint=false]"],
  "}" ]
  where
    quote s = concat ["\"", s, "\""]
    label n = labelToDot $ safeLookup n (sg_index2node g) "index2node"
    events v = intercalate "\\n" $ map show $ safeLookup v (sg_edges g) "edges"

labelToDot (ProgramState {st_procs=p, st_vars=v, st_mons=m}) = intercalate "\\n" $ filter (not . null) [vars, mons, procs]
  where
    vars = if M.null v
           then ""
           else (join [v ++ "=" ++ show val | (v,val) <- M.toList v])
    mons = if M.null m
           then ""
           else ("M: "++join [m ++ ":" ++ show pid ++ "/" ++ show depth 
                             | (m, MonOccupied pid depth) <- M.toList m])
    procs = "P: "++join [show pid ++ "=" ++ n ++ ":" ++ show ip ++ show stk ++ 
                         maybe "" ("?"++) wm
                        | (pid,(n,Running {proc_ip=ip, proc_stack=stk, proc_waitedMon=wm})) <- M.toList p ]
    join = intercalate ","


