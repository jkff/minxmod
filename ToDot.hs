module ToDot where

import Types
import StateGraph

import qualified Data.Map as M
import Data.List
import Data.Maybe

safeLookup :: (Ord k, Show k) => k -> M.Map k v -> String -> v
safeLookup k map mapName = case M.lookup k map of
  Nothing -> error $ "Key " ++ show k ++ " not in " ++ mapName
  Just v -> v

toDot :: StateGraph -> String
toDot g = unlines [
  "digraph g {",
  "node [shape=record]",
  unlines $ do
    i <- M.keys (sg_index2node g)
    let nodeStyle =
          maybe ", style=dashed" (const "") $
          safeLookup i (sg_node2out g) "sg_node2out"
    let neighbours =
          fromMaybe [] $ safeLookup i (sg_node2out g) "sg_node2out"
    [ unwords [ show i, "[label =", quote (label i neighbours), nodeStyle, "]" ],
      unlines $ do
        j <- neighbours
        let edgeLabel = quote (events (i, j))
        let edgeStyle =
              if M.findWithDefault (-1) j (sg_node2prev g) == i
              then "[style=bold, color=red, weight=10]"
              else "[constraint=false]" 
        return $ unwords [show i ++ ":f" ++ show j, "->", show j, edgeStyle]
      ],
  "}" ]
  where
    quote = wrap "\"" "\""
    wrap a b s = concat [a, s, b]
    label n ns =  wrap "{" "}" $ intercalate "|" $
                 labelToDot (safeLookup n (sg_index2node g) "index2node")
                 : map (\j -> concat ["<f", show j, ">", events (n,j)]) ns
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


