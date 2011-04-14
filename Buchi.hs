module Buchi where

import Data.Graph
import Data.Tree
import Control.Arrow
import Data.List
import qualified Data.Map as M
import qualified Data.Array as A

-- Non-deterministic Buchi automaton
data NBA e = NBA 
  {
    nba_outs :: M.Map Int [(e, Int)],
    nba_accept :: M.Map Int Bool,
    nba_initial :: M.Map Int Bool
  }

-- | Returns: (non-cyclic prefix, cycle body)
-- Find SCC; mark those SCC that contain an accepting state; check if one is reachable from the SCC containing the initial state.
findAcceptingCycle :: NBA e -> Maybe ([Int], [Int])
findAcceptingCycle (NBA {nba_outs = outs, nba_accept = accept, nba_initial = init}) = case pathsToAcceptFromInit of
    []  -> Nothing
    p:_ -> Just (unfoldPathThroughSCC p)
  where
    initStates = [i | (i, ((_,True),_)) <- M.toList sccGraph]
    pathsFromInit = concatMap (flatten . pathTree []) (dfs sccGraphArr initStates)
    pathsToAcceptFromInit = [p | p@(i:_) <- pathsFromInit, case sccGraph M.! i of { ((accept,_),_) -> accept }]
    sccGraphArr = A.accumArray (flip (:)) [] (0, M.size sccGraph) [(i, o) | (i,(_,os)) <- M.toList sccGraph, o <- os]
    sccGraph = factorGraph ((or *** or) . unzip) componentF ((accept M.!) &&& (init M.!)) structure
    structure = fmap (map snd) outs
    componentMap = M.fromList (components2assocs (findSCC structure))
    components2assocs = concatMap (\(ci,contents) -> zip contents (repeat ci)) . zip [0..]
    componentF = (componentMap M.!)
    pathTree p (Node a kids) = Node p' (map (pathTree p') kids) where p' = a:p
    unfoldPathThroughSCC p = (pathFromInitToAccept, acceptingCycle)
      where
        pathFromInitToAccept = undefined
        acceptingCycle = undefined

factorGraph :: ([a] -> b) -> (Int -> Int) -> (Int -> a) -> M.Map Int [Int] -> M.Map Int (b, [Int])
factorGraph reduceMark componentF mark g = mergeWith (,) newMarks newStructure
  where
    mergeWith f ma mb = M.fromList [(k,f (ma M.! k) (mb M.! k)) | k <- M.keys ma]
    newMarks = fmap reduceMark $ M.fromListWith (++) [(componentF i,[mark i]) | i <- M.keys g]
    newStructure = fmap (distinct . map componentF) $ M.mapKeysWith (++) componentF g 
    distinct = M.keys . M.fromList . (`zip` (repeat ()))

findSCC :: M.Map Int [Int] -> [[Int]]
findSCC g = map flattenSCC $ stronglyConnComp $ map (\(i,os) -> (i,i,os)) (M.toList g)
