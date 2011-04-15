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
findAcceptingCycle (NBA {nba_outs = outs, nba_accept = accept, nba_initial = init}) = case rpathsInitToAccept of
    []  -> Nothing
    p:_ -> Just (unfoldPathThroughSCC p)
  where
    -- scc graph with marked accept/init nodes...
    structure = fmap (map snd) outs
    sccs = zip [0..] (findSCC structure)
    node2scc = M.fromList (components2assocs sccs)
    scc2nodes = M.fromList sccs
    components2assocs = concatMap (\(ci,contents) -> zip contents (repeat ci))
    componentF = (node2scc M.!)
    sccGraph = factorGraph ((or *** or) . unzip) componentF (isInitNode &&& isAcceptNode) structure
    sccGraphArr = toGraph sccGraph

    toGraph g = A.accumArray (flip (:)) [] (0, M.size g) [(i, o) | (i,(_,os)) <- M.toList g, o <- os]

    isInitScc    i = case sccGraph M.! i of { ((_,  init),_) -> init   }
    isAcceptScc  i = case sccGraph M.! i of { ((accept,_),_) -> accept }
    isInitNode   i = accept M.! i
    isAcceptNode i = init   M.! i
    
    -- reversed paths from an initial scc to an accepting scc
    rpathsInitToAccept = filter (isAcceptScc . head) $ 
                         concatMap (flatten . pathTree []) . dfs sccGraphArr $
                         filter isInitScc (M.keys sccGraph)

    pathTree p (Node a kids) = Node p' (map (pathTree p') kids) where p' = a:p

    -- given an init->accept path through scc, generate a path from an init node
    -- to an accept node and the cycle within the accept scc through that node
    unfoldRPathI2AThroughSCC rpI2A = (pathFromAcceptToInit, acceptingCycle)
      where
        (acceptScc, initScc) = (first sccPath, last sccPath)
        (acceptNode, initNode) = (head $ filter isAcceptNode $ scc2nodes M.! acceptScc, 
                                  head $ filter isInitNode   $ scc2nodes M.! initScc)
        pathsToInitNode = 
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
