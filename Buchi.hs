module Buchi where

import Data.Graph
import Data.Tree
import Control.Arrow
import Data.List
import qualified Data.Map as M
import qualified Data.Array as A

import Debug.Trace

-- Non-deterministic Buchi automaton
data NBA e = NBA 
  {
    nba_outs :: M.Map Int [(e, Int)],
    nba_accept :: M.Map Int Bool,
    nba_initial :: M.Map Int Bool
  }

exampleNBA = NBA {
        nba_outs = M.fromList [
            (0, aug [5]),
            (1, aug [0,2,5]),
            (2, aug [6]),
            (3, aug [1,6]),
            (4, aug [5]),
            (5, aug [2,4]),
            (6, aug [4])
            ],
        nba_accept  = M.fromList $ zip [0..] [False,False,False,False,False,False,True],
        nba_initial = M.fromList $ zip [0..] [False,True,False,False,False,False,False]
    }
  where aug = zip (repeat ())

instance (Show a) => Show (SCC a) where
  show (AcyclicSCC a) = "AcyclicSCC " ++show a
  show (CyclicSCC as) = "CyclicSCC " ++show as

-- | Returns: (non-cyclic prefix, cycle body)
-- Find SCC; mark those cyclic SCC that contain an accepting state; 
-- check if one is reachable from the SCC containing the initial state.
findAcceptingCycle :: NBA e -> Maybe ([Int], [Int])
findAcceptingCycle (NBA {nba_outs = outs, nba_accept = accept, nba_initial = init}) = case rpaths of
    [] -> Nothing
    p:ps -> Just $ unfoldInit2AcceptPathThroughScc p
  where
    sccs = zip [0..] (findSCC g)
    cyclicAcceptSccInds = [i | (i, c@CyclicSCC{}) <- sccs, any isAcceptNode (flattenSCC c)]
    initSccInds         = [i | (i, c            ) <- sccs, any isInitNode   (flattenSCC c)]
    rpaths = findRpathsBetween initSccInds cyclicAcceptSccInds sccGraphArr

    g = fmap (map snd) outs
    isInitNode   i = init   M.! i
    isAcceptNode i = accept M.! i
    sccGraph = factorGraph componentF g
    sccGraphArr = toGraph sccGraph
    node2scc = M.fromList $ concatMap (\(i,c) -> map (\j -> (j,i)) (flattenSCC c)) sccs
    scc2nodes = M.fromList $ map (\(i, c) -> (i, flattenSCC c)) sccs
    componentF i = node2scc M.! i
    findRpathsBetween srcInds dstInds g = 
        filter ((`M.member` dstIndsSet) . head) $
        concatMap (flatten . pathTree) $ 
        dfs g $ srcInds
      where
        dstIndsSet :: M.Map Int ()
        dstIndsSet = M.fromList $ zip dstInds (repeat ())
    
    unfoldInit2AcceptPathThroughScc p = (stem, cycle)
      -- p is a reversed path through sccs, first element of p is 
      -- an accepting cyclic scc, last is an initial scc.
      -- we should take some accepting node and some initial node
      -- and find a path between them in the original graph.
      where
        (acceptScc, initScc) = (head p, last p)
        (acceptNodes, initNodes) = (filter isAcceptNode (scc2nodes M.! acceptScc),
                                    filter isInitNode   (scc2nodes M.! initScc))
        ps = findRpathsBetween initNodes acceptNodes (toGraph g)
        stem = reverse $ head ps
        cycle = findCycle (scc2nodes M.! acceptScc) (head acceptNodes) g

toGraph :: M.Map Int [Int] -> Graph
toGraph g = A.array (0, maximum (M.keys g)) $ M.toList g

pathTree :: Tree Int -> Tree [Int]
pathTree = pathTree' []
  where pathTree' p (Node label kids) = let p'=label:p in Node p' (map (pathTree' p') kids)

findCycle :: [Int] -> Int -> M.Map Int [Int] -> [Int]
findCycle vs v g = reverse (head $ filter (\(n:ns) -> v `elem` g' M.! n) ps)
  where
    g' = restrict vs g
    ps = concatMap (flatten . pathTree) $ dfs (toGraph g') [v]
    
restrict :: [Int] -> M.Map Int [Int] -> M.Map Int [Int]
restrict vs g = M.fromList $ map (\i -> (i, filter (`M.member` vsSet) (g M.! i))) vs
  where
    vsSet = M.fromList $ zip vs (repeat ())

factorGraph :: (Int -> Int) -> M.Map Int [Int] -> M.Map Int [Int]
factorGraph componentF g = fmap (distinct . map componentF) $ M.mapKeysWith (++) componentF g 
  where
    distinct = M.keys . M.fromList . (`zip` (repeat ()))

findSCC :: M.Map Int [Int] -> [SCC Int]
findSCC g = stronglyConnComp $ map (\(i,os) -> (i,i,os)) (M.toList g)
