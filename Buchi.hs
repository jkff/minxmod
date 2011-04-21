module Buchi where

import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS
import Data.Maybe
import Data.Ord

-- Non-deterministic Buchi automaton
data NBA e = NBA 
  {
    nba_graph :: Gr (Bool, Bool) e -- (accept, init)
  }

data SCC a = AcyclicSCC a | CyclicSCC [a] deriving (Show)

exampleNBA = NBA {
    nba_graph = mkGraph 
        (zip [0..6] $ zip
            [False,False,False,False,False,False,True]
            [False,True,False,False,False,False,False])
        (concatMap (\(i,os) -> [(i, o, ()) | o <- os])
           [(0, [5]),
            (1, [0,2,5]),
            (2, [6]),
            (3, [1,6]),
            (4, [5]),
            (5, [2,4]),
            (6, [4])])
    }

-- | Returns: (non-cyclic prefix (stem), cycle body (loop))
findAcceptingCycle :: NBA e -> Maybe ([Int], [Int])
findAcceptingCycle (NBA {nba_graph = g}) = case stem of [] -> Nothing; _ -> Just (stem, cycle)
  where
    initNodes           = [j | (j, (_,True)) <- labNodes g]
    acceptsInCyclicSCCs = [j | CyclicSCC cs <- findSCC g, j <- cs, isAcceptNode j]
    isAcceptNode        = fst . fromJust . lab g

    stem  = shortestPathBetween initNodes acceptsInCyclicSCCs g
    cycle = shortestCycleThrough (last stem) g

    shortestCycleThrough v g = head [p | p <- bft v g, v `elem` suc g (head p)]

    shortestPathBetween froms tos g = init . tail $ esp fromAux toAux g'
      where
        [fromAux, toAux] = newNodes 2 g
        g' = insEdges [(fromAux,  i,     undefined) | i <- froms] $ 
             insEdges [(i,        toAux, undefined) | i <- tos  ] $
             insNodes [(fromAux,  undefined), (toAux, undefined)] $
             g

findSCC :: Graph gr => gr a b -> [SCC Node]
findSCC g = map toSCC . scc $ g
  where toSCC [n] | n `elem` suc g n = CyclicSCC [n]
                  | otherwise        = AcyclicSCC n
        toSCC ns  = CyclicSCC ns
