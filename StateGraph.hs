{-# LANGUAGE RankNTypes #-}
module StateGraph where

import Types
import Step

import qualified Data.Map as M
import Data.List
import qualified Data.Sequence as S

type Event = Int

data StateGraph = StateGraph
  {
    sg_index2node :: M.Map Int ProgramState,
    sg_node2index :: M.Map ProgramState Int,
    -- Nothing means the node was not explored (reached state exploration depth limit)
    -- Just [] means the node WAS explored and was a dead end.
    sg_node2out   :: M.Map Int (Maybe [Int]),
    sg_node2in    :: M.Map Int [Int],
    sg_node2prev  :: M.Map Int Int,
    sg_edges      :: M.Map (Int, Int) [Event]
  }
  deriving (Show)

stateGraph :: ProgramState -> Int -> StateGraph
stateGraph init n = condenseGraph $ addEmptyEdgeLists $
                    buildGraph (S.singleton (n, init))
                    (StateGraph M.empty M.empty M.empty M.empty M.empty M.empty)
  where
    addEmptyEdgeLists g = g {sg_node2out = foldl' (\g i -> M.insertWith (\_ old -> old) i (Just []) g) (sg_node2out g)
                                                  (M.keys (sg_index2node g))}

    buildGraph :: S.Seq (Int, ProgramState) -> StateGraph -> StateGraph
    buildGraph frontier g | S.null frontier = g
                          | otherwise       = buildGraph frontier'  $ g''
      where
        addEdge' u v g = addEdge u v [0] g
        g' = foldr (addEdge' node) g outs
        g'' = g' { sg_node2out = M.adjust (\old -> case old of {Just os -> Just os; Nothing -> Just []})
                                          (sg_node2index g' M.! node)
                                          (sg_node2out g') }
        (remDepth,node) S.:< rest = S.viewl frontier
        outs = map fst (runStep stepState node)
        newOuts = filter (`M.notMember` sg_node2index g) outs
        frontier' = foldl (S.|>) rest [(remDepth-1, out) | out <- newOuts, remDepth > 0]

addEdge a b e g@(StateGraph i2n n2i n2o n2in n2p edges) = StateGraph i2n'' n2i'' n2o'' n2in' n2p' edges'
  where
    aIsNew = not (M.member a n2i)
    (ia,i2n',n2i')
      | aIsNew    = (M.size i2n, M.insert ia a i2n, M.insert a ia n2i)
      | otherwise = (n2i M.! a,  i2n,               n2i              )
    bIsNew = not (M.member b n2i')
    (ib,i2n'',n2i'')
      | bIsNew    = (M.size i2n', M.insert ib b i2n', M.insert b ib n2i')
      | otherwise = (n2i' M.! b,  i2n',               n2i'              )
    n2o' = M.alter addB ia n2o
    n2o'' = if bIsNew then M.insert ib Nothing n2o' else n2o'
    addB Nothing          = Just (Just [ib])
    addB (Just Nothing)   = Just (Just [ib])
    addB (Just (Just os)) = Just $ if ib `elem` os then Just os else Just (ib:os)
    n2in' = M.insertWith (++) ib [ia] n2in
    n2p' = if bIsNew then M.insert ib ia n2p else n2p
    edges' = M.insert (ia, ib) e edges

isStratifiable :: StateGraph -> Int -> Bool
isStratifiable (StateGraph i2n n2i n2o n2in n2p edges) i = singleIn && singleOut
  where singleIn = maybe 0 length (M.findWithDefault Nothing i n2o) < 2
        singleOut = length (M.findWithDefault [] i n2in) < 2

-- node -> farthest node reachable from this one via stratifiable nodes
stratifiedTarget :: StateGraph -> Int -> (Int, [Event])
stratifiedTarget g@(StateGraph i2n n2i n2o n2in n2p edges) i = case (isStratifiable g i, M.findWithDefault Nothing i n2o) of
  (True, Just [n]) -> (target,  (M.findWithDefault [] (i, n) edges) ++ events)
    where
      (target, events) = stratifiedTarget g n
  _ -> (i, [])

-- node -> farthest node reachable from this one via stratifiable back edges
stratifiedSource :: StateGraph -> Int -> Int
stratifiedSource g@(StateGraph i2n n2i n2o n2in n2p edges) i = case (isStratifiable g i, M.findWithDefault [] i n2in) of
  (True, [n]) -> stratifiedSource g n
  _ -> i

condenseGraph :: StateGraph -> StateGraph
condenseGraph g@(StateGraph i2n n2i n2o n2in n2p edges) = StateGraph i2n' n2i' n2o' n2in' n2p' edges
  where
    strat = fst . stratifiedTarget g
    isStraight k = k == strat k
    cleanup :: forall v . M.Map Int v -> M.Map Int v
    cleanup m = M.filterWithKey (\k _ -> isStraight k) m
    i2n' = cleanup i2n
    n2i' = M.filter (`M.member` i2n') $ M.map strat n2i
    n2o' = cleanup $ M.mapWithKey (\i os -> fmap (filter (/= i) . nub . map strat) os) n2o
    n2in' = cleanup $ M.mapWithKey (\i os -> filter (/= i) . nub . map strat $ os) n2in
    n2p' = cleanup $ M.map (stratifiedSource g) n2p
    --TODO filter here

