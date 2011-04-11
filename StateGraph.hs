module StateGraph where

import Types
import Step

import qualified Data.Map as M
import Data.List
import qualified Data.Sequence as S

data StateGraph = StateGraph
  { 
    sg_index2node :: M.Map Int ProgramState,
    sg_node2index :: M.Map ProgramState Int,
    sg_node2out   :: M.Map Int (Maybe [Int]),
    sg_node2prev  :: M.Map Int Int
  }
  deriving (Show)

stateGraph :: ProgramState -> Int -> StateGraph
stateGraph init n = addEmptyEdgeLists $ 
                    buildGraph (S.singleton (n, init)) 
                    (StateGraph M.empty M.empty M.empty M.empty)
  where
    addEmptyEdgeLists g = g {sg_node2out = foldl' (\g i -> M.insertWith (\_ old -> old) i (Just []) g) (sg_node2out g) 
                                                  (M.keys (sg_index2node g))}

    buildGraph :: S.Seq (Int, ProgramState) -> StateGraph -> StateGraph
    buildGraph frontier g | S.null frontier = g
                          | otherwise       = buildGraph frontier'  $ g''
      where
        g' = foldr (addEdge node) g outs
        g'' = g' { sg_node2out = M.adjust (\old -> case old of {Just os -> Just os; Nothing -> Just []}) 
                                          (sg_node2index g' M.! node) 
                                          (sg_node2out g') }
        (remDepth,node) S.:< rest = S.viewl frontier
        outs = map fst (runStep stepState node)
        newOuts = filter (`M.notMember` sg_node2index g) outs
        frontier' = foldl (S.|>) rest [(remDepth-1, out) | out <- newOuts, remDepth > 0]

addEdge a b g@(StateGraph i2n n2i n2o n2p) = StateGraph i2n'' n2i'' n2o'' n2p'
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
    n2p' = if bIsNew then M.insert ib ia n2p else n2p


