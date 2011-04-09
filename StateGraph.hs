module StateGraph where

import Types
import Step

import qualified Data.Map as M
import qualified Data.Sequence as S

data StateGraph = StateGraph
  { 
    sg_index2node :: M.Map Int ProgramState,
    sg_node2index :: M.Map ProgramState Int,
    sg_node2out   :: M.Map Int [Int],
    sg_node2prev  :: M.Map Int Int,
    sg_node2open  :: M.Map Int Bool
  }
  deriving (Show)

stateGraph :: ProgramState -> Int -> StateGraph
stateGraph init n = buildGraph (S.singleton (n, init)) (StateGraph M.empty M.empty M.empty M.empty (M.fromList [(0,True)]))
  where
    buildGraph :: S.Seq (Int, ProgramState) -> StateGraph -> StateGraph
    buildGraph frontier g | S.null frontier = g
                          | otherwise       = buildGraph frontier' g''
      where
        g' = foldr (addEdge node) g outs
        opennessUpdate = map (\(n,b) -> (sg_node2index g' M.! n, b)) $ 
                         (node,False):[(no,True) | no <- newOuts]
        g'' = g' { sg_node2open = foldr (uncurry M.insert) (sg_node2open g') opennessUpdate }
        (remDepth,node) S.:< rest = S.viewl frontier
        outs = map fst (runStep stepState node)
        newOuts = filter (`M.notMember` sg_node2index g) outs
        frontier' = foldl (S.|>) rest [(remDepth-1, out) | out <- newOuts, remDepth > 0]

addEdge a b g@(StateGraph i2n n2i n2o n2p n2open) = StateGraph i2n'' n2i'' n2o' n2p' n2open
  where
    (ia,i2n',n2i') 
      | M.member a n2i = (n2i M.! a,  i2n,               n2i              )
      | otherwise      = (M.size i2n, M.insert ia a i2n, M.insert a ia n2i)
    (ib,i2n'',n2i'') 
      | M.member b n2i' = (n2i' M.! b,  i2n',               n2i'              )
      | otherwise       = (M.size i2n', M.insert ib b i2n', M.insert b ib n2i')
    n2o' = M.alter addB ia n2o
    addB Nothing = Just [ib]
    addB (Just os) = if ib `elem` os then Just os else Just (ib:os)
    n2p' = if M.member ib n2p then n2p else M.insert ib ia n2p


