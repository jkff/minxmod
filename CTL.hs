module CTL where

import qualified Data.Map as M
import Data.Boolean

data CTL p =
    CTLFalse
  | CTLTrue
  | CTLPred p
  | CTLNeg (CTL p)
  | CTLOr (CTL p) (CTL p)
  | CTLAnd (CTL p) (CTL p)
  | CTLImpl (CTL p) (CTL p)
  | CTLEquiv (CTL p) (CTL p)
  | CTLAllNext (CTL p)
  | CTLExistsNext (CTL p)
  | CTLAllFinally (CTL p)
  | CTLExistsFinally (CTL p)
  | CTLAllGlobally (CTL p)
  | CTLExistsGlobally (CTL p)
  | CTLAllUntil (CTL p) (CTL p)
  | CTLExistsUntil (CTL p) (CTL p)
  deriving (Show)

toBasis :: (Boolean p) => CTL p -> CTL p
toBasis CTLFalse = CTLFalse
toBasis (CTLPred p) = CTLPred p
toBasis (CTLNeg p) = CTLNeg (toBasis p)
toBasis (CTLOr p q) = CTLOr (toBasis p) (toBasis q)
toBasis (CTLExistsNext p) = CTLExistsNext (toBasis p)
toBasis (CTLExistsGlobally p) = CTLExistsGlobally (toBasis p)
toBasis (CTLExistsUntil p q) = CTLExistsUntil (toBasis p) (toBasis q)
toBasis CTLTrue = CTLNeg CTLFalse
toBasis (CTLAnd p q) = toBasis (CTLNeg (CTLOr (CTLNeg p) (CTLNeg q)))
toBasis (CTLImpl p q) = toBasis (CTLOr q (CTLNeg p))
toBasis (CTLEquiv p q) = toBasis (CTLOr (CTLAnd p q) (CTLAnd (CTLNeg p) (CTLNeg q)))
toBasis (CTLAllNext p) = toBasis (CTLNeg (CTLExistsNext (CTLNeg p)))
toBasis (CTLAllFinally p) = toBasis (CTLNeg (CTLExistsGlobally (CTLNeg p)))
toBasis (CTLExistsFinally p) = toBasis (CTLExistsUntil CTLTrue p)
toBasis (CTLAllGlobally p) = toBasis (CTLNeg (CTLExistsUntil CTLTrue (CTLNeg p)))
toBasis (CTLAllUntil p q) = toBasis (CTLOr (CTLNeg (CTLExistsUntil (CTLNeg q) (CTLNeg (CTLOr p q)))) 
                                           (CTLExistsGlobally (CTLNeg q)))

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f a0 = let as = iterate f a0 in fst (head [(x,y) | (x,y) <- zip as (tail as), x == y])

anyB f xs = foldr (||*) false (map f xs)

verify :: (Ord s, Boolean b, Eq b) => CTL (s -> b) -> M.Map s [s] -> M.Map s b
verify p g = verifyB (toBasis p) g

verifyB :: (Ord s, Boolean b, Eq b) => CTL (s -> b) -> M.Map s [s] -> M.Map s b
verifyB CTLFalse g = fmap (const false) g
verifyB (CTLPred p) g = M.mapWithKey (\s _ -> p s) g
verifyB (CTLOr p q) g = M.unionWith (||*) (verifyB p g) (verifyB q g)
verifyB (CTLNeg p) g = fmap notB (verifyB p g)
verifyB (CTLExistsGlobally p) g = fixpoint step (fmap (const false) g)
  where phi = verifyB p g
        step = M.unionWith (||*) phi . ex g
verifyB (CTLExistsNext p) g = ex g (verifyB p g)
  where phi = verifyB p g
verifyB (CTLExistsUntil p q) g = fixpoint step (fmap (const false) g)
  where phi = verifyB p g
        psi = verifyB q g
        step = M.unionWith (||*) psi . M.unionWith (&&*) phi . ex g

ex g phi = M.mapWithKey (\s _ -> anyB (phi M.!) (g M.! s)) g
