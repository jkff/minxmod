{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses, FlexibleInstances #-}
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
  deriving (Show, Functor)

toBasis :: CTL p -> CTL p
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

data Tribool = Known Bool | Unknown deriving (Eq, Show)
instance Boolean Tribool where
  true = Known True
  false = Known False
  notB (Known b) = Known (not b)
  notB Unknown = Unknown

  Known True  ||* _          = Known True
  Known False ||* x          = x
  Unknown     ||* Known True = Known True
  Unknown     ||* _          = Unknown

  Known False &&* _           = Known False
  Known True  &&* x           = x
  Unknown     &&* Known False = Known False
  Unknown     &&* _           = Unknown

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f a0 = let as = iterate f a0 in fst (head [(x,y) | (x,y) <- zip as (tail as), x == y])

anyB f xs = foldr (||*) false (map f xs)

verify :: (Ord s) => CTL (s -> Bool) -> M.Map s (Maybe [s]) -> M.Map s Tribool
verify p g = verifyB (toBasis p) g

verifyB :: (Ord s) => CTL (s -> Bool) -> M.Map s (Maybe [s]) -> M.Map s Tribool
verifyB CTLFalse g = fmap (const false) g
verifyB (CTLPred p) g = M.mapWithKey (\s _ -> Known (p s)) g
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

ex :: (Ord s) => M.Map s (Maybe [s]) -> M.Map s Tribool -> M.Map s Tribool
ex g phi = M.mapWithKey (\s _ -> case g M.! s of { Nothing -> Unknown; Just outs -> anyB (phi M.!) outs }) g
