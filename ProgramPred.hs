{-# LANGUAGE DataKinds, FlexibleInstances, PolyKinds, ScopedTypeVariables, FunctionalDependencies #-}

-- Simple syntax for predicates on ProgramState
module ProgramPred where

import qualified Data.Map as M
import Types
import CTL
import StateGraph

import GHC.TypeLits

data Mutex (s :: Symbol) = MutexFree
                         | MutexBusy { owner :: String, ownerPid :: Int, depth :: Int }
newtype IntVar (s :: Symbol) = IntVar { intValue :: Int }
newtype BoolVar (s :: Symbol) = BoolVar { boolValue :: Bool }

class HasValue a b | a -> b where
  value :: a -> b

instance HasValue (BoolVar s) Bool where
  value (BoolVar b) = b

instance HasValue (IntVar s) Int where
  value (IntVar b) = b

class ProgramPred f where
  apply :: ProgramState -> f -> Bool

instance ProgramPred Bool where
  apply _ = id

instance (SingI s, ProgramPred b) => ProgramPred (BoolVar s -> b) where
  apply ps f = case M.lookup name (st_vars ps) of
    Just (BoolValue x) -> apply ps (f (BoolVar x))
    Just x -> error $ "Not a BoolValue: " ++ show x
    _ -> error $ "Undefined variable: " ++ name
   where name = fromSing (sing :: Sing s)

instance (SingI s, ProgramPred b) => ProgramPred (IntVar s -> b) where
  apply ps f = case M.lookup name (st_vars ps) of
    Just (IntValue x) -> apply ps (f (IntVar x))
    Just x -> error $ "Not a IntValue: " ++ show x
    _ -> error $ "Undefined variable: " ++ name
   where name = fromSing (sing :: Sing s)

instance (SingI s, ProgramPred b) => ProgramPred (Mutex s -> b) where
  apply ps f = case M.lookup name (st_mons ps) of
    Just MonFree -> apply ps (f MutexFree)
    Just (MonOccupied (Pid pid) depth) -> apply ps (f (MutexBusy owner pid depth))
      where owner = fst (st_procs ps M.! Pid pid)
    _ -> error $ "Undefined mutex: " ++ name
   where name = fromSing (sing :: Sing s)

-- fun (m :: Mutex "m") (b :: BoolVar "b") = depth m > 0 || value b
-- CTLPred (`apply` (\(m :: Mutex "m") (b :: BoolVar "b") -> depth m > 0 || value b))
verifySG :: (ProgramPred p) => CTL p -> StateGraph -> [Int]
verifySG p g = M.keys $ M.filter (\b -> case b of {Known True -> True; _ -> False}) m
  where
    onIndex p = \i -> apply (sg_index2node g M.! i) p
    m = verify (fmap onIndex p) (sg_node2out g)

result = verifySG (CTLPred (\(m :: Mutex "m") (b :: BoolVar "b") -> depth m > 0 || value b))
                  (undefined :: StateGraph)
