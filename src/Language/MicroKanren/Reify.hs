module Language.MicroKanren.Reify where

import Language.MicroKanren.Core

import Numeric.Natural

mKReify :: [[State a]] -> [Term a]
mKReify = fmap reifyState1stVar

reifyState1stVar :: [State a] -> Term a
reifyState1stVar []    = Variable 0
reifyState1stVar (a:_) = walk' v (reifyS v [])
    where
    v = walk' (Variable 0) a

reifyS :: Term a -> State a -> State a
reifyS v s = go $ walk v s
    where
    go (Variable v') = (v', reifyName $ length s):s
    go (Pair a d)    = reifyS d $ reifyS a s
    go _             = s

reifyName :: Int -> Term a
reifyName n = Variable $ fromIntegral n

walk' :: Term a -> State a -> Term a
walk' v s = go $ walk v s
    where
    go (Pair a d)    = Pair (walk' a s) (walk' d s)
    go v'            = v'

emptyState :: (State a, Natural)
emptyState = ([], 0)

callEmptyState :: Goal a -> [(State a, Natural)]
callEmptyState = ($ emptyState)

run :: Natural -> Goal a -> [Term a]
run n g = take (fromIntegral n) $ mKReify $ fmap (pure . fst) $ callEmptyState g
