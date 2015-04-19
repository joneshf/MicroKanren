module Language.MicroKanren.Core where

import Control.Monad ((>=>), liftM2)

import Numeric.Natural (Natural)

newtype Var = Var { unVar :: Natural } deriving (Eq, Num, Show)

data Term a = Variable Var | Term a | Pair (Term a) (Term a) deriving (Eq, Show)

type State a = [(Var, Term a)]

type Goal a = (State a, Natural) -> [(State a, Natural)]

walk :: Term a -> State a -> Term a
walk u@(Variable v) s = maybe u (`walk` s) $ lookup v s
walk u              _ = u

extS :: Var -> Term a -> State a -> State a
extS x v s = (x, v):s

unify :: Eq a => Term a -> Term a -> State a -> State a
unify u v s = go (walk u s) (walk v s)
    where
    go (Variable u') (Variable v') | u' == v' = s
    go (Variable u') v'                       = extS u' v' s
    go u'            (Variable v')            = extS v' u' s
    go (Pair ua ud)  (Pair va vd)             = unify ud vd . unify ua va $ s
    go u'            v'            | u' == v' = s
    go _             _                        = []

(===) :: Eq a => Term a -> Term a -> Goal a
u === v = \(a, d) -> pure (unify u v a, d)

callFresh :: (Var -> Goal a) -> Goal a
callFresh f (a, d) = f (Var d) (a, d + 1)

conj, disj :: Goal a -> Goal a -> Goal a
conj = (>=>)
disj = liftM2 plus

plus :: [a] -> [a] -> [a]
plus []     ys      = ys
plus (x:xs) ~(y:ys) = x:y:(xs `plus` ys)
