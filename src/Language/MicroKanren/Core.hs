module Language.MicroKanren.Core where

import Control.Monad ((>=>), liftM2, mzero)

import Numeric.Natural (Natural)

newtype Var = Var { unVar :: Natural } deriving (Eq, Num, Show)

data Term a = Variable Var | Term a | Pair (Term a) (Term a) deriving (Eq, Show)

type Unify a = Maybe (State a)

type State a = [(Var, Term a)]

type Goal a = (State a, Natural) -> [(State a, Natural)]

walk :: Term a -> State a -> Term a
walk u@(Variable v) s = maybe u (`walk` s) $ lookup v s
walk u              _ = u

extS :: Var -> Term a -> State a -> State a
extS x v s = (x, v):s

unify :: Eq a => Term a -> Term a -> State a -> Unify a
unify u v s = go (walk u s) (walk v s)
    where
    go (Variable u') (Variable v') | u' == v' = Just $ s
    go (Variable u') v'                       = Just $ extS u' v' s
    go u'            (Variable v')            = Just $ extS v' u' s
    go (Pair ua ud)  (Pair va vd)             = (unify ua va >=> unify ud vd) s
    go u'            v'            | u' == v' = Just s
    go _             _                        = Nothing

(===) :: Eq a => Term a -> Term a -> Goal a
u === v = \(a, d) -> maybe mzero (pure . flip (,) d) $ unify u v a

callFresh :: (Var -> Goal a) -> Goal a
callFresh f = \(a, d) -> f (Var d) (a, d + 1)

conj, disj :: Goal a -> Goal a -> Goal a
conj = (>=>)
disj = liftM2 (++)
