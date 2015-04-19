{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module MicroKanren where

import Control.Monad ((>=>), liftM2, mzero)

import Numeric.Natural (Natural)

import Text.Groom (groom)

newtype Var = Var { unVar :: Natural } deriving (Eq, Num, Show)

data Term a = Variable Var | Term a | Pair (Term a) (Term a) deriving (Eq, Show)

newtype Unify a = Unify { unUnify :: Maybe (State a) } deriving (Eq, Show)

type State a = [(Var, Term a)]

type Goal a = (State a, Natural) -> [(State a, Natural)]

var :: Natural -> Var
var = Var

varP :: Term a -> Bool
varP (Variable _) = True
varP _            = False

pairP :: Term a -> Bool
pairP (Pair _ _) = True
pairP _          = False

varEq :: Eq a => Term a -> Term a -> Bool
varEq x1 x2 = varP x1 && varP x2 && x1 == x2

walk :: Term a -> State a -> Term a
walk u@(Variable v) s = maybe u (`walk` s) $ lookup v s
walk u              _ = u

extS :: Var -> Term a -> State a -> State a
extS x v s = (x, v):s

(===) :: Eq a => Term a -> Term a -> Goal a
u === v = \(a, d) -> maybe mzero (pure . (, d)) $ unUnify $ unify u v a

unify :: Eq a => Term a -> Term a -> State a -> Unify a
unify u v s
    | varEq u' v'          = Unify $ Just $ s
    | varP u'              =
        let (Variable u'') = u'
        in Unify $ Just $ extS u'' v' s
    | varP v'              =
        let (Variable v'') = v'
        in Unify $ Just $ extS v'' u' s
    | pairP u' && pairP v' =
        let (Pair ua ud) = u'
            (Pair va vd) = v'
        in Unify $ (unUnify . unify ua va >=> unUnify . unify ud vd) s
    | u' == v'             = Unify $ Just $ s
    | otherwise            = Unify Nothing
    where
    u' = walk u s
    v' = walk v s

callFresh :: (Var -> Goal a) -> Goal a
callFresh f = \(a, d) -> f (var d) (a, d + 1)

conj, disj :: Goal a -> Goal a -> Goal a
conj = (>=>)
disj = liftM2 (++)

emptyState :: (State a, Natural)
emptyState = ([], 0)

q5 :: Goal Natural
q5 = callFresh $ \q -> Variable q === Term 5

aAndB :: Goal Natural
aAndB = conj (callFresh $ \a -> Variable a === Term 7)
             (callFresh $ \b -> disj (Variable b === Term 5)
                                     (Variable b === Term 6))

bs :: Goal Natural
bs = callFresh $ \b -> disj (Variable b === Term 1) (Variable b === Term 2)

main :: IO ()
main = do
    putStrLn $ groom $ q5 emptyState
    putStrLn $ groom $ aAndB emptyState
    putStrLn $ groom $ bs emptyState
