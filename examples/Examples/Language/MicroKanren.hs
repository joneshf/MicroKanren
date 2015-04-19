module Examples.Language.MicroKanren where

import Language.MicroKanren

import Numeric.Natural

q5 :: Goal Natural
q5 = callFresh $ \q -> Variable q === Term 5

aAndB :: Goal Natural
aAndB = conj (callFresh $ \a -> Variable a === Term 7)
             (callFresh $ \b -> disj (Variable b === Term 5)
                                     (Variable b === Term 6))

bs :: Goal Natural
bs = callFresh $ \b -> disj (Variable b === Term 1) (Variable b === Term 2)

fives :: Var -> Goal Natural
fives x = disj (Variable x === Term 5) (fives x)

sixes :: Var -> Goal Natural
sixes x = disj (Variable x === Term 6) (sixes x)

fivesAndSixes :: Goal Natural
fivesAndSixes = callFresh $ disj <$> fives <*> sixes

q3 =
    callFresh $ \q ->
    callFresh $ \x ->
    callFresh $ \z ->
    conj (Variable x === Variable z) (conj (Term (Just 3) === Variable z) (Variable q === Variable x))

q = callFresh $ \q ->
    callFresh $ \x ->
    Variable x === Term 3

main :: IO ()
main = do
    print $ q5 emptyState
    print $ aAndB emptyState
    print $ bs emptyState
    print $ take 5 $ fivesAndSixes emptyState
    print $ run 1 q3
    print $ run 1 q5
    print $ run 5 fivesAndSixes
