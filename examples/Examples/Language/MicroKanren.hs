module Examples.Language.MicroKanren where

import Language.MicroKanren

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
    print $ q5 emptyState
    print $ aAndB emptyState
    print $ bs emptyState
