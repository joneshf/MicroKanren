module Language.MicroKanren.Extended where

import Language.MicroKanren.Core

conj' :: [Goal a] -> Goal a
conj' = foldr conj $ const [([], 0)]

disj' :: [Goal a] -> Goal a
disj' = foldr disj $ const []

conde :: Eq a => [[Goal a]] -> Goal a
conde = disj' . fmap conj'
