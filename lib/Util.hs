module Util where

import Control.Monad

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (init xs, last xs)

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x : xs) (y : ys) = x == y && startsWith xs ys

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = foldr (\x r -> f x `mplus` r) Nothing

whenErr :: (Monad m) => Either a b -> (a -> m ()) -> m ()
whenErr (Left x) f = f x
whenErr _ _ = return ()

showSanitized :: (Show a) => a -> String
showSanitized = (tail . init) . (show . show)
