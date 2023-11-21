module Util where

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (init xs, last xs)

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x : xs) (y : ys) = x == y && startsWith xs ys
