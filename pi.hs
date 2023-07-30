module Main where

circle :: Float -> Float
circle x = sqrt (abs(1.0 - x**2.0))

area :: Float -> Float -> Float
area x1 x2 = (x2 - x1) * circle x2

estimate (x:[]) = 0
estimate (x:y:xs) = estimate (y:xs) + (area x y)

main :: IO ()
main = do
  let n = 4000
  print $ 4.0 * estimate (map (/n) [1..n])
