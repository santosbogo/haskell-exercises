module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0         = Nothing
  | n == 1 = Just 0
  | n `mod` 2 == 0 = fmap (+1) (collatz(n `div` 2))
  | n `mod` 2 == 1 = fmap (+1) (collatz(3 * n + 1))

