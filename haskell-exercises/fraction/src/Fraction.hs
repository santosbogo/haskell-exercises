module Fraction (Fraction, add, sub, mul, divide, hcf) where

type Fraction = (Int, Int)

add :: Fraction -> Fraction -> Fraction
add (a, b) (c, d) = simplify(a * d + b * c, b * d)

sub :: Fraction -> Fraction -> Fraction
sub (a, b) (c, d) = simplify(a * d - b * c, b * d)

mul :: Fraction -> Fraction -> Fraction
mul (a, b) (c, d) = simplify(a * c, b * d)

divide :: Fraction -> Fraction -> Fraction
divide (a, b) (c, d) = simplify(a * d, b * c)

hcf :: Int -> Int -> Int
hcf a 0 = abs a
hcf a b = hcf b (a `mod` b)

simplify :: Fraction -> Fraction
simplify (a, b) = (a `div` (hcf a b), b `div` (hcf a b))