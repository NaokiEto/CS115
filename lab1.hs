module Lab1 where

--Part A, Problem 1a
(+*) :: Double -> (Double -> Double)
x +* y = x^2 + y^2

infixr 7 +*

--Part A, Problem 1b
(^||) :: Bool -> (Bool -> Bool)
False ^|| y = y
True ^|| y = not(y)

infixr 3 ^||

--Part A, Problem 2
rangeProduct :: Integer -> Integer -> Integer
rangeProduct x y | x == y = y
rangeProduct x y | x > y = error "1st number must be smaller than 2nd number"
rangeProduct x y | x < y = x * (rangeProduct (x+1) y)

--Part A, Problem 3
--define prod
prod :: [Integer] -> Integer
prod a | a == [] = 1
prod a = foldr (*) 1 a

--use prod to define rangeproduct (from problem 2)
rangeProductV2 :: Integer -> Integer -> Integer
rangeProductV2 x y | x <= y = prod [x..y]
rangeProductV2 x y | x > y = error "1st number must be smaller than 2nd number"

--Part B, problem 1

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- Vanier says that it is poor style to use head or tail when we can use 
-- pattern matching instead.



