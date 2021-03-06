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

--Part A, Problem 4
--define map2
map2 :: (a -> a -> a) -> [a] -> [a] -> [a]
map2 f _ [] = []
map2 f [] _ = []
map2 f (x:xs) (y:ys) =  f x y : map2 f xs ys

--define map3
map3 :: (a -> a -> a -> a) -> [a] -> [a] -> [a] -> [a]
map3 f [] _ _ = []
map3 f _ [] _ = []
map3 f _ _ [] = []
map3 f (x:xs) (y:ys) (z:zs) =  f x y z : map3 f xs ys zs

dot :: [Integer] -> [Integer] -> Integer
dot = (sum .) . map2 (*)

--show evaluation for dotfree form
{-
dot lst1 lst2

((sum .) . map2 (*)) lst1 lst2
(sum .) (map2 (*) lst1 lst2)
(\x -> sum . x) (map2 (*) lst1 lst2)
(sum (map2 (*) lst1 lst2))
(sum lst3)
num
-}

--Part A, Problem 5
-- sum [x | x <-[1..1000], x `mod` 5 == 0 || x `mod` 3 == 0]
-- sum: 234168

--Part A, Problem 6
sieve :: [Integer] -> [Integer]
sieve (n:ns) = [n] ++ (filter (\x -> x `mod` n /= 0) ns)


--Part B, problem 1

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- Vanier says that it is poor style to use head or tail when we can use 
-- pattern matching instead.

--Part B, Problem 2
largest2 :: [Integer] -> Integer
largest2 xs | length xs == 0 = error "empty list"
largest2 xs | length xs == 1 = head xs
largest2 xs = max (head xs) (largest2 (tail xs))


largest :: [Integer] -> Integer
largest [] = error "empty list"
largest [x] = x
largest (x:xs) = max x (largest xs)

--Part C, Part 1

{-
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

Evaluate fib 3

fib 3
--> fib (3 - 1) + fib (3 - 2)
--> fib 2 + fib (3 - 2)
--> fib 2 + fib 1
--> fib (2 - 1) + fib (2 - 2) + fib 1
--> fib 1 + fib (2 - 2) + fib 1
--> fib 1 + fib 0 + fib 1
--> 1 + fib 0 + fib 1
--> 1 + 0 + fib 1
--> 1 + 0 + 1
--> 2
-}

-- Part C, Part 2

{-
fact :: Integer -> Integer
fact n = n * fact (n - 1)
fact 0 = 1

Evaluate fact 3

fact 3
--> 3 * fact (3 - 1)
--> 3 * fact 2
--> 3 * 2 * fact (2 - 1)
--> 3 * 2 * fact 1
--> 3 * 2 * 1 * fact (1 - 1)
--> 3 * 2 * 1 * fact 0
--> 3 * 2 * 1 * 0 * fact (0 - 1)
--> 3 * 2 * 1 * 0 * fact -1

Non-termination is the outcome.

The reason is that the order in the pattern matching of the
function should be switched. Currently, fact 3 will always
match to the fact n pattern, which will be non-termination. 
This is because the fact n pattern is before the fact 0 pattern, 
and order matters. fact 0 pattern match should be above 
fact n pattern match.

-}

reverse :: [a] -> [a]
reverse xs = iter xs []
  where
    iter :: [a] -> [a] -> [a]
    iter [] ys = ys
    iter (x:xs) ys = iter xs (x:ys)
