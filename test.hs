module Test where

fact :: Integer -> Integer
fact n = n * fact (n - 1)
fact 0 = 1
