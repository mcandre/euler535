#!/usr/bin/env runhaskell

import Control.Exception.Base (assert)

-- fractal series 1, 1, 2, 1, 3, 2, 4, 1, 5, 3, 6, 2, 7, 8, 4, 9, 1, 10, 11, 5, ...
s :: Integer -> Integer
s 1 = 1 -- from t
s n = (n) `mod` (n `div` 2) -- or something

-- sum of [s 1, .., s n]
t :: Int -> Integer
t n = sum $ take n $ map s [1..]

t' :: Int -> Integer
t' = ((flip mod) 1000000000) . t  -- from spec

shout :: (Show a) => String -> a -> IO ()
shout label value = putStrLn (label ++ ":") >> print value

s1_20_given = [1, 1, 2, 1, 3, 2, 4, 1, 5, 3, 6, 2, 7, 8, 4, 9, 1, 10, 11, 5]

t1_given = 1
t20_given = 86
t1000_given = 364089
t1000000000_given = 498676527978348241

main :: IO ()
main = do
  shout "S(1:20)_given" s1_20_given
  shout "S(1:20)" $ map s $ take 20 [1..]

  -- shout "T(1)_given" t1_given
  -- shout "T(1)" $ t 1

  -- shout "T(20)_given" t20_given
  -- shout "T(20)" $ t 20

  -- shout "T(1000)_given" t1000_given
  -- shout "T(1000" $ t 1000

  -- shout "T(10^9)_given" t1000000000_given
  -- shout "T(10^9)" $ t (10^9)

  -- shout "T'(10^18)" $ t' 10^18
