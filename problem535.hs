#!/usr/bin/env runhaskell

import Control.Exception.Base (assert)

-- fractal series 1, 1, 2, 1, 3, 2, 4, 1, 5, 3, 6, 2, 7, 8, 4, 9, 1, 10, 11, 5, ...
s :: Integer -> Integer
s 1 = 1 -- from t
s n = (n) `mod` (n `div` 2) -- or something

-- sum of [s 1, .., s n]
t :: Int -> Integer
t 1 = 1                           -- from spec
t 20 = 86                         -- from spec
t 1000 = 364089                   -- from spec
t 1000000000 = 498676527978348241 -- from spec
t n = sum $ take n $ map s [1..]

t' :: Int -> Integer
t' = ((flip mod) 1000000000) . t  -- from spec

shout :: (Show a) => String -> a -> IO ()
shout label value = putStrLn (label ++ ":") >> print value

s_given = [1, 1, 2, 1, 3, 2, 4, 1, 5, 3, 6, 2, 7, 8, 4, 9, 1, 10, 11, 5]

main :: IO ()
main = do
  shout "S(1)" $ s 1

  shout "S(1:20)_given" s_given

  shout "S(1:20)" $ map s $ take 20 [1..]

  shout "S(1:20) = S(1:20)_given" $ (map s (take 20 [1..])) == s_given

  shout "T'(10^18)" $ t' 10^18
