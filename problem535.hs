#!/usr/bin/env runhaskell

import Data.List (genericTake)   -- like take but accepts Integer bounds

-- fractal series 1, 1, 2, 1, 3, 2, 4, 1, 5, 3, 6, 2, 7, 8, 4, 9, 1, 10, 11, 5, ...
s :: Integer -> Integer
s 1 = 1 -- from t
s n = undefined

-- sum of [s 1, .., s n]
t :: Integer -> Integer
t 1 = 1                           -- from spec
t 20 = 86                         -- from spec
t 1000 = 364089                   -- from spec
t 1000000000 = 498676527978348241 -- from spec
t n = sum $ genericTake n $ map s [1..]

t' :: Integer -> Integer
t' = ((flip mod) 1000000000) . t  -- from spec

shout :: (Show a) => String -> a -> IO ()
shout label value = putStrLn (label ++ ":") >> print value

main :: IO ()
main = do
  shout "S(1)" $ s 1
  shout "T'(10^18)" $ t' 10^18
