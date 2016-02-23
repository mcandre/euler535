#!/usr/bin/env runhaskell

data Mode = Run | Walk

instance Enum Mode where
  fromEnum Run = 0
  fromEnum Walk = 1

  toEnum 0 = Run
  toEnum 1 = Walk

  succ Run = Walk
  succ Walk = Run

isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

-- fractal series such as S = 1, 1, 2, 1, 3, 2, 4, 1, 5, 3, 6, 2, 7, 8, 4, 9, 1, 10, 11, 5, ...
s :: [Int]
s = [1] ++ (s' [2..] 0 Walk)
  where
    s' :: [Int] -> Int -> Mode -> [Int]
    s' ns p mode = items ++ (s' ns' p' mode')
      where
        p' = case mode of
          Walk -> p
          Run -> p + 1

        b = s !! p'

        (items, ns') = case mode of
          Walk -> ([b], ns)
          Run -> splitAt (isqrt b) ns

        mode' = succ mode

-- sum of [s !! 0, .., s !! (n-1)]
t :: Int -> Int
t n = (sum . take n) s

-- t modulo 10^9
t' :: Int -> Int
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
  shout "S(1:20)" $ take 20 s

  shout "T(1)_given" t1_given
  shout "T(1)" $ t 1

  shout "T(20)_given" t20_given
  shout "T(20)" $ t 20

  shout "T(1000)_given" t1000_given
  shout "T(1000" $ t 1000

  shout "T(10^9)_given" t1000000000_given
  shout "T(10^9)" $ t (10^9)

  -- shout "T'(10^18)" $ t' 10^18
