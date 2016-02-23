data Mode = Run | Walk

isqrt :: Integral a => a -> a
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

actions :: [Mode]
actions = cycle [Walk, Run]

-- fractal series such as S = 1, 1, 2, 1, 3, 2, 4, 1, 5, 3, 6, 2, 7, 8, 4, 9, 1, 10, 11, 5, ...
s :: [Int]
s = [1] ++ (s' [1] [2..] 0 actions)
  where
    s' :: [Int] -> [Int] -> Int -> [Mode] -> [Int]
    s' this ns p (mode:ms) = emissions ++ (s' this' ns' p' ms)
      where
        p' = case mode of
          Walk -> p
          Run -> p + 1

        b = this !! p'

        (emissions, ns') = case mode of
          Walk -> ([b], ns)
          Run -> splitAt (isqrt b) ns

        this' = this ++ emissions

-- sum of [s !! 0, .., s !! (n-1)]
t :: Int -> Int
t n = (sum . take n) s

-- t modulo 10^9
t' :: Int -> Int
t' = ((flip mod) 1000000000) . t  -- from spec

shout :: (Show a) => String -> a -> IO ()
shout label value = putStrLn (label ++ ":") >> print value

s1_20_given :: [Int]
s1_20_given = [1, 1, 2, 1, 3, 2, 4, 1, 5, 3, 6, 2, 7, 8, 4, 9, 1, 10, 11, 5]

t1_given :: Int
t1_given = 1

t20_given :: Int
t20_given = 86

t1000_given :: Int
t1000_given = 364089

t1000000000_given :: Int
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
  shout "T(10^9)" $ t 1000000000

  -- shout "T'(10^18)" $ t' 1000000000000000000
