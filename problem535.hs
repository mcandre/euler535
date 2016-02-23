import qualified Data.Sequence as Q -- Cheap appending and random indexing

isqrt :: Integral a => a -> a
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

-- fractal series such as S = 1, 1, 2, 1, 3, 2, 4, 1, 5, 3, 6, 2, 7, 8, 4, 9, 1, 10, 11, 5, ...
s :: [Int]
s = 1 : s' (Q.fromList [1] :: Q.Seq Int) 2 0
  where
    s' :: Q.Seq Int -> Int -> Int -> [Int]
    s' this i 0 = y : s' this'' i (isqrt y')
      where
        this' = this Q.|> y
        y = Q.index this' 0
        y' = Q.index this' 1
        this'' = Q.drop 1 this'
    s' this i r = y : s' this' i' r'
      where
        y = i
        this' = this Q.|> y
        i' = i + 1
        r' = r - 1

-- sum of [s !! 0, .., s !! (n-1)]
t :: Int -> Int
t n = (sum . take n) s

-- -- t modulo 10^9
-- t' :: Int -> Int
-- t' = flip mod 1000000000 . t  -- from spec

shout :: (Show a) => String -> a -> IO ()
shout label value = putStrLn (label ++ ":") >> print value

sFirstTwentyGiven :: [Int]
sFirstTwentyGiven = [1, 1, 2, 1, 3, 2, 4, 1, 5, 3, 6, 2, 7, 8, 4, 9, 1, 10, 11, 5]

tFirstGiven :: Int
tFirstGiven = 1

tTwentyGiven :: Int
tTwentyGiven = 86

tOneThousandGiven :: Int
tOneThousandGiven = 364089

tOneBillionGiven :: Int
tOneBillionGiven = 498676527978348241

main :: IO ()
main = do
  shout "S(1:20)_given" sFirstTwentyGiven
  shout "S(1:20)" $ take 20 s

  shout "T(1)_given" tFirstGiven
  shout "T(1)" $ t 1

  shout "T(20)_given" tTwentyGiven
  shout "T(20)" $ t 20

  shout "T(1000)_given" tOneThousandGiven
  shout "T(1000" $ t 1000

  shout "T(10^9)_given" tOneBillionGiven
  shout "T(10^9)" $ t 1000000000

  -- shout "T'(10^18)" $ t' 1000000000000000000
