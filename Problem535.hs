module Main (main) where

import qualified Data.Sequence as Q -- Cheap appending and random indexing

{-# INLINE isqrt #-}

isqrt :: Word -> Word
isqrt = floor . (sqrt :: Float -> Float) . fromIntegral -- single-precision floating point

-- fractal series such as S = 1, 1, 2, 1, 3, 2, 4, 1, 5, 3, 6, 2, 7, 8, 4, 9, 1, 10, 11, 5, ...
s :: [Word] -- unsigned 64 bit integer
s = 1 : s' (1 Q.<| Q.empty) 2 0
  where
    s' :: Q.Seq Word -> Word -> Word -> [Word]
    s' this i 0 = y : s' (Q.drop 1 this') i (isqrt y')
      where
        this' = this Q.|> y
        y = Q.index this' 0
        y' = Q.index this' 1
    s' this i r = y : s' (this Q.|> y) (i + 1) (r - 1)
      where
        y = i

-- sum of [s !! 0, .., s !! (n-1)]
t :: Int -> Integer
t n = (sum . take n . map fromIntegral) s

-- -- t modulo 10^9
-- t' :: Word -> Word
-- t' = flip mod 1000000000 . t  -- from spec

shout :: (Show a) => String -> a -> IO ()
shout label value = putStrLn (label ++ ":") >> print value

sFirstTwentyGiven :: [Word]
sFirstTwentyGiven = [1, 1, 2, 1, 3, 2, 4, 1, 5, 3, 6, 2, 7, 8, 4, 9, 1, 10, 11, 5]

tFirstGiven :: Word
tFirstGiven = 1

tTwentyGiven :: Word
tTwentyGiven = 86

tOneThousandGiven :: Word
tOneThousandGiven = 364089

tOneBillionGiven :: Word
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
  shout "T(1000)" $ t 1000

  shout "T(10^9)_given" tOneBillionGiven
  shout "T(10^9)" $ t 1000000000

  -- shout "T'(10^18)" $ t' 1000000000000000000