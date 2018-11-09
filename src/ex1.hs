{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude
import Data.Char

--rand :: Seed -> (Integer, Seed)
--mkSeed:: Integer -> Seed

type Gen a = Seed -> (a, Seed)

randSeq :: (Integer -> a) -> Integer -> Integer -> [a]
randSeq f init = randSeq' [] (randThing f $ mkSeed init)
  where
    randSeq' acc  _        0     = reverse acc
    randSeq' acc (a, seed) count = randSeq' (a:acc) (randThing f $ seed) (count - 1)
    randThing f s = randThing' (rand s)
      where
        randThing' (n,s) = (f n,s)

--randPair :: Gen (Char, Integer)
--randPair = randThing (\(s,a) -> (toLetter a, fst $ rand s))

--am confuse. Too sleepy
--generalPair :: Gen a -> Gen b -> Gen (a,b)
--generalPair fa fb = randThing (\a -> (fa a, fst $ fb $ mkSeed a))

fiveRandInts :: [Integer]
fiveRandInts = randSeq (\a -> a) 1 5 

fiveRandEvenInts :: [Integer]
fiveRandEvenInts = randSeq (* 2) 1 5 

threeRandChars :: [Char]
threeRandChars = randSeq (toLetter) 1 3
