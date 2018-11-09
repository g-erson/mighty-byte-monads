{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude
import Data.Char

--rand :: Seed -> (Integer, Seed)
--mkSeed:: Integer -> Seed

type Gen a = Seed -> (a, Seed)

randSeq :: Gen a -> Integer -> Integer -> [a]
randSeq f init = randSeq' [] (f $ mkSeed init)
  where
    randSeq' acc  _        0     = reverse acc
    randSeq' acc (a, seed) count = randSeq' (a:acc) (f seed) (count - 1)

randThing :: (Integer -> a) -> Gen a
randThing f s = randThing' (rand s)
  where
    randThing' (n,s) = (f n,s)

randLetter :: Gen Char
randLetter = randThing toLetter 

randPair :: Gen (Char, Integer)
randPair = randThing (\(s,a) -> (toLetter a, fst $ rand s))

--am confuse. Too sleepy
--generalPair :: Gen a -> Gen b -> Gen (a,b)
--generalPair fa fb = randThing (\a -> (fa a, fst $ fb $ mkSeed a))

randEven :: Gen Integer -- the output of rand * 2
randEven = randThing (* 2)

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = randThing (\a -> (a * 2) + 1)

randTen :: Gen Integer -- the output of rand * 10
randTen = randThing (* 10)

fiveRandInts :: [Integer]
fiveRandInts = randSeq (rand) 1 5 

fiveRandEvenInts :: [Integer]
fiveRandEvenInts = randSeq (randEven) 1 5 

fiveRandOddInts :: [Integer]
fiveRandOddInts = randSeq (randOdd) 1 5 

fiveRandTenInts :: [Integer]
fiveRandTenInts = randSeq (randTen) 1 5 

threeRandChars :: [Char]
threeRandChars = randSeq (randLetter) 1 3

multRands :: [Integer] -> Integer
multRands = foldl (\a b -> a * b) 1
