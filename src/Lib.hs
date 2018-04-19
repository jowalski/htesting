{-# LANGUAGE DeriveGeneric #-}

module Lib
  (someFunc)
  where

import GHC.Generics
import Test.QuickCheck
import Generic.Random

-- -- generate :: Gen a -> IO a
-- -- produce 1, 2, or 3
-- generate $ elements [1,2,3]
-- -- produce a lowercase letter
-- generate $ choose ('a', 'z')
-- -- produce a constant value (since Gen has a Monad instance)
-- generate $ return 1
--
--
-- So far the grammar is fairly limited, but QuickCheck uses a typeclass called
-- Arbitrary to produce generators for more varied types.
--
-- class Arbitrary a where
--   arbitrary :: Gen a
--
-- -- manually generating random Bool
-- generate $ choose (False,True)
-- -- ...vs using the Bool instance of Arbitrary
-- generate (arbitrary :: Gen Bool)
--
-- Gen is also an instance of Applicative, so it can be applied to fill the
-- fields of custom datatypes.
--
-- If you’re truly lazy you can avoid repeatedly applying arbitrary by using the
-- generic-random package.
--
data MyType =
  MyType {foo :: Int
         ,bar :: Bool
         ,baz :: Float}
  deriving (Show,Generic)

instance Arbitrary MyType where
  arbitrary = genericArbitraryRec uniform

-- generate (arbitrary :: Gen MyType)
-- generate $ MyType <$> arbitrary <*> arbitrary <*> arbitrary
-- generate (genericArbitrary :: Gen MyType)
-- To generate more complicated recursive test cases, use the Monad and
-- Applicative instance of Gen. For instance,
--
-- Randomly use one of several generators with
-- oneof :: [Gen a] -> Gen a
myList :: Arbitrary a
       => Gen [a]
myList = oneof [return [],(:) <$> arbitrary <*> myList]

-- generate $ (myList :: Gen [Int])
--
-- This generator tends to produce very short lists, many of them
-- empty. Statistically that’s how it works when at each step the function is
-- fifty-percent likely to terminate. Rather than use oneof to choose generators
-- we can skew the probability.
-- -- like oneof, but with weighted distribution
-- -- frequency :: [(Int, Gen a)] -> Gen a
myList' :: Arbitrary a
        => Gen [a]
myList' = frequency [(1,return []),(4,(:) <$> arbitrary <*> myList')]

-- We can modify our list generator to heed its size parameter. We’ll tie the
-- size to the likelihood of continuing to build a list and use the sized
-- function to read the parameter.
--
-- sized :: (Int -> Gen a) -> Gen a
flexList :: Arbitrary a
         => Gen [a]
flexList =
  sized $ \n -> frequency [(1,return []),(n,(:) <$> arbitrary <*> flexList)]

-- We can modify a generator to use a fixed size with
-- resize :: Int -> Gen a -> Gen a
--
-- Here's a big list of big numbers
-- generate (resize 1000 flexList :: Gen [Int])
--
-- Alternately we can alter a generator's sensitivity to size. This will be
-- useful later
--
-- generate (scale (*33) flexList :: Gen [Int])
blah = id

someFunc :: IO ()
someFunc = putStrLn "someFunc"