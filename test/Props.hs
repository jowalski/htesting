{-# LANGUAGE TemplateHaskell #-}

module Props where

import Test.QuickCheck.All
import Test.QuickCheck (Gen, arbitrary, quickCheck)
import Test.QuickCheck.Property (succeeded, failed, reason, Result)
import Test.Invariant

-- (continued)
-- from https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html:
--
-- Specifying Laws
-- Laws, or properties are predicates that can be checked by testing. QuickCheck
-- represents them as Gen Result where Result holds various facts about a test
-- including:
--
--- * Is the test successful, failed, or should it be discarded?
--- * Whether to retry the test
--- * Statistics about test cases
--- * Reason for failure
--- * Exceptions thrown
-- At a high level each property generates a test case inside the Gen monad,
-- evaluates the data against part of the program under test, then constructs a
-- Result. As all this happens inside Gen, the result is of type Gen Result aka
-- Property. To run the tests, QuickCheck generates the results and displays
-- statistics about them.
--
-- Let’s make a Property by hand (the hard way) and then see how QuickCheck
-- makes it easier.
--
-- -- returned values are modified versions of
-- -- succeeded :: Result
-- -- failed :: Result
prop_commutativeAdd :: Gen Result
prop_commutativeAdd =
  do (x,y) <- arbitrary :: Gen (Int,Int)
     return $
       if x + y == y + x
          then succeeded
          else failed {reason = "stupid non-commutative addition"}

prop_commutativeAdd2 :: Int -> Int -> Bool
prop_commutativeAdd2 x y = x + y == y + x

prop_commutativeAdd3 :: Int -> Int -> Bool
prop_commutativeAdd3 = commutative (+)

-- quickCheck prop_commutativeAdd3
--
-- Note that properties must use monomorphic types, you can’t have a property
-- with polymorphism like [a] -> [a] -> Bool. You must pick a monomorphic
-- instance for which to generate test cases. Be careful which instance you
-- choose! Observe the difference between addition of Ints vs Doubles.
--
-- > quickCheck $ associative ((+) :: Int -> Int -> Int)
-- +++ OK, passed 100 tests.
--
-- > quickCheck $ associative ((+) :: Double -> Double -> Double)
--- *** Failed! Falsifiable (after 5 tests and 188 shrinks):
--- -1.4210880596288213e-14
--- 2.842174061166933e-14
--- 128.0
-- blah
prop_associative :: Int -> Int -> Int -> Bool
prop_associative = associative (+)

--------------------------
return []

runTests :: IO Bool
runTests = $quickCheckAll