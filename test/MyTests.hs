{-# LANGUAGE TemplateHaskell #-}

module MyTests where

import Shrinking
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Instances ()
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Text.Regex.PCRE
import Data.Char (ord)
import Data.Monoid ((<>))
import GHC.Int
import qualified Data.Set as S
import Data.Either
import Data.Word (Word8)

-- Haskell infers the type of b because it is passed to
-- to encode. The quickcheck-instances provides an Arbitrary
-- for it, and this whole property is thus Testable. That's
-- a reminder of why it is written so concisely.
prop_sizeRatio b =
  BL.length (encode b) == 4 * ceiling (fromIntegral (BL.length b) / 3)

-- Are the random inputs really exercising all the padding possibilities? What
-- if the input cases never attain certain sizes modulo four? We can get a
-- better window into the test by labeling each case.
prop_endsWithPadding b =
  collect suffix $
  (encB =~ (pack "(^|[^=])" <> suffix <> pack "$"))  -- at end
   &&
  not (encB =~ ((pack "=[^=]") :: BL.ByteString)) -- only at end
  where encB = encode b
        remainder = fromIntegral $ BL.length b `rem` 3
        suffix =
          BL.replicate ((3 - remainder) `rem` 3)
                       (fromIntegral $ ord '=')

prop_outputAlphabet b = used `S.isSubsetOf` allowed
  where used = S.fromList . BL.unpack $ encode b
        allowed =
          S.fromList . map (fromIntegral . ord) $
          ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> ['+','/','=']

-- Calling classify provides a nice way to keep a casual eye on distributions
-- but we want a stronger guarantee. To insist that we get at least one
-- full-alphabet output we switch from classify to cover.
prop_outputAlphabetS b =
  classify (S.size used >= 32) "half-alphabet" .
  classify (S.size used >= 63) "full-alphabet" $
  used `S.isSubsetOf` allowed
  where used = S.fromList . BL.unpack $ encode b
        allowed =
          S.fromList . map (fromIntegral . ord) $
          ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> ['+','/','=']

prop_outputAlphabetC b =
  cover (S.size used >= 63) 1 "full-alphabet" $ used `S.isSubsetOf` allowed
  where used = S.fromList . BL.unpack $ encode b
        allowed =
          S.fromList . map (fromIntegral . ord) $
          ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> ['+','/','=']

prop_outputAlphabetC2 =
  forAll (scale (* 3) (arbitrary :: Gen BL.ByteString)) $
  \b ->
    let used = S.fromList . BL.unpack $ encode b
        allowed =
          S.fromList . map (fromIntegral . ord) $
          ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> ['+','/','=']
    in cover (S.size used >= 63) 1 "full-alphabet" $
       used `S.isSubsetOf` allowed

-- the easy direction
prop_decEnc b = decode (encode b) == Right b

-- this will quickly fail
prop_encDecX b = [b] == (encode <$> rights [decode b])

-- We can discard test cases that aren’t suitable by using the conditional
-- operator.  This works but it is inefficient when the ratio of suitable to
-- unsuitable test cases is small. After getting too many rejections QuickCheck
-- fails.
prop_encDecXX b = legit ==> [b] == (encode <$> rights [dec])
  where dec = decode b
        legit = isRight dec

-- It’s always more efficient to use a custom generator than to conditionally
-- discard test cases. Here’s generator for messages in the Base64 alphabet with
-- proper equals padding, plus the modified test.
--
-- using more Gen combinators: listOf, vectorOf
encoded :: Gen BL.ByteString
encoded =
  do body <- concat <$> listOf (group 0)
     end <- group =<< choose (0,2)
     return . BL.pack $ body <> end
  where group :: Int -> Gen [Word8]
        group pad =
          do letters <-
               vectorOf (4 - pad) . elements . map (fromIntegral . ord) $
               ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> ['+','/','=']
             return $ letters <> replicate pad 61  -- 61 is ascii for =

prop_encDec = forAll encoded $ \b -> [b] == (encode <$> rights [decode b])

--------------------------
return []

runTests :: IO Bool
runTests = $quickCheckAll