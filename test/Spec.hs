import Data.Numbers.Primes (primeFactors, primes)
import Data.Text (pack)
import FactoredRandomNumbers (genARandomPreFactoredNumberLTEn, preFactoredNumOfBitSize, preFactoredNumOfBitSizePar)
import System.IO.Error (isDoesNotExistError, tryIOError)
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldNotReturn, shouldReturn)
import Test.Hspec.Core.QuickCheck (modifyMaxDiscardRatio, modifyMaxSuccess)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Args (..), Gen, Negative (..), NonNegative (..), Positive (..), Property, arbitrary, chatty, choose, classify, collect, counterexample, cover, elements, expectFailure, forAll, forAllProperties, listOf, printTestCase, quickCheck, quickCheckWithResult, suchThat, verbose, verboseCheckWithResult, withMaxSuccess, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

main :: IO ()
main = hspec $ do
  libH

-- Rigorous test arguments. unused as of now
rigorousArgs :: Args
rigorousArgs =
  Args
    { replay = Nothing,
      maxSuccess = 100, -- tests to run
      maxDiscardRatio = 5, -- the number of tests that are thrown out and ignored b/c of "==>" conditions, before "giving up" and failing due to too many discarded tests
      maxSize = 10, -- if a prop_ function uses a list ([]) type, maxSize is the max length of the randomly generated list
      chatty = True,
      maxShrinks = 5
    }

libH :: Spec
libH = describe "All Property Tests" $ do
  libHProperty8
  libHProperty9
  libHProperty10
  libHProperty11
  libHProperty12
  libHProperty13
  libHProperty14

libHProperty8 :: Spec
libHProperty8 = do
  modifyMaxSuccess (const 100) $
    prop
      "prop_checkIfLTEn"
      prop_checkIfLTEn

libHProperty9 :: Spec
libHProperty9 = do
  modifyMaxSuccess (const 5) $
    modifyMaxDiscardRatio (const 10) $
      prop
        "prop_checkIffiltersInValidInput"
        prop_checkIffiltersInValidInput

libHProperty10 :: Spec
libHProperty10 = do
  modifyMaxSuccess (const 100) $
    prop
      "prop_checkValidOutput"
      prop_checkValidOutput

libHProperty11 :: Spec
libHProperty11 = do
  modifyMaxSuccess (const 100) $
    prop
      "prop_checkAccurateOutput"
      prop_checkAccurateOutput

libHProperty12 :: Spec
libHProperty12 = do
  modifyMaxSuccess (const 100) $
    prop
      "prop_checkAccurateOutputVal"
      prop_checkAccurateOutputVal

libHProperty13 :: Spec
libHProperty13 = do
  modifyMaxSuccess (const 2) $
    modifyMaxDiscardRatio (const 50) $
      prop
        "prop_checkAccurateOutputValBitSize"
        prop_checkAccurateOutputValBitSize

libHProperty14 :: Spec
libHProperty14 = do
  modifyMaxSuccess (const 2) $
    modifyMaxDiscardRatio (const 50) $
      prop
        "prop_checkAccurateOutputValBitSizePar"
        prop_checkAccurateOutputValBitSizePar

------------
prop_checkIfLTEn :: Positive Integer -> Property
prop_checkIfLTEn (Positive n) = n > 2 && n < 30 ==> monadicIO $ do
  x <- run $ genARandomPreFactoredNumberLTEn n
  case x of
    Left _ -> assert False
    Right y -> assert (fst y <= n)

prop_checkIffiltersInValidInput :: Negative Integer -> Property
prop_checkIffiltersInValidInput (Negative n) = n > -10000 && n < 1 ==> monadicIO $ do
  -- Constraining n to be within a "bad range"
  x <- run $ genARandomPreFactoredNumberLTEn n
  case x of
    Left err -> assert (err == pack "Invalid")
    Right _ -> assert False

-- input should be GTE to the head value of a valid pre-factored list
-- (6, [3,2]) ==> 6 >= 3 (5, [5, 1]) ==> 5 >= 5
prop_checkValidOutput :: Positive Integer -> Property
prop_checkValidOutput (Positive n) = n > 2 && n < 50 ==> classify (n < 30) "n LT 30" $ collect n $ monadicIO $ do
  -- if n upper end is set at 100 then it results in an error https://www.cnblogs.com/BlogOfASBOIER/p/13096167.html
  x <- run $ genARandomPreFactoredNumberLTEn n
  case x of
    Left err -> assert (err == pack "Invalid")
    Right y -> assert (fst y >= head (snd y))

prop_checkAccurateOutput :: Positive Integer -> Property
prop_checkAccurateOutput (Positive n) = n > 2 && n < 50 ==> classify (n < 30) "n LT 30" $ collect n $ counterexample "Failed case" $ monadicIO $ do
  -- if n upper end is set at 100 then it results in an error https://www.cnblogs.com/BlogOfASBOIER/p/13096167.html
  x <- run $ genARandomPreFactoredNumberLTEn n
  case x of
    Left err -> assert (err == pack "Invalid")
    Right y -> assert (primeFactorsOr1 (fst y) == snd y)

prop_checkAccurateOutputVal :: Positive Integer -> Property
prop_checkAccurateOutputVal (Positive n) = n > 2 && n < 50 ==> classify (n < 30) "n LT 30" $ collect n $ counterexample "Failed case" $ monadicIO $ do
  -- if n upper end is set at 100 then it results in an error https://www.cnblogs.com/BlogOfASBOIER/p/13096167.html
  x <- run $ genARandomPreFactoredNumberLTEn n
  case x of
    Left err -> assert (err == pack "Invalid")
    Right y -> assert (fst y == product (snd y))

prop_checkAccurateOutputValBitSize :: Positive Integer -> Property
prop_checkAccurateOutputValBitSize (Positive n) = n > 2 && n < 70 ==> classify (n < 50) "n LT 50" $ collect n $ counterexample "Failed case" $ monadicIO $ do
  -- if n upper end is set at 100 then it results in an error https://www.cnblogs.com/BlogOfASBOIER/p/13096167.html
  x <- run $ preFactoredNumOfBitSize n
  case x of
    Left err -> assert (err == pack "Invalid")
    Right y -> assert (fst y == product (snd y))

prop_checkAccurateOutputValBitSizePar :: Positive Integer -> Property
prop_checkAccurateOutputValBitSizePar (Positive n) = n > 2 && n < 70 ==> classify (n < 50) "n LT 50" $ collect n $ counterexample "Failed case" $ monadicIO $ do
  -- if n upper end is set at 100 then it results in an error https://www.cnblogs.com/BlogOfASBOIER/p/13096167.html
  x <- run $ preFactoredNumOfBitSizePar n
  case x of
    Left err -> assert (err == pack "Invalid")
    Right y -> assert (fst y == product (snd y))

primeFactorsOr1 :: Integer -> [Integer]
primeFactorsOr1 1 = [1]
primeFactorsOr1 n = reverse (1 : primeFactors n)


