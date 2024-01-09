import Data.Numbers.Primes
import Data.Text (pack)
import Lib (createSeq, firstPrimeLE, genARandomPreFactoredNumberLEn, lstPrimesLE)
import System.IO.Error (isDoesNotExistError, tryIOError)
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldNotReturn, shouldReturn)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Args (..), Gen, NonNegative (..), Positive (..), Property, arbitrary, chatty, choose, classify, collect, cover, elements, expectFailure, forAll, forAllProperties, listOf, printTestCase, quickCheck, quickCheckWithResult, suchThat, verbose, verboseCheckWithResult, withMaxSuccess, (==>))
import Test.QuickCheck.Monadic (assert, forAllM, monadicIO, pick, pre, run)

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
  libHProperty1
  libHProperty2
  libHProperty3
  libHProperty4
  libHProperty5
  libHProperty6

libHProperty1 :: Spec
libHProperty1 = do
  modifyMaxSuccess (const 100) $ 
    prop
      "propcheckIfPrimeOdd"
      prop_checkIfPrimeIsOdd

libHProperty2 :: Spec
libHProperty2 = do
  modifyMaxSuccess (const 100) $
    prop
      "prop_ensureIfPrimeActuallyIsLess"
      prop_ensureIfPrimeIsActuallyLess

libHProperty3 :: Spec
libHProperty3 = do
  modifyMaxSuccess (const 100) $
    prop
      "prop_checkSeqisOnlyPrimes"
      prop_checkSeqisOnlyPrimes

libHProperty4 :: Spec
libHProperty4 = do
  modifyMaxSuccess (const 100) $
    prop
      "prop_checkIfLEn"
      prop_checkIfLEn

libHProperty5 :: Spec
libHProperty5 = do
  modifyMaxSuccess (const 100) $
    prop
      "prop_checkIffiltersValidInput"
      prop_checkIffiltersValidInput

libHProperty6 :: Spec
libHProperty6 = do
  modifyMaxSuccess (const 100) $
    prop
      "prop_checkValidOutput1"
      prop_checkValidOutput1

-- banks on the property that firstPrimeLE must be odd and the sum of two odds must be even
prop_checkIfPrimeIsOdd :: Positive Int -> Positive Int -> Property
prop_checkIfPrimeIsOdd (Positive n) (Positive m) = n > 2 && n < 30 && m < 50 && m > 2 ==> classify (n > 2) "n GT 2" $ even (firstPrimeLE n + firstPrimeLE m)

prop_ensureIfPrimeIsActuallyLess :: Positive Int -> Property
prop_ensureIfPrimeIsActuallyLess (Positive n) = n > 2 && n < 30 ==> collect n $ firstPrimeLE n <= n

prop_checkSeqisOnlyPrimes :: Positive Int -> Property
prop_checkSeqisOnlyPrimes (Positive n) = n > 2 && n < 30 ==> filter (not . Data.Numbers.Primes.isPrime) (createSeq n) == [1]

prop_checkIfLEn :: Positive Int -> Property
prop_checkIfLEn (Positive n) = n > 2 && n < 30 ==> monadicIO $ do
  x <- run $ genARandomPreFactoredNumberLEn n
  case x of
    Left _ -> assert (1 == 2)
    Right y -> assert (fst y <= n)

prop_checkIffiltersValidInput :: Int -> Property
prop_checkIffiltersValidInput n = n > -10 && n < 1 ==> monadicIO $ do
  -- notice we are constraining n to be within a "bad range"
  x <- run $ genARandomPreFactoredNumberLEn n
  case x of
    Left err -> assert (err == pack "Invalid")
    Right _ -> assert (1 == 2)

prop_checkValidOutput1 :: Positive Int -> Property
prop_checkValidOutput1 (Positive n) = n > 2 && n < 50 ==> classify (n > 30) "n GT 30" $ collect n $ monadicIO $ do
  -- if n upper end is set at 100 then it results in an error https://www.cnblogs.com/BlogOfASBOIER/p/13096167.html
  x <- run $ genARandomPreFactoredNumberLEn n
  case x of
    Left err -> assert (err == pack "Invalid")
    Right y -> assert (fst y >= (head $ snd y))
