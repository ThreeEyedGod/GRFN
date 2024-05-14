import Data.Numbers.Primes (primeFactors, primes)
import Data.Text (pack)
import Lib (genARandomPreFactoredNumberLTEn)
import System.IO.Error (isDoesNotExistError, tryIOError)
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldNotReturn, shouldReturn)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess, modifyMaxDiscardRatio)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Args (..), Gen, NonNegative (..), Positive (..), Negative (..), Property, arbitrary, chatty, choose, classify, collect, cover, elements, expectFailure, forAll, forAllProperties, listOf, printTestCase, quickCheck, quickCheckWithResult, suchThat, verbose, verboseCheckWithResult, withMaxSuccess, (==>))
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


libHProperty8 :: Spec
libHProperty8 = do
  modifyMaxSuccess (const 100) $
    prop
      "prop_checkIfLTEn"
      prop_checkIfLTEn

libHProperty9 :: Spec
libHProperty9 = do
  modifyMaxSuccess (const 2) $ modifyMaxDiscardRatio (const 11) $
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

------------
prop_checkIfLTEn :: Positive Int -> Property
prop_checkIfLTEn (Positive n) = n > 2 && n < 30 ==> monadicIO $ do
  x <- run $ genARandomPreFactoredNumberLTEn n
  case x of
    Left _ -> assert False
    Right y -> assert (fst y <= n)

prop_checkIffiltersInValidInput :: Negative Int -> Property
prop_checkIffiltersInValidInput (Negative n) = n > -10 && n < 1 ==> monadicIO $ do
  -- notice we are constraining n to be within a "bad range"
  x <- run $ genARandomPreFactoredNumberLTEn n
  case x of
    Left err -> assert (err == pack "Invalid")
    Right _ -> assert False

-- input should be GTE to the head value of a valid pre-factored list
-- (6, [3,2]) ==> 6 >= 3 (5, [5, 1]) ==> 5 >= 5
prop_checkValidOutput :: Positive Int -> Property
prop_checkValidOutput (Positive n) = n > 2 && n < 50 ==> classify (n < 30) "n LT 30" $ collect n $ monadicIO $ do
  -- if n upper end is set at 100 then it results in an error https://www.cnblogs.com/BlogOfASBOIER/p/13096167.html
  x <- run $ genARandomPreFactoredNumberLTEn n
  case x of
    Left err -> assert (err == pack "Invalid")
    Right y -> assert (fst y >= (head $ snd y))

prop_checkAccurateOutput :: Positive Int -> Property
prop_checkAccurateOutput (Positive n) = n > 2 && n < 50 ==> classify (n < 30) "n LT 30" $ collect n $ printTestCase "Failed case" $ monadicIO $ do
  -- if n upper end is set at 100 then it results in an error https://www.cnblogs.com/BlogOfASBOIER/p/13096167.html
  x <- run $ genARandomPreFactoredNumberLTEn n
  case x of
    Left err -> assert (err == pack "Invalid")
    Right y -> assert ((primeFactorsOr1 $ fst y) == snd y)

primeFactorsOr1 :: Int -> [Int]
primeFactorsOr1 1 = [1]
primeFactorsOr1 n = reverse (1 : primeFactors n)
