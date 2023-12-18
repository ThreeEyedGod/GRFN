import Data.Numbers.Primes
import Lib
import System.IO.Error (isDoesNotExistError, tryIOError)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary, Gen, NonNegative (..), Positive (..), Property, arbitrary, choose, elements, expectFailure, forAll, listOf, quickCheck, suchThat, (==>))
import Test.QuickCheck.Monadic (assert, forAllM, monadicIO, pick, pre, run)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup
      "Group firstPrimeLE"
      [ testProperty "propcheckIfPrimeOdd" prop_checkIfPrimeIsOdd,
        testProperty "prop_ensureIfPrimeActuallyIsLess" prop_ensureIfPrimeIsActuallyLess
      ],
    testGroup
      "Group createSeq"
      [ testProperty "prop_checkSeqisOnlyPrimes" prop_checkSeqisOnlyPrimes
      ],
    testGroup
      "Group genARandomPreFactoredNumberLEn"
      [ testProperty "prop_checkIfLEn" prop_checkIfLEn,
        testProperty "prop_checkIffiltersValidInput" prop_checkIffiltersValidInput
      ]
  ]

-- banks on the property that firstPrimeLE must be odd and the sum of two odds must be even
prop_checkIfPrimeIsOdd :: Positive Int -> Positive Int -> Property
prop_checkIfPrimeIsOdd (Positive n) (Positive m) = n > 2 && n < 30 && m < 50 && m > 2 ==> even (firstPrimeLE n + firstPrimeLE m)

prop_ensureIfPrimeIsActuallyLess :: Positive Int -> Property
prop_ensureIfPrimeIsActuallyLess (Positive n) = n > 2 && n < 30 ==> firstPrimeLE n <= n

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
  x <- run $ genARandomPreFactoredNumberLEn n
  case x of
    Left err -> assert (err == "Invalid")
    Right _ -> assert (1 == 2)
