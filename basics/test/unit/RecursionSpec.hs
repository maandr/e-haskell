module RecursionSpec where

import Test.Hspec

-- calculate factorial of given integer recursively
factorial :: Integer -> Integer
factorial 0 = 0
factorial 1 = 1
factorial x = x * factorial (x - 1)

-- calculate factorial of given integer recursively
-- including safe-check for negative numbers
safeFactorial :: Integer -> Either String Integer
safeFactorial x = if x < 0
    then Left "negative numbers not allowed."
    else Right (factorial x)

-- calculate fibonacci of given integer recursively
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

-- calculate fibonacci of given integer recursively
-- including safe-check for negative numbers
safeFibonacci :: Integer -> Either String Integer
safeFibonacci x = if x < 0
    then Left "negative numbers not allowed."
    else Right (fibonacci x)

spec :: Spec
spec = do

    describe "factorial" $ do
        it "should calculate factorial of given integer" $ do
            factorial 0 `shouldBe` 0
            factorial 1 `shouldBe` 1
            factorial 3 `shouldBe` 6
            factorial 9 `shouldBe` 362880
            factorial 10 `shouldBe` 3628800

    describe "safeFactorial" $ do
        it "should calculate factorial of given integer with safty-check for negative input values" $ do
            safeFactorial 0 `shouldBe` Right 0
            safeFactorial 1 `shouldBe` Right 1
            safeFactorial 3 `shouldBe` Right 6
            safeFactorial 9 `shouldBe` Right 362880
            safeFactorial 10 `shouldBe` Right 3628800
            safeFactorial (-1) `shouldBe` Left "negative numbers not allowed."

    describe "fibonacci" $ do
        it "should calculate fibonacci number of a given integer" $ do
            fibonacci 0 `shouldBe` 0
            fibonacci 1 `shouldBe` 1
            fibonacci 2 `shouldBe` 1
            fibonacci 5 `shouldBe` 5
            fibonacci 7 `shouldBe` 13
            fibonacci 10 `shouldBe` 55
            fibonacci 23 `shouldBe` 28657

    describe "safeFibonacci" $ do
        it "should calculate fibonacci number of a given integer with safty-check for negativ input values" $ do
            safeFibonacci 0 `shouldBe` Right 0
            safeFibonacci 1 `shouldBe` Right 1
            safeFibonacci 2 `shouldBe` Right 1
            safeFibonacci 5 `shouldBe` Right 5
            safeFibonacci 7 `shouldBe` Right 13
            safeFibonacci 10 `shouldBe` Right 55
            safeFibonacci 23 `shouldBe` Right 28657
            safeFibonacci (-1) `shouldBe` Left "negative numbers not allowed."

