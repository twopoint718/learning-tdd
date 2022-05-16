module Main where

import Test.Hspec

data Currency = EUR | USD | KRW deriving (Eq , Show)
data Money = Money { amount :: Float, currency :: Currency }
    deriving (Eq, Show)

times Money { amount = amt, currency = curr } multiplier =
    Money { amount = amt * multiplier, currency = curr }

divide Money { amount = amt, currency = curr } divisor =
    Money { amount = amt / divisor, currency = curr }

main :: IO ()
main = hspec $
    describe "Money" $ do
        it "multiplication in dollars" $ do
            let fiver = Money { amount = 5, currency = USD }
            let tenner = fiver `times` 2
            amount tenner `shouldBe` 10

        it "multiplication in euros" $ do
            let tenEuros = Money { amount = 10, currency = EUR }
            let twentyEuros = tenEuros `times` 2
            amount twentyEuros `shouldBe` 20
            currency twentyEuros `shouldBe` EUR

        it "division" $ do
            let originalMoney = Money { amount = 4002, currency = KRW }
            let actualMoneyAfterDivision = originalMoney `divide` 4
            let expectedMoneyAfterDivision = Money { amount = 1000.5, currency = KRW }
            actualMoneyAfterDivision `shouldBe` expectedMoneyAfterDivision
