module Main where

import Test.Hspec

import Stocks.Money
import Stocks.Portfolio

main :: IO ()
main = hspec $
    describe "Money" $ do
        it "multiplication" $ do
            let tenEuros = newMoney 10 EUR
            let twentyEuros = tenEuros `times` 2
            amount twentyEuros `shouldBe` 20
            currency twentyEuros `shouldBe` EUR

        it "division" $ do
            let originalMoney = newMoney 4002 KRW
            let actualMoneyAfterDivision = originalMoney `divide` 4
            let expectedMoneyAfterDivision = newMoney 1000.5 KRW
            actualMoneyAfterDivision `shouldBe` expectedMoneyAfterDivision

        it "addition" $ do
            let portfolio = Portfolio []

            let fiveDollars = newMoney 5 USD
            let tenDollars = newMoney 10 USD
            let fifteenDollars = newMoney 15 USD

            let portfolio' = portfolio `add` fiveDollars
            let portfolio'' = portfolio' `add` tenDollars
            let portfolioInDollars = portfolio'' `evaluate` USD

            portfolioInDollars `shouldBe` fifteenDollars
