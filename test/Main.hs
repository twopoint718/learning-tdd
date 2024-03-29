{-# LANGUAGE OverloadedLists #-}

module Main where

import Test.Hspec

import Stocks.Money
import Stocks.Portfolio
import Stocks.Bank

bank :: Bank
bank =
    [ ((EUR, USD), 1.2)
    , ((USD, KRW), 1100)
    ]

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
            let fiveDollars = newMoney 5 USD
            let tenDollars = newMoney 10 USD
            let fifteenDollars = Right (newMoney 15 USD)

            let portfolio = [fiveDollars, tenDollars]
            let portfolioInDollars = portfolio `evaluate` bank $ USD

            portfolioInDollars `shouldBe` fifteenDollars

        it "addition of dollars and euros" $ do
            let fiveDollars = newMoney 5 USD
            let tenEuros = newMoney 10 EUR
            let portfolio = [fiveDollars, tenEuros]

            let expectedValue = Right (newMoney 17 USD)
            let actualValue = portfolio `evaluate` bank $ USD

            actualValue `shouldBe` expectedValue

        it "addition of dollars and wons" $ do
            let oneDollar = newMoney 1 USD
            let elevenHundredWon = newMoney 1100 KRW
            let portfolio = [oneDollar, elevenHundredWon]

            let expectedValue = Right (newMoney 2200 KRW)
            let actualValue = portfolio `evaluate` bank $ KRW

            actualValue `shouldBe` expectedValue

        it "addition with multiple missing exchange rates" $ do
            let oneDollar = newMoney 1 USD
            let oneEuro = newMoney 1 EUR
            let oneWon = newMoney 1 KRW

            let portfolio = [oneWon, oneEuro, oneDollar]

            let expectedErrorMessage = Left "Missing exchange rate(s): KRW->Kalganid, EUR->Kalganid, USD->Kalganid"
            let actualError = portfolio `evaluate` bank $ Kalganid

            actualError `shouldBe` expectedErrorMessage

        it "conversion with different rates between two curriencies" $ do
            let tenEuros = newMoney 10 EUR
            let Validation (Right actualConvertedMoney) = bank `convert` tenEuros $ USD
            actualConvertedMoney `shouldBe` newMoney 12 USD
            let bank' = addExchangeRate (EUR, USD) 1.3 bank
            let Validation (Right actualConvertedMoney') = bank' `convert` tenEuros $ USD
            actualConvertedMoney' `shouldBe` newMoney 13 USD

        it "conversion with missing exchange rate" $ do
            let tenEuros = newMoney 10 EUR
            let Validation (Left [err]) = bank `convert` tenEuros $ Kalganid
            err `shouldBe` "EUR->Kalganid"
