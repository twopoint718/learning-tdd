{-# LANGUAGE DeriveFunctor #-}

module Stocks.Portfolio where

import Data.List (intercalate)

import Stocks.Money

data Portfolio = Portfolio [Money]
newtype Validation e r = Validation (Either e r) deriving (Eq, Show, Functor)

instance Semigroup s => Applicative (Validation s) where
    pure = Validation . pure
    Validation (Left x) <*> Validation (Left y) = Validation (Left (x <> y))
    Validation f <*> Validation r = Validation (f <*> r)

newPortfolio = Portfolio []

add (Portfolio ms) money = Portfolio (money:ms)

evaluate (Portfolio monies) targetCurrency =
    let
        convertedMonies = traverse (\money -> convert money targetCurrency) monies
        totalUpMoney newAmounts = newMoney (sum newAmounts) targetCurrency
    in
        case fmap totalUpMoney convertedMonies of
            Validation (Left msgs) -> Left ("Missing exchange rate(s): " ++ intercalate ", " msgs)
            Validation (Right money) -> Right money

convert money targetCurrency =
    if currency money == targetCurrency then
        Validation (Right (amount money))
    else
        fmap (\rate -> rate * (amount money)) (exchangeRate (currency money) targetCurrency)
  where
    exchangeRate EUR USD = Validation (Right 1.2)
    exchangeRate USD KRW = Validation (Right 1100)
    exchangeRate from to = Validation (Left [show from ++ "->" ++ show to])
