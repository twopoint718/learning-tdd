{-# LANGUAGE DeriveFunctor #-}

module Stocks.Bank where

import qualified Data.Map as Map

import Stocks.Money

data Bank = Bank (Map.Map (Currency, Currency) Float)

newtype Validation e r = Validation (Either e r) deriving (Eq, Show, Functor)

instance Semigroup s => Applicative (Validation s) where
    pure = Validation . pure
    Validation (Left x) <*> Validation (Left y) = Validation (Left (x <> y))
    Validation f <*> Validation r = Validation (f <*> r)

newBank :: Bank
newBank = Bank Map.empty

addExchangeRate :: Bank -> (Currency, Currency, Float) -> Bank
addExchangeRate (Bank bank) (fromCurr, toCurr, rate) =
    Bank (Map.insert (fromCurr, toCurr) rate bank)

convert :: Bank -> Money -> Currency -> Validation [String] Money
convert (Bank bank) money toCurr = Validation $
    if currency money == toCurr then
        Right money
    else
        case Map.lookup (currency money, toCurr) bank of
            Nothing -> Left [show (currency money) ++ "->" ++ show toCurr]
            Just rate -> Right (newMoney ((amount money) * rate) toCurr)
