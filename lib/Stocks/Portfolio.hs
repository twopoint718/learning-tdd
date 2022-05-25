{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances #-}
module Stocks.Portfolio where

import Data.List (intercalate)
import GHC.Exts (IsList)

import Stocks.Money
import Stocks.Bank

newtype Portfolio = Portfolio [Money]
    deriving IsList

evaluate :: Portfolio -> Bank -> Currency -> Either [Char] Money
evaluate (Portfolio monies) bank targetCurrency =
    let
        convertedMonies = traverse (\money -> convert bank money targetCurrency) monies
        totalUpMoney newMonies = newMoney (sum (map amount newMonies)) targetCurrency
    in
        case fmap totalUpMoney convertedMonies of
            Validation (Left msgs) -> Left ("Missing exchange rate(s): " ++ intercalate ", " msgs)
            Validation (Right money) -> Right money
