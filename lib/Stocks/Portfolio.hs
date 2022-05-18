module Stocks.Portfolio where

import Data.List (intercalate)

import Stocks.Money
import Stocks.Bank

data Portfolio = Portfolio [Money]

newPortfolio :: Portfolio
newPortfolio = Portfolio []

add :: Portfolio -> Money -> Portfolio
add (Portfolio ms) money = Portfolio (money:ms)

evaluate :: Portfolio -> Bank -> Currency -> Either [Char] Money
evaluate (Portfolio monies) bank targetCurrency =
    let
        convertedMonies = traverse (\money -> convert bank money targetCurrency) monies
        totalUpMoney newMonies = newMoney (sum (map amount newMonies)) targetCurrency
    in
        case fmap totalUpMoney convertedMonies of
            Validation (Left msgs) -> Left ("Missing exchange rate(s): " ++ intercalate ", " msgs)
            Validation (Right money) -> Right money
