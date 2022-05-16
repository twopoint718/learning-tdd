module Stocks.Portfolio where

import Stocks.Money

data Portfolio = Portfolio [Money]

newPortfolio = Portfolio []

add (Portfolio ms) money = Portfolio (money:ms)

evaluate (Portfolio monies) targetCurrency =
    newMoney (sum (map (\m -> convert m targetCurrency) monies)) targetCurrency

convert money targetCurrency =
    if currency money == targetCurrency then
        amount money
    else
        amount money * eurToUsd
  where
    eurToUsd = 1.2
