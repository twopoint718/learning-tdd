module Stocks.Portfolio where

import Stocks.Money

data Portfolio = Portfolio [Money]

add (Portfolio ms) money = Portfolio (money:ms)

evaluate (Portfolio monies) currency =
    newMoney (sum (map amount monies)) currency
