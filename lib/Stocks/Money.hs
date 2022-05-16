module Stocks.Money
    ( Currency(..)
    , Money -- type only, no constructor
    , newMoney
    , times
    , divide
    , amount
    , currency
    )
where

data Currency = EUR | USD | KRW deriving (Eq , Show)
data Money = Money { amount :: Float, currency :: Currency }
    deriving (Eq, Show)

newMoney amount currency = Money { amount = amount, currency = currency }

times Money { amount = amt, currency = curr } multiplier =
    Money { amount = amt * multiplier, currency = curr }

divide Money { amount = amt, currency = curr } divisor =
    Money { amount = amt / divisor, currency = curr }
