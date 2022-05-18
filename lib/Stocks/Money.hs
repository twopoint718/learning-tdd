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

data Currency = EUR | USD | KRW | Kalganid deriving (Eq, Ord, Show)
data Money = Money { amount :: Float, currency :: Currency }
    deriving (Eq, Show)

newMoney :: Float -> Currency -> Money
newMoney amt curr = Money { amount = amt, currency = curr }

times :: Money -> Float -> Money
times Money { amount = amt, currency = curr } multiplier =
    Money { amount = amt * multiplier, currency = curr }

divide :: Money -> Float -> Money
divide Money { amount = amt, currency = curr } divisor =
    Money { amount = amt / divisor, currency = curr }
