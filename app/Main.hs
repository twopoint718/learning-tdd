module Main where

import Test.Hspec

data Dollar = Dollar { amount :: Int }

times :: Dollar -> Int -> Dollar
times Dollar { amount = amt } multiplier =
    Dollar { amount = amt * multiplier }

main :: IO ()
main = hspec $
    describe "Money" $
        it "multiplication" $ do
            let fiver = Dollar { amount = 5 }
            let tenner = fiver `times` 2
            amount tenner `shouldBe` 10
