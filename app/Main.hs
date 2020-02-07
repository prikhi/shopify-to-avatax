module Main where

import           Lib

import qualified Data.ByteString.Lazy          as LBS

main :: IO ()
main = do
    contents <- LBS.readFile "orders_export_1.csv"
    let shopifyOrders = either error id $ parseShopifyOrders contents
        avataxLines   = concatMap toAvalaraLine shopifyOrders
    LBS.writeFile "avatax-wholesale-transactions.csv"
        $ encodeAvaTaxLines avataxLines
