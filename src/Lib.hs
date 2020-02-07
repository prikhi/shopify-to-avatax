{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Lib where


import           Data.Csv
import           Data.Maybe
import           Data.Scientific                ( Scientific )
import           Data.Text                     as T



data ShopifyLine =
    ShopifyLine
          -- Required by AvaTax
        { slDate :: T.Text
        , slId :: T.Text
        , slCustomerId :: T.Text
        , slLineTotal :: Scientific
          -- Required for U.S.A.
        , slShipRegion :: T.Text
        , slShipZip :: T.Text
          -- Required for SST
        , slStreet :: T.Text
        , slCountry :: T.Text
        , slSku :: T.Text
        , slName :: T.Text
        } deriving (Show, Read, Eq)


instance FromNamedRecord ShopifyLine where
    parseNamedRecord r = do
        slDate       <- T.takeWhile (/= ' ') <$> r .: "Created at"
        slId         <- optionalPrefixed $ r .: "Id"
        slCustomerId <- prefixed $ r .: "Name"
        lineQuantity <- r .: "Lineitem quantity"
        linePrice    <- r .: "Lineitem price"
        let slLineTotal = lineQuantity * linePrice
        slShipRegion <- optional $ r .: "Shipping Province"
        slShipZip    <- optional $ r .: "Shipping Zip"
        slStreet     <- optional $ r .: "Shipping Address1"
        slCountry    <- optional $ r .: "Shipping Country"
        slSku        <- r .: "Lineitem sku"
        slName       <- r .: "Lineitem name"
        return ShopifyLine { .. }
      where
        optional         = fmap $ fromMaybe ""
        optionalPrefixed = fmap $ maybe "" ("wholesale-" <>)
        prefixed         = fmap ("wholesale-" <>)
