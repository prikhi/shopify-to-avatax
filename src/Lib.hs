{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveGeneric #-}
module Lib where


import           Data.Char                      ( isDigit )
import           Data.Csv
import           Data.List.Index
import           Data.List.NonEmpty
import           Data.Maybe
import           Data.Scientific                ( Scientific )
import           GHC.Generics                   ( Generic )

import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import qualified Data.ByteString.Lazy          as LBS


-- SHOPIFY

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
          -- Optional
        , slQuantity :: Integer
          -- Store shipping to pop out into another line
        , slShippingCost :: Scientific
          -- Indicates this line should be filtered out
        , slIgnore :: Bool
        } deriving (Show, Read, Eq)


instance FromNamedRecord ShopifyLine where
    parseNamedRecord r = do
        slDate       <- T.takeWhile (/= ' ') <$> r .: "Created at"
        slId         <- optional $ r .: "Id"
        slCustomerId <- r .: "Email"
        slQuantity   <- r .: "Lineitem quantity"
        linePrice    <- r .: "Lineitem price"
        let slLineTotal = fromInteger slQuantity * linePrice
        slShipRegion   <- optional $ r .: "Shipping Province"
        slShipZip <- fmap (T.filter isDigit) . optional $ r .: "Shipping Zip"
        slStreet       <- optional $ r .: "Shipping Address1"
        slCountry      <- optional $ r .: "Shipping Country"
        slName         <- r .: "Lineitem name"
        slSku          <- cleanSku slName <$> r .: "Lineitem sku"
        slShippingCost <- fromMaybe (-9001) <$> r .: "Shipping"
        let slIgnore = T.isPrefixOf "Payment for" slName
        return ShopifyLine { .. }
      where
        optional = fmap $ fromMaybe ""
        cleanSku name sku
            | name == "Garden Guide and Catalog Wire Hanger"
            = "GuideCatalogHanger"
            | T.length sku < 5
            = "0" <> sku
            | otherwise
            = sku


-- | This type contains the data that shopify only exports to the first
-- row/line of each order in the export.
data ShopifyOrder =
    ShopifyOrder
        { soId :: T.Text
        , soShipRegion :: T.Text
        , soShipZip :: T.Text
        , soShipStreet :: T.Text
        , soShipCountry :: T.Text
        -- For shipping line
        , soDate :: T.Text
        , soCustomerId :: T.Text
        , soShippingCost :: Scientific
        } deriving (Show, Read, Eq)


parseShopifyOrders
    :: LBS.ByteString -> Either String [(ShopifyOrder, [ShopifyLine])]
parseShopifyOrders contents = case decodeByName contents of
    Left err -> Left err
    Right (_, sLines) ->
        mapM processLines
            $ groupWith (\l -> (slCustomerId l, slDate l))
            $ Prelude.filter (not . slIgnore)
            $ V.toList sLines

  where
    processLines
        :: NonEmpty ShopifyLine -> Either String (ShopifyOrder, [ShopifyLine])
    processLines (first :| rest) =
        if any (\f -> f first == "")
               [slId, slShipRegion, slShipZip, slStreet, slCountry]
            || (slShippingCost first == (-9001))
        then
            Left
                "Got blank id, region, zip, street, country, shipping cost in first line of order."
        else
            Right
                ( ShopifyOrder { soId           = slId first
                               , soShipRegion   = slShipRegion first
                               , soShipZip      = slShipZip first
                               , soShipStreet   = slStreet first
                               , soShipCountry  = slCountry first
                               , soDate         = slDate first
                               , soCustomerId   = slCustomerId first
                               , soShippingCost = slShippingCost first
                               }
                , first : rest
                )




-- AVATAX

data AvaTaxLine =
    AvaTaxLine
        { processCode :: T.Text
        , docCode :: T.Text
        , docType :: Integer
        , companyCode :: T.Text
        , docDate :: T.Text
        , customerCode :: T.Text
        , lineNo :: T.Text
        , qty :: Integer
        , total :: Scientific
        , itemCode :: T.Text
        , description :: T.Text
        , origRegion :: T.Text
        , origPostCode :: T.Text
        , destAddress :: T.Text
        , destRegion :: T.Text
        , destPostCode :: T.Text
        , destCountry :: T.Text
        , isSellerImporterOfRecord :: T.Text
        } deriving (Show, Read, Eq, Generic)

instance ToNamedRecord AvaTaxLine
instance DefaultOrdered AvaTaxLine

toAvalaraLine :: (ShopifyOrder, [ShopifyLine]) -> [AvaTaxLine]
toAvalaraLine (ShopifyOrder {..}, sLines) = shippingLine : imap convert sLines
  where
    shippingLine :: AvaTaxLine
    shippingLine = AvaTaxLine { processCode              = "3"
                              , docCode                  = soId
                              , docType                  = 1
                              , companyCode              = "SEEDRACKS"
                              , docDate                  = soDate
                              , customerCode             = soCustomerId
                              , lineNo                   = "1"
                              , qty                      = 1
                              , total                    = soShippingCost
                              , itemCode                 = "SHIPPING"
                              , description              = "Wholesale Shipping"
                              , origRegion               = "VA"
                              , origPostCode             = "23117"
                              , destAddress              = soShipStreet
                              , destRegion               = soShipRegion
                              , destPostCode             = soShipZip
                              , destCountry              = soShipCountry
                              , isSellerImporterOfRecord = "TRUE"
                              }

    convert :: Int -> ShopifyLine -> AvaTaxLine
    convert index0 ShopifyLine {..} = AvaTaxLine
        { processCode              = "3"
        , docCode                  = soId
        , docType                  = 1
        , companyCode              = "SEEDRACKS"
        , docDate                  = slDate
        , customerCode             = slCustomerId
        , lineNo                   = T.pack . show $ index0 + 2
        , qty                      = slQuantity
        , total                    = slLineTotal
        , itemCode                 = slSku
        , description              = slName
        , origRegion               = "VA"
        , origPostCode             = "23117"
        , destAddress              = soShipStreet
        , destRegion               = soShipRegion
        , destPostCode             = soShipZip
        , destCountry              = soShipCountry
        , isSellerImporterOfRecord = "TRUE"
        }

encodeAvaTaxLines :: [AvaTaxLine] -> LBS.ByteString
encodeAvaTaxLines = encodeDefaultOrderedByName
