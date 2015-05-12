module Main where

import Control.Applicative ((<$>))
import Data.List (isSuffixOf)
import System.Directory
import System.FilePath.Posix ((</>))

import Test.Tasty
import Test.Tasty.HUnit
import Bluesnap.Test.Parser

main = do
  xmlRequestParserTests <- mapM readTestData [
      XMLParserTest "Order Request"   "test/data/request/order/"  testOrderRequestParser
    , XMLParserTest "Batch-Order Request" "test/data/request/batch-order" testBatchOrderReqParser
    , XMLParserTest "Subscription Request" "test/data/request/subscription" testSubscriptionReqParser
    , XMLParserTest "Subscription-charge Request" "test/data/request/subscription-charge" testSubscriptionChargeReqParser
    , XMLParserTest "Shopping-Context Request" "test/data/request/shopping-context" testShoppingContextReqParser
    , XMLParserTest "Product Request" "test/data/request/product" testProductReqParser
    , XMLParserTest "Catalog-SKU Request" "test/data/request/catalog-sku" testCatalogSkuReqParser
    , XMLParserTest "Custom-parameter Request" "test/data/request/custom-parameter" testCustomParameterReqParser
    , XMLParserTest "Param-decryption Request" "test/data/request/param-decryption" testParamDecryptionReqParser
    , XMLParserTest "Param-encryption Request" "test/data/request/param-encryption" testParamEncryptionReqParser
    ]
  xmlResponseParserTests <- mapM readTestData [
      XMLParserTest "Order Response" "test/data/response/order/" testOrderResponseParser
    , XMLParserTest "Batch-Order Response" "test/data/response/batch-order" testBatchOrderRspParser
    , XMLParserTest "Fulfillment Response" "test/data/response/fulfillment" testFulfillmentRspParser
    , XMLParserTest "Order-history Response" "test/data/response/order-history" testOrderHistoryRspParser
    , XMLParserTest "Subscription Response" "test/data/response/subscription" testSubscriptionRspParser
    , XMLParserTest "Subscription-charge Response" "test/data/response/subscription-charge" testSubscriptionChargeRspParser
    , XMLParserTest "Shopper-Subscriptions Response" "test/data/response/shopper-subscriptions" testShopperSubscriptionsRspParser
    , XMLParserTest "Shopping-Context Response" "test/data/response/shopping-context" testShoppingContextRspParser
    , XMLParserTest "Item-Price Response" "test/data/response/item-price" testItemPriceRspParser
    , XMLParserTest "Product Response" "test/data/response/product" testProductRspParser
    , XMLParserTest "Catalog-SKU Response" "test/data/response/catalog-sku" testCatalogSkuRspParser
    , XMLParserTest "Custom-parameter Response" "test/data/response/custom-parameter" testCustomParameterRspParser
    , XMLParserTest "Coupon-info Response" "test/data/response/coupon-info" testCouponInfoRspParer
    , XMLParserTest "Param-decryption Response" "test/data/response/param-decryption" testParamDecryptionRspParser
    , XMLParserTest "Param-encryption Response" "test/data/response/param-encryption" testParamEncryptionRspParser
    , XMLParserTest "Price Response" "test/data/response/price" testPriceRspParser
    , XMLParserTest "Web-Authentication" "test/data/response/web-authentication" testWebAuthenticationRspParser
    ]
  defaultMain $ testGroup "Tests" [
--      testGroup "Request" xmlRequestParserTests
      testGroup "Response" xmlResponseParserTests
    ]

data XMLParserTest = XMLParserTest String FilePath (String -> Assertion)

readTestData (XMLParserTest name dir parserTest) = do
  xmls <- filter (isSuffixOf ".xml") . filter (not . flip elem [".", ".."]) <$> getDirectoryContents dir
  return
    $ testGroup name
    $ map (\name -> let name' = dir </> name
                    in testCase name' $ parserTest name')
      xmls

