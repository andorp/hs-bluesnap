module Main where

import Control.Applicative ((<$>))
import Data.List (isSuffixOf)
import System.Directory
import System.FilePath.Posix ((</>))

import Test.Tasty
import Test.Tasty.HUnit
import Bluesnap.Test.Parser

main = do
  xmlParserTests <- mapM readTestData [
      XMLParserTest "Order Response" "test/data/response/order/" testOrderResponseParser
    , XMLParserTest "Order Request"   "test/data/request/order/"  testOrderRequestParser
    , XMLParserTest "Batch-Order Request" "test/data/request/batch-order" testBatchOrderReqParser
    , XMLParserTest "Batch-Order Response" "test/data/response/batch-order" testBatchOrderRspParser
    , XMLParserTest "Fulfillment Response" "test/data/response/fulfillment" testFulfillmentRspParser
    , XMLParserTest "Order-history Response" "test/data/response/order-history" testOrderHistoryRspParser
    , XMLParserTest "Subscription Request" "test/data/request/subscription" testSubscriptionReqParser
    , XMLParserTest "Subscription Response" "test/data/response/subscription" testSubscriptionRspParser
    , XMLParserTest "Subscription-charge Request" "test/data/request/subscription-charge" testSubscriptionChargeReqParser
    , XMLParserTest "Subscription-charge Response" "test/data/response/subscription-charge" testSubscriptionChargeRspParser
    , XMLParserTest "Shopper-Subscriptions Response" "test/data/response/shopper-subscriptions" testShopperSubscriptionsRspParser
    , XMLParserTest "Shopping-Context Request" "test/data/request/shopping-context" testShoppingContextReqParser
    , XMLParserTest "Shopping-Context Response" "test/data/response/shopping-context" testShoppingContextRspParser
    , XMLParserTest "Item-Price Response" "test/data/response/item-price" testItemPriceRspParser
    , XMLParserTest "Product Request" "test/data/request/product" testProductReqParser
    , XMLParserTest "Product Response" "test/data/response/product" testProductRspParser
    ]
  defaultMain $ testGroup "Tests" xmlParserTests

data XMLParserTest = XMLParserTest String FilePath (String -> Assertion)

readTestData (XMLParserTest name dir parserTest) = do
  xmls <- filter (isSuffixOf ".xml") . filter (not . flip elem [".", ".."]) <$> getDirectoryContents dir
  return
    $ testGroup name
    $ map (\name -> let name' = dir </> name
                    in testCase name' $ parserTest name')
      xmls

