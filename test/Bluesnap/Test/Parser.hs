module Bluesnap.Test.Parser where

import Test.Tasty.HUnit
import Bluesnap.API.Parser
import Bluesnap.API.Response as Response
import Bluesnap.API.Request as Request

testXMLParser p fname = do
  content <- readFile fname
  let result = parse p content
  case result of
    Left err -> assertFailure err
    Right _ -> return ()

testOrderResponseParser = testXMLParser Response.elementOrder
testOrderRequestParser  = testXMLParser Request.elementOrder

testBatchOrderReqParser = testXMLParser Request.elementBatch_order
testBatchOrderRspParser = testXMLParser Response.elementBatch_order

testFulfillmentRspParser = testXMLParser Response.elementFulfillment

testOrderHistoryRspParser = testXMLParser Response.elementOrder_history

testSubscriptionReqParser = testXMLParser Request.elementSubscription
testSubscriptionRspParser = testXMLParser Response.elementSubscription

testSubscriptionChargeReqParser = testXMLParser Request.elementSubscription_charge
testSubscriptionChargeRspParser = testXMLParser Response.elementSubscription_charge

testShopperSubscriptionsRspParser = testXMLParser Response.elementShopper_subscriptions

testShoppingContextReqParser = testXMLParser Request.elementShopping_context
testShoppingContextRspParser = testXMLParser Response.elementShopping_context

testItemPriceRspParser = testXMLParser Response.elementItem_price

testProductReqParser = testXMLParser Request.elementProduct
testProductRspParser = testXMLParser Response.elementProduct
