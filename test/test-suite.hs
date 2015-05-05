module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Bluesnap.Test.Parser

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [responseParseTests, requestParseTests]

responseParseTests = testGroup "Response parse tests"
  [ testCase "order.xml" $ testOrderResponseParser "test/data/response/order.xml"
  ]

requestParseTests = testGroup "Request parse tests"
  [ testCase "order.xml" $ testOrderRequestParser "test/data/request/order.xml"
  ]
