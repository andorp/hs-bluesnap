module Main where

import Control.Applicative ((<$>))
import Data.List (isSuffixOf)
import System.Directory
import System.FilePath.Posix ((</>))

import Test.Tasty
import Test.Tasty.HUnit
import Bluesnap.Test.Parser

main = do
  responseOrderTests <- readTestData "Order Responses" "test/data/response/order/" testOrderResponseParser
  requestOrderTests  <- readTestData "Order Request"   "test/data/request/order/"  testOrderRequestParser
  defaultMain $ testGroup "Tests" [
      responseOrderTests
    , requestOrderTests
    ]

readTestData name dir parserTest = do
  xmls <- filter (isSuffixOf ".xml") . filter (not . flip elem [".", ".."]) <$> getDirectoryContents dir
  print xmls
  return
    $ testGroup name
    $ map (\name -> let name' = dir </> name
                    in testCase name' $ parserTest name')
      xmls


{-}
tests :: TestTree
tests = testGroup "Tests" [responseParseTests, requestParseTests]

responseParseTests = testGroup "Response parse tests"
  [ testCase "order.xml" $ testOrderResponseParser "test/data/response/order.xml"
  ]

requestParseTests = testGroup "Request parse tests"
  [ testCase "order.xml" $ testOrderRequestParser "test/data/request/order.xml"
  ]
-}
