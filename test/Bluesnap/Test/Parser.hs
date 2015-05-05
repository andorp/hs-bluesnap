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

testOrderRequestParser = testXMLParser Request.elementOrder