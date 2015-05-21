{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Bluesnap.Client (
    module Control.Applicative
  , module Bluesnap.Model
  , Bluesnap(..)
  , BluesnapError(..)
  , Environment(..)
  , Path
  , Payload
  , runBluesnap
  , get
  , post
  , put
  , checkParse
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import qualified Control.Monad.Except as CME
import qualified Control.Monad.Reader as CMR
import           Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Text

import           Network.Curl

import           Bluesnap.Model

data BluesnapError
  = BluesnapError String
  | CurlError String
  | BSException SomeException
  | ParseError String
  deriving (Show)

data Environment = Environment {
    env_user :: String
  , env_password :: String
  , env_base_url :: String
  , env_merchant_id :: String
  } deriving (Eq, Show)

data Cache = Cache {
    basicAuthCache :: String
  }

-- Craete cached values from environment at the start time
createCache :: Environment -> Cache
createCache env = Cache {
    basicAuthCache = BS.unpack . BS.append "Basic " . B64.encode . BS.pack $ join [
        env_user env, ":", env_password env 
      ]
  }

newtype Bluesnap a = Bluesnap (CMR.ReaderT (Environment, Cache) (CME.ExceptT BluesnapError IO) a)
  deriving (Functor, Applicative, Monad, CME.MonadError BluesnapError, CMR.MonadReader (Environment, Cache))

runBluesnap :: Environment -> Bluesnap a -> IO (Either BluesnapError a)
runBluesnap env (Bluesnap bs) = CME.runExceptT $ CMR.runReaderT bs (env, createCache env)

type Path    = String
type Payload = String

class UrlPath c where
  urlPath :: c -> Path

-- Lifts the IO computation to the bluesnap monad
-- and guards it against the exceptions
bluesnapIO :: IO a -> Bluesnap a
bluesnapIO m = Bluesnap $ do
  r <- lift . lift $ try' m
  case r of
    Left  e -> CME.throwError $ BSException e
    Right a -> return a
  -- TODO: Handle asyncronous exceptions
  where
    try' :: IO a -> IO (Either SomeException a)
    try' = try

baseURL :: Bluesnap String
baseURL = CMR.asks (env_base_url . fst)

basicAuth :: Bluesnap String
basicAuth = CMR.asks (basicAuthCache . snd)

-- | Check for errors in Curl and thow a bluesnap error if found
checkCurlErrors :: CurlResponse -> Bluesnap ()
checkCurlErrors rsp = do
  let curlCode = respCurlCode rsp
  when (isNotOK curlCode) . CME.throwError . CurlError $ join [
      show curlCode, " ", show (respStatus rsp), " ", respStatusLine rsp
    ]
  where
    isNotOK CurlOK = False
    isNotOK _      = True

-- | Sends an authorized GET request to the bluesnap for the given path
-- and returns the body of the answer on 2xx or throws an error if
-- some exception happened
get :: Path -> Bluesnap Payload
get path = do
  base <- baseURL
  let url = base ++ path
  auth <- basicAuth
  let opts = [CurlHttpHeaders [
                  "Content-Type: application/xml"
                , "Authorization: " ++ auth
                , "Accept-Charset: UTF-8"
                ]]
  rsp <- bluesnapIO $ curlGetResponse url opts
  checkCurlErrors rsp
  return (respBody rsp)
  where
    curlGetResponse :: URLString -> [CurlOption] -> IO CurlResponse
    curlGetResponse = curlGetResponse_

-- | Sends an authorized POST request to the bluesnap for the given path
-- and payload returns unit on success or throws an error if
-- some exception happened
post :: Path -> Payload -> Bluesnap ()
post path payload = do
  base <- baseURL
  let url = base ++ path
  auth <- basicAuth
  let opts = [ CurlHttpHeaders [
                   "Content-Type: application/xml"
                 , "Authorization: " ++ auth
                 , "Accept-Charset: UTF-8"
                 ]
             , CurlPost True
             , CurlPostFields [payload]
             ]
  rsp <- bluesnapIO $ curlGetResponse url opts
  checkCurlErrors rsp
  where
    curlGetResponse :: URLString -> [CurlOption] -> IO CurlResponse
    curlGetResponse = curlGetResponse_

put :: Path -> Payload -> Bluesnap ()
put = error "put :: Path -> Payload -> Bluesnap ()"

-- | Checks the parsing result for error, it it happened throws
-- a bluesnap ParseError, otherwise returns with the value
checkParse :: Either String a -> Bluesnap a
checkParse (Left e)  = CME.throwError $ ParseError e
checkParse (Right x) = return x
