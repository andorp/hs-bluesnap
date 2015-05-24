{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Bluesnap.Client (
    module Control.Applicative
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

import           Network.Curl hiding (Header)

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
type Header  = (String, String)

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

-- | Calculates the curl auth parameters and final URL
-- based on the environment for the given path
curlEnv :: Path -> Bluesnap (String, [CurlOption])
curlEnv path = do
  base <- baseURL
  let url = base ++ path
  auth <- basicAuth
  let opts = [ CurlHttpHeaders [
                 "Content-Type: application/xml"
               , "Authorization: " ++ auth
               , "Accept-Charset: UTF-8"
               ]
             ]
  return (url, opts)

-- | Sends an authorized GET request to the bluesnap for the given path
-- and returns the body of the answer on 2xx or throws an error if
-- some exception happened
get :: Path -> Bluesnap Payload
get path = do
  (url, opts) <- curlEnv path
  rsp <- bluesnapIO $ curlGetResponse url opts
  checkCurlErrors rsp
  return (respBody rsp)
  where
    curlGetResponse :: URLString -> [CurlOption] -> IO CurlResponse
    curlGetResponse = curlGetResponse_

-- | Sends an authorized POST request to the bluesnap for the given path
-- and payload returns the headers and body on success or throws an error if
-- some exception happened.
post :: Path -> Payload -> Bluesnap ([Header], Payload)
post path payload = do
  (url, opts) <- curlEnv path
  rsp <- bluesnapIO $ curlPostResponse url opts payload
  checkCurlErrors rsp
  return $ headerAndPayload rsp

-- | Sends an authorized PUT request to the bluesnap for the given path
-- and payload returns the headers and body on success or throws an error if
-- some exception happened.
put :: Path -> Payload -> Bluesnap ([Header], Payload)
put path payload = do
  (url, opts) <- curlEnv path
  rsp <- bluesnapIO $ curlPutResponse url opts payload
  checkCurlErrors rsp
  return $ headerAndPayload rsp

-- | Checks the parsing result for error, if it happened throws
-- a bluesnap ParseError, otherwise returns with the value
checkParse :: Either String a -> Bluesnap a
checkParse (Left e)  = CME.throwError $ ParseError e
checkParse (Right x) = return x

-- * Curl Helpers

curlPostResponse :: URLString -> [CurlOption] -> String -> IO CurlResponse
curlPostResponse url opts payload = curlGetResponse_ url ((CurlPost True):(CurlPostFields [payload]):opts)

curlPutResponse :: URLString -> [CurlOption] -> String -> IO CurlResponse
curlPutResponse url opts payload = curlGetResponse_ url ((CurlPost True):(CurlPostFields [payload]):(CurlCustomRequest "PUT"):opts)
  -- HACK Around: (CurlCustomRequest "PUT") <=> http://sourceforge.net/p/curl/bugs/1349/

headerAndPayload :: CurlResponse_ [Header] String -> ([Header], Payload)
headerAndPayload c = (respHeaders c, respBody c)
