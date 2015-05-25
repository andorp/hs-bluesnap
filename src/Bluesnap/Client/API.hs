{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Bluesnap.Client.API where

import           Data.Char (toLower)
import           Data.List (find)

import qualified Bluesnap.API.Request  as Request
import qualified Bluesnap.API.Response as Response
import           Bluesnap.API.XML
import           Bluesnap.Client hiding (get, post, put)
import qualified Bluesnap.Client as Client (get, post, put)

class Object o where
  data ObjectID o
  data ObjectRequest o
  data ObjectResponse o
  servicePath       :: o                -> Path
  idToPath          :: ObjectID o       -> Path
  responseParser    :: o                -> XMLParser (ObjectResponse o)
  fromResponse      :: ObjectResponse o -> Either String o
  toRequest         :: o                -> ObjectRequest o
  requestToContents :: ObjectRequest o  -> [Content ()]
  oidFromLocation   :: String           -> ObjectID o

-- | Type inference trick to get out the object type from a given object id type
fakeObj :: (Object o) => ObjectID o -> o
fakeObj = error "fakeObj :: (Object o) => ObjectID o -> o"

fullPath :: (Object o) => ObjectID o -> Path
fullPath oid = concat [servicePath (fakeObj oid), idToPath oid]

-- | Gets the object from Bluesnap based on the given id.
get :: Object o => (ObjectID o) -> Bluesnap o
get oid = do
  let parser = responseParser (fakeObj oid)
  payload <- Client.get (fullPath oid)
  responseXML <- checkParse $ parse parser payload
  checkParse $ fromResponse responseXML

-- | Saves the object in Bluesnap and returns its id based on
-- the location header.
save :: Object o => o -> Bluesnap (ObjectID o)
save obj = do
  payload <- fmap render . safeHead . requestToContents $ toRequest obj
  (headers, payload) <- Client.put (servicePath obj) payload
  location <- getLocationValue headers
  return $ oidFromLocation location
  where
    safeHead :: [a] -> Bluesnap a
    safeHead []    = throwError $ BluesnapError "safeHead failed"
    safeHead (a:_) = return a

update :: Object o => (ObjectID o) -> o -> Bluesnap ()
update oid obj = error "update :: Object o => (ObjectID o) -> o -> Bluesnap ()"

-- | Get the value of location header or throws a Bluesnap error
getLocationValue :: [Header] -> Bluesnap String
getLocationValue hs = maybe
  (throwError $ BluesnapError "Location header is not found!")
  (return . snd)
  (find (("location"==) . map toLower . fst) hs)

data Subscription = Subscription String
  deriving (Eq, Show)

instance Object Subscription where
  data ObjectID Subscription = SubscriptionID String
  data ObjectRequest Subscription = SubsReq { unSubsReq :: Request.Subscription }
  data ObjectResponse Subscription = SubsResp { unSubsResp :: Response.Subscription }
  servicePath _ = "/services/2/subscriptions/"
  idToPath (SubscriptionID value) = value 
  responseParser _ = fmap SubsResp $ Response.elementSubscription
  fromResponse (SubsResp r) = Right $ Subscription (show r)
  toRequest s = error "toRequest s"
  requestToContents = Request.elementToXMLSubscription . unSubsReq
  oidFromLocation = SubscriptionID -- TODO: Last part of the path
