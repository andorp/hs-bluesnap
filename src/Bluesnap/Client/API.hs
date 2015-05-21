{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Bluesnap.Client.API where {- (
    get
  , save
  , update
  ) where -}

import           Bluesnap.API.Parser
import qualified Bluesnap.API.Request  as Request
import qualified Bluesnap.API.Response as Response
import           Bluesnap.Client hiding (get, post, put)
import qualified Bluesnap.Client as Client (get, post, put)

class Object o where
  data ObjectID o
  type ObjectRequest o
  type ObjectResponse o
  servicePath  :: o -> Path
  idToPath :: ObjectID o -> Path
  responseParser :: o -> XMLParser (ObjectResponse o)
  requestParser :: o -> XMLParser (ObjectRequest o)
  fromResponse :: ObjectResponse o -> Either String o
  toRequest :: o -> ObjectRequest o

fakeObj :: (Object o) => ObjectID o -> o
fakeObj = error "fakeObj :: (Object o) => ObjectID o -> o"

fullPath :: (Object o) => ObjectID o -> Path
fullPath oid = concat [servicePath (fakeObj oid), idToPath oid]

get :: Object o => (ObjectID o) -> Bluesnap o
get oid = do
  let parser = responseParser (fakeObj oid)
  payload <- Client.get (fullPath oid)
  responseXML <- checkParse $ parse parser payload
  checkParse $ fromResponse responseXML

save :: Object o => o -> Bluesnap (ObjectID o)
save obj = error "save :: Object o => o -> Bluesnap (ObjectID o)"

update :: Object o => (ObjectID o) -> o -> Bluesnap ()
update oid obj = error "update :: Object o => (ObjectID o) -> o -> Bluesnap ()"

data Subscription = Subscription String
  deriving (Eq, Show)

instance Object Subscription where
  data ObjectID Subscription = SubscriptionID String
  type ObjectRequest Subscription = Request.Subscription
  type ObjectResponse Subscription = Response.Subscription
  servicePath _ = "/services/2/subscriptions/"
  idToPath (SubscriptionID value) = value 
  responseParser _ = Response.elementSubscription
  requestParser _ = Request.elementSubscription
  fromResponse r = Right $ Subscription (show r)
  toRequest s = error "toRequest s"
