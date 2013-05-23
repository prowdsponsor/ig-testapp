{-# LANGUAGE ScopedTypeVariables #-}
-- | callback for real-time subscriptions
module Handler.SubscriptionCallback where

import Import
import Instagram as IG
import Data.Aeson
import Network.Wai (requestHeaders)

import Data.Conduit
import Data.Conduit.Binary (sinkHandle)
import System.IO (stdout)

-- | the get is done by Instagram when the subscription is created
-- we only need to echo the challenge
-- TODO have a verify token verification routine
getCallbackR :: Handler RepPlain
getCallbackR = do
  mode<-lookupGetParam "hub.mode"
  case mode of
    Just "subscribe"-> do
      challenge<-lookupGetParam "hub.challenge"
      case challenge of  
        Just clg->return $ RepPlain $ toContent clg
        _->invalidArgs ["hub.challenge"]
    _->invalidArgs ["hub.mode"]
 
-- | the post is done by Instagram when a real time notification occurs
-- this is where you need to do something with the update (in a new thread, since you only have 2 seconds to return)
-- TODO X-Hub-Signature verification
postCallbackR :: Handler RepJson
postCallbackR = do
  wr<-waiRequest
  let rh=requestHeaders wr
      sign=filter (\(n,_)-> ("X-Hub-Signature" == n) ) rh
  --  for now, just dump
  liftIO $ print "Signature"
  liftIO $ print sign
  rawRequestBody $$  sinkHandle stdout
  r::(Result [IG.Update])<-parseJsonBody
  liftIO $ print "Updates"
  liftIO $ print r
  -- spec doesn't say anything about what the post should send back
  return $ RepJson $ toContent $ object []