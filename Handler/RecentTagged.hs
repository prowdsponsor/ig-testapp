{-# LANGUAGE RankNTypes #-}
-- | display list of media recently tagged
module Handler.RecentTagged where

import Import

import Instagram

import Data.Maybe
import Data.Default
import qualified Data.Text as T (null)

-- | the handler takes the auth token of the user and the tag name as parameters
getRecentTaggedR :: Text->Text->Handler RepHtml
getRecentTaggedR auth tag=do
  mNext<-lookupGetParam "next_max_id"
  mMin<-lookupGetParam "next_min_id"
  defaultLayout $
    catchW $ do
        -- (AccessToken auth)
        -- (Just (AccessToken auth))
         emds<-runInstagramInYesod $
            getRecentTagged tag Nothing (def{rtpMaxID=mNext,rtpMinID=mMin})
         let medias=eData emds
         let previous=fromMaybe "" $ getPrevious emds
         let next=fromMaybe "" $ getNext emds 
         setTitleI $ MsgRecentTagged tag
         $(widgetFile "recenttagged")
         
-- | get next id
getNext :: forall d. Envelope d -> Maybe Text
getNext emds=do 
   p <- ePagination emds
   pNextMaxTagID p

-- | get previous id
getPrevious :: forall d. Envelope d -> Maybe Text
getPrevious emds=do 
   p <- ePagination emds
   pMinTagID p