-- | the page handling the redirection from Instagram
module Handler.Redirect where

import Import

import Instagram

import Data.Maybe (fromMaybe)
import Data.Default (def)
import Control.Monad (liftM)

-- | handle the redirect
getRedirectR :: Handler RepHtml
getRedirectR=do
  mCode<-lookupGetParam "code"
  -- get the redirect url
  render <- getUrlRender  
  let igredirect = render RedirectR
  defaultLayout $
    case mCode of
      Just code->catchW $ do
          -- perform step 2: get auth token with the given code
          authToken<-runInstagramInYesod $
            getUserAccessTokenURL2 igredirect code
          -- a simple query to show the user is logged in
          emds<-runInstagramInYesod $
            -- getRecent (uID $ oaUser authToken) authToken def
            -- getPopularMedia Nothing
            -- getComments "somemediaid" Nothing
            -- getSelfLiked authToken 
            -- _<-like "somemediaid" authToken
            -- getLikes "somemediaid" Nothing
            -- getMedia "somemediaid" Nothing
            -- Just 13.743136919, lLongitude = Just 100.562324524
            -- searchLocations (Just authToken) (def{lspLatitude = Just 13.743136919, lspLongitude = Just 100.562324524, lspDistance=Just 10})
            -- searchMedia Nothing (def{mspLatitude = Just 13.743136919, mspLongitude = Just 100.562324524, mspDistance=Just 1000})
            -- getFollows (uID $ oaUser authToken) Nothing
            -- getFollowedBy "someuserid" (Just authToken)
            -- getRequestedBy authToken
            -- getRelationship "someuserid" authToken
            -- setRelationShip "someuserid" authToken Follow
            -- getLocation "4318380" Nothing
            -- getLocationRecentMedia  "4318380" Nothing def
            -- 3270684
            getGeographyRecentMedia "geography id given by object_id from subscription" def
            --getSelfFeed authToken def
            -- searchUsers Nothing (UserSearchParams "jack" (Just 3))
            -- getTag "ouch" Nothing -- authToken
            -- getUser (uID $ oaUser authToken) Nothing
            -- searchTags "love" Nothing -- authToken
            -- getRecentTagged "ouch" (Just authToken) def
          -- data for the link to RecentTagged
          let (AccessToken auth)=oaAccessToken authToken
          let tag="ouch"
          setTitleI MsgLoginOK
          $(widgetFile "redirect_ok")
      Nothing->do
        -- get all error information
        err<-liftM (fromMaybe "") $ lookupGetParam "error"
        reason<-liftM (fromMaybe "") $ lookupGetParam "error_reason"
        description<-liftM (fromMaybe "") $ lookupGetParam "error_description"
        setTitleI MsgLoginFail
        $(widgetFile "redirect_fail")
  