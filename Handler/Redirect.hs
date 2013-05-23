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
            --getRecent (uID $ oaUser authToken) (oaAccessToken authToken) def
            getSelfLiked authToken def
          setTitleI MsgLoginOK
          $(widgetFile "redirect_ok")
      Nothing->do
        -- get all error information
        err<-liftM (fromMaybe "") $ lookupGetParam "error"
        reason<-liftM (fromMaybe "") $ lookupGetParam "error_reason"
        description<-liftM (fromMaybe "") $ lookupGetParam "error_description"
        setTitleI MsgLoginFail
        $(widgetFile "redirect_fail")
  