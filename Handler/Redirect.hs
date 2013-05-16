module Handler.Redirect where

import Import
import Yesod.Default.Config (appExtra)

import Instagram

import qualified Data.Map as DM
import Data.Maybe (fromMaybe)


getRedirectR :: Handler RepHtml
getRedirectR=do
  req<-getRequest
  y<-getYesod
  let params=DM.fromList $ reqGetParams req
      igredirect=extraIGRedirect $ appExtra $ settings y
  defaultLayout $ do
    case DM.lookup "code" params of
      Just code->catchW $ do
          authToken<-runInstragramInYesod $
            getUserAccessTokenURL2 igredirect code
          setTitleI MsgLoginOK
          $(widgetFile "redirect_ok")
      Nothing->do
        let err=fromMaybe "" $ DM.lookup "error" params
        let reason=fromMaybe "" $ DM.lookup "error_reason" params
        let description=fromMaybe "" $ DM.lookup "error_description" params
        setTitleI MsgLoginFail
        $(widgetFile "redirect_fail")
  
-- code=CODE
--  error=access_denied&error_reason=user_denied&error_description