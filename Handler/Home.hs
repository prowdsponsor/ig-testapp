{-# LANGUAGE TupleSections, OverloadedStrings #-}
-- | home page
module Handler.Home where

import Import

import Instagram

-- | displays the login action
getHomeR :: Handler RepHtml
getHomeR = do
     -- get the redirect url
    render <- getUrlRender  
    let igredirect = render RedirectR
    url<-runInstagramInYesod $
          -- Relationships
          getUserAccessTokenURL1 igredirect []
        
    defaultLayout $ do
        aDomId <- newIdent
        setTitleI MsgTitle
        $(widgetFile "homepage")

