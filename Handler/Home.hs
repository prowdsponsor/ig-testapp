{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Default.Config (appExtra)

import Instagram

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    y <- getYesod
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
        igredirect=extraIGRedirect $ appExtra $ settings y
    url<-runInstragramInYesod $
          getUserAccessTokenURL1 igredirect []
        
    defaultLayout $ do
        aDomId <- newIdent
        setTitleI MsgTitle
        $(widgetFile "homepage")

