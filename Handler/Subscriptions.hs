{-# LANGUAGE RankNTypes #-}
-- | manage subscriptions
module Handler.Subscriptions where


import Import

import Instagram

-- | simply list the existing subscriptions
getSubscriptionsR :: Handler RepHtml
getSubscriptionsR = 
  defaultLayout $
    catchW showSubscriptionList

-- | delete a given subscription
getDeleteSubscriptionR :: Text -> Handler RepHtml
getDeleteSubscriptionR delID=
  defaultLayout $
    catchW $ do
      _<-runInstagramInYesod $ deleteSubscriptions $ DeleteOne delID
      showSubscriptionList

-- | show the list of current subscriptions
showSubscriptionList :: WidgetT App IO ()   
showSubscriptionList=do
  esubs<-runInstagramInYesod listSubscriptions
  (widget, enctype) <- generateFormPost tagForm
  let subs=eData esubs
  setTitleI MsgSubscriptions
  $(widgetFile "subscriptions")
  
-- | create a tag subscription  
postCreateSubscriptionR ::  Handler RepHtml
postCreateSubscriptionR = do
  ((result, _), _) <- runFormPost tagForm
  case result of
        FormSuccess (TagN t) -> 
          defaultLayout $
            catchW $ do
              -- get callback url
              render <- getUrlRender  
              let igcallback = render CallbackR
              let params=SubscriptionParams (TagRequest t) igcallback media Nothing
              _<-runInstagramInYesod $ createSubscription params
              showSubscriptionList
        _ -> defaultLayout $ catchW showSubscriptionList
  
-- | simple data for creation form
data TagN=TagN Text  

-- | the form for the tag subscription
tagForm ::  forall (m :: * -> *).(MonadHandler m, HandlerSite m ~ App) =>
  Html -> MForm m (FormResult TagN, WidgetT App IO ())
tagForm=renderDivs $ TagN
    <$> areq textField (fs MsgSubscription_Tag) Nothing
    where fs n=FieldSettings (SomeMessage n) (Just $ SomeMessage n) Nothing Nothing []
    