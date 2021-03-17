{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Mailer where

import           Control.Exception      (SomeException, try)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text              as T
import           Network.HTTP.Req

data Content = Content {
   _type  :: T.Text ,
   _value :: T.Text
}
-- $(deriveJSON defaultOptions ''Content)

instance ToJSON Content where
   toJSON Content{..} =
      object [
      "type" .= _type,
      "value" .= _value
      ]

data Addressee = Addressee {
   email :: T.Text,
   name  :: T.Text
}
$(deriveJSON defaultOptions ''Addressee)

data DynamicTemplate = DynamicTemplate {
   verb             ::  T.Text,
   adjective        :: T.Text ,
   noun             :: T.Text,
   currentDayOfWeek :: T.Text
}
$(deriveJSON defaultOptions ''DynamicTemplate)

data PersoObject = PersoObject {
   to      :: [Addressee],
   -- dynamic_template_data :: DynamicTemplate,
   subject :: T.Text
}
$(deriveJSON defaultOptions ''PersoObject)

data Email = Email {
   from             :: Addressee,
   reply_to         :: Addressee,
   personalizations :: [PersoObject],
   content          :: [Content]
}
-- $(deriveJSON defaultOptions ''Email)

instance ToJSON Email where
   toJSON Email{..} = object [
      "from" .= from,
      "reply_to" .= reply_to,
      "personalizations" .= personalizations,
      "content" .= content
      ]

sendEmail :: IO ()
sendEmail = runReq defaultHttpConfig $ do
    let email_header = oAuth2Bearer "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"
        content = Content "text/plain" "Ca ne va pas se passer comme ca!"
        sender = Addressee "mrnycticorax@gmail.com" "Geraud Lernais"
        addressee = Addressee "adrien.glauser@gmail.com" "Adrien Glauser"
        -- dyn_templ = DynamicTemplate "" "" "" ""
        personalization = PersoObject [addressee] "Mille sabords!"
        email = Email sender addressee [personalization] [content]
    liftIO . print $ encode email
    resp <- req POST (https "api.sendgrid.com" /: "v3" /: "mail" /: "send" ) (ReqBodyJson email) bsResponse  email_header
    -- liftIO $ print (responseBody resp :: Value)
    liftIO $ print (responseBody resp)

main = sendEmail