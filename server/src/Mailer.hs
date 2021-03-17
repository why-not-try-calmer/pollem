{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}


module Mailer where

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

type Header = Option 'Https

makeSendGridEmail :: (Header, Email)
makeSendGridEmail =
   let   email_header = oAuth2Bearer "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"
         content = Content "text/plain" "Ca ne va pas se passer comme ca!"
         sender = Addressee "mrnycticorax@gmail.com" "Geraud Lernais"
         addressee = Addressee "adrien.glauser@gmail.com" "Adrien Glauser"
         personalization = PersoObject [addressee] "Mille sabords!"
   in (email_header, Email sender addressee [personalization] [content])

sendEmail :: (Header, Email) -> IO ()
sendEmail (header, email) = runReq defaultHttpConfig $ do
    resp <- req POST (https "api.sendgrid.com" /: "v3" /: "mail" /: "send" ) (ReqBodyJson email) bsResponse header
    liftIO $ print (responseBody resp)

main :: IO ()
main = sendEmail makeSendGridEmail
