{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Mailer where

import           AppData
import           Control.Concurrent     (putMVar, takeMVar)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8)
import           Network.HTTP.Req
--

{-- Mailer types  --}

--
data Content = Content {
   _type  :: T.Text ,
   _value :: T.Text
}
-- $(deriveJSON defaultOptions ''Content)1

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

data SendGridEmail = SendGridEmail {
   header  :: Header,
   payload :: Email
}
--

{-- Mailer functions  --}

--
makeSendGridEmail :: SendGridConfig -> T.Text -> T.Text  -> SendGridEmail
makeSendGridEmail (SendGridBearer bearer) token email =
   let   email_header = oAuth2Bearer bearer
         content = Content "text/plain" $ "Please copy-paste this token to the 'token' field in the application: " `T.append` token
         sender = Addressee "mrnycticorax@gmail.com" "MrNycticorax"
         addressee = Addressee email "Undisclosed Recipient"
         personalization = PersoObject [addressee] "Mille sabords!"
   in    SendGridEmail email_header $ Email sender addressee [personalization] [content]

sendEmail :: SendGridEmail -> IO ()
sendEmail (SendGridEmail header email) = runReq defaultHttpConfig $ do
    resp <- req POST (https "api.sendgrid.com" /: "v3" /: "mail" /: "send" ) (ReqBodyJson email) bsResponse header
    liftIO . print $ responseBody resp
