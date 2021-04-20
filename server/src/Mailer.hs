{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Mailer where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString        as B
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8)
import qualified ErrorsReplies          as R
import           HandlersDataTypes
import           Network.HTTP.Req
import Data.Foldable (foldl')
--

{-- App types  --}

--
newtype SendGridConfig = SendGridBearer { bearer :: B.ByteString }

initSendgridConfig :: SendGridConfig
initSendgridConfig = SendGridBearer "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"
--

{-- Mailer types  --}

--
data Content = Content {
   _type  :: T.Text ,
   _value :: T.Text
}

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
   personalizations :: [PersoObject],
   content          :: [Content]
}

instance ToJSON Email where
   toJSON Email{..} = object [
      "from" .= from,
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
emailToken :: SendGridConfig -> B.ByteString -> B.ByteString -> SendGridEmail
emailToken (SendGridBearer bearer) token email =
   let   email_header = oAuth2Bearer bearer
         content = Content "text/plain" $ "Please copy-paste this token to the 'token' field in the application: " `T.append` decodeUtf8 token
         sender = Addressee "mrnycticorax@gmail.com" "MrNycticorax"
         addressee = Addressee (decodeUtf8 email) "Undisclosed Recipient"
         personalization = PersoObject [addressee] "Mille sabords!"
   in    SendGridEmail email_header $ Email sender [personalization] [content]

sendEmail :: SendGridEmail -> IO (Either (R.Err T.Text) (R.Ok T.Text))
sendEmail (SendGridEmail header email) =
   let   request = req POST (https "api.sendgrid.com" /: "v3" /: "mail" /: "send" ) (ReqBodyJson email) bsResponse  header
   in    runReq defaultHttpConfig request >>= \resp ->
            if responseStatusCode resp == 202 then pure . Right $ R.Ok "Email sent."
            else pure . Left $ R.Err R.SendGridError mempty

emailNotifyOnClose (SendGridBearer bearer) recipients poll author scores =
   let   email_header = oAuth2Bearer bearer
         results =
            let zipped = zip (poll_answers poll) scores
                collectAnswersScores acc (answer, score) = acc ++ ["- " `T.append` answer `T.append` ": " `T.append` (T.pack . show $ score) `T.append` "\n"]
            in  T.concat $ foldl' collectAnswersScores ["Answers:\n"] zipped
         message =
            "*" `T.append` poll_question poll `T.append` "*" `T.append` " created by: " `T.append` decodeUtf8 author `T.append` "\n" `T.append`
            "Ran between: " `T.append` poll_startDate poll `T.append` " and " `T.append` fromMaybe "now" (poll_endDate poll) `T.append`  "\n" `T.append`
            "Description: " `T.append` poll_description poll `T.append` "\n" `T.append`
            "Results " `T.append` results `T.append` "\n"
         content = Content "text/plain" message
         sender = Addressee "mrnycticorax@gmail.com" "MrNycticorax"
         addressee email = Addressee (decodeUtf8 email) "Undisclosed Recipient"
         personalization = PersoObject (map addressee recipients) "Mille sabords!"
   in    SendGridEmail email_header $ Email sender [personalization] [content]