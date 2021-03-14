{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Mailer where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text              as T
import           Network.HTTP.Req

data From = From {
    from_email :: T.Text,
    from_name  :: T.Text
}
$(deriveJSON defaultOptions ''From)

data ReplyTo = ReplyTo {
    reply_email :: T.Text,
    reply_name  :: T.Text
}
$(deriveJSON defaultOptions ''ReplyTo)

newtype Content = Content { content :: [T.Text ] }
$(deriveJSON defaultOptions ''Content)

data EmailData = EmailData {
    _from     :: From,
    _reply_to :: ReplyTo,
    _subject  :: T.Text
}
$(deriveJSON defaultOptions ''EmailData)

newtype Personalization = Personalization { personalization :: [T.Text] }
$(deriveJSON defaultOptions ''Personalization)

newtype EmailBody = EmailBody {
    _personalizations :: [Personalization]
}
$(deriveJSON defaultOptions ''EmailBody)

data Email = Email {
    email_url    :: T.Text,
    email_header :: T.Text,
    email_data   :: EmailData
}
$(deriveJSON defaultOptions ''Email)

sendEmail :: IO ()
sendEmail = runReq defaultHttpConfig $ do
    let h = oAuth2Bearer "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"
        d = "{'personalizations':[{'to':[{'email':'adrien.glauser@gmail.com','name':'John Doe'}],'subject':'Hello, World!'}],'from':{'email':'mrnycticorax@gmail.com','name':'Géraud Lernais'},'reply_to':{'email':'mrnycticorax@gmail.com','name':'géraud Lernais'}}" :: T.Text
    v <- req POST (https "https://api.sendgrid.com/v3/mail/send" /: "post" ) (ReqBodyJson d) jsonResponse h
    liftIO $ print (responseBody v :: Value)

main = sendEmail


{-

{
   "personalizations":[
      {
         "to":[
            {
               "email":"john.doe@example.com",
               "name":"John Doe"
            }
         ],
         "dynamic_template_data":{
            "verb":"",
            "adjective":"",
            "noun":"",
            "currentDayofWeek":""
         },
         "subject":"Hello, World!"
      }
   ],
   "from":{
      "email":"noreply@johndoe.com",
      "name":"John Doe"
   },
   "reply_to":{
      "email":"noreply@johndoe.com",
      "name":"John Doe"
   },
   "template_id":"<<YOUR_TEMPLATE_ID>>"
}

-}