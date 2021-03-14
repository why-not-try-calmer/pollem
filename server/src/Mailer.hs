{-# LANGUAGE OverloadedStrings #-}

module Mailer where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text              as T
import           Network.HTTP.Req

data From = From {
    from_email :: T.Text,
    from_name  :: T.Text
}

data ReplyTo = ReplyTo {
    reply_email :: T.Text,
    reply_name  :: T.Text
}

newtype Content = Content { content :: [T.Text ] }

data EmailData = EmailData {
    _from     :: From,
    _reply_to :: ReplyTo,
    _subject  :: T.Text
}

newtype Personalization = Personalization { personalization :: [T.Text] }

newtype EmailBody = EmailBody {
    _personalizations :: [Personalization]
}

data Email = Email {
    email_url    :: T.Text,
    email_header :: T.Text,
    email_data   :: EmailData
}

sendEmail :: IO ()
sendEmail = runReq defaultHttpConfig $ do
    let h = header "authorization: Bearer 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" "content-type: application/json"
        d = "{'personalizations':[{'to':[{'email':'adrien.glauser@gmail.com','name':'John Doe'}],'subject':'Hello, World!'}],'from':{'email':'mrnycticorax@gmail.com','name':'Géraud Lernais'},'reply_to':{'email':'mrnycticorax@gmail.com','name':'géraud Lernais'}}" :: T.Text
    v <- req POST (https "https://api.sendgrid.com/v3/mail/send" /: "post") (ReqBodyJson d) jsonResponse mempty
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