{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module ErrorsReplies where

import qualified Data.Text as T

data ErrT =
    BadEmail | EmailTaken | PollExists | PollNotExist | PollIncomplete |
    PollTakenAlready |EmptyString | TokenNotExist | Database |
    UserNotExist | PollInactive | UserNotVerified | BorkedData | Custom | SendGridError

data Err a = Err ErrT a

newtype Ok a = Ok a

addToText :: Show a => a -> T.Text -> T.Text
addToText v s = T.append s . T.pack . show $ s

renderError :: (Show a) => Err a -> T.Text
renderError (Err BadEmail v) = addToText v "This email is not formatted property: "
renderError (Err BorkedData _) = "Cannot work on this probably corrupted data."
renderError (Err Custom v) = addToText v mempty
renderError (Err Database _) = "Database error: Couldn't satisfy server request."
renderError (Err EmailTaken _) = "Bear in mind that this email address is registered already. I will send you a verification email nonetheless. You don't need to use it unless you have issues authenticating on this application."
renderError (Err EmptyString _) = "The string you've submitted was empty."
renderError (Err PollExists v) = addToText v "This poll exists already: "
renderError (Err PollInactive v) = addToText v "You've tried to participate to an inactive poll."
renderError (Err PollNotExist v) = addToText v "This poll does not exist (anymore): "
renderError (Err PollIncomplete v) = addToText v "Your input is not complete, please complete it: "
renderError (Err PollTakenAlready v) = addToText v "You've taken this poll already: "
renderError (Err SendGridError v) = addToText v "Request to Sengrid failed, replied with error code."
renderError (Err TokenNotExist v) =  addToText v "This token does not exist (anymore): "
renderError (Err UserNotExist _) =  "This user does not exist. Please ask for a new token."
renderError (Err UserNotVerified _) = "We've got your email address already, but you haven't confirmed your token. Please check your emails."

renderOk :: (Show a) => Ok a -> T.Text
renderOk (Ok val) = T.pack . show $ val
