{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module ErrorsReplies where

import qualified Data.Text as T

data ErrT =
    BadEmail | EmailTaken | PollExists | PollNotExist | PollIncomplete |
    PollTakenAlready |EmptyString | TokenNotExist | Database |
    UserNotExist | PollInactive | UserNotVerified | BorkedData | Custom | SendGridError | DatetimeFormat | BadSecret deriving (Eq, Show)

data Err a = Err ErrT a deriving (Eq, Show)

newtype Ok a = Ok a

addToText :: Show a => T.Text -> a -> T.Text
addToText v = T.append v . T.pack . show

renderError :: (Show a) => Err a -> T.Text
renderError (Err BadEmail v) = addToText "This email is not formatted property: " v
renderError (Err BadSecret _) = "This poll is private but your secret does not match ours. Sorry."
renderError (Err BorkedData _) = "Cannot work on this probably corrupted data."
renderError (Err Custom v) = addToText mempty v
renderError (Err Database _) = "Database error: Couldn't satisfy server request."
renderError (Err DatetimeFormat _) = "Unable to parse the date, aborting now."
renderError (Err EmailTaken _) = "Bear in mind that this email address is registered already. I will send you a verification email nonetheless. You don't need to use it unless you have issues authenticating on this application."
renderError (Err EmptyString _) = "The string you've submitted was empty."
renderError (Err PollExists v) = addToText "This poll exists already: " v
renderError (Err PollInactive v) = addToText "You've tried to participate to an inactive poll." v
renderError (Err PollNotExist v) = addToText "This poll does not exist (anymore): " v
renderError (Err PollIncomplete v) = addToText "Your input is not complete, please complete it: " v
renderError (Err PollTakenAlready v) = addToText "You've taken this poll already: " v
renderError (Err SendGridError v) = addToText "Request to Sengrid failed, replied with error code." v
renderError (Err TokenNotExist v) =  addToText "This token does not exist (anymore): " v
renderError (Err UserNotExist _) =  "This user does not exist. Please ask for a new token."
renderError (Err UserNotVerified _) = "We've got your email address already, but you haven't confirmed your token. Please check your emails."

renderOk :: (Show a) => Ok a -> T.Text
renderOk (Ok val) = T.pack . show $ val
