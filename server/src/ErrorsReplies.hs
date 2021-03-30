{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module ErrorsReplies where

import qualified Data.Text as T

data ErrT =
    BadEmail | EmailTaken | PollExists | PollNotExist | PollIncomplete |
    PollTakenAlready |NoEmptyString | TokenNotExist | Database |
    UserNotExist | PollInactive | UserUnverified | BorkedData

data Err a = Err ErrT a

newtype Ok a = Ok a

addToText :: Show a => a -> T.Text -> T.Text
addToText v s = T.append s . T.pack . show $ s

renderError :: (Show a) => Err a -> T.Text
renderError (Err BadEmail v) = addToText v "This email is not formatted property: "
renderError (Err EmailTaken v) = addToText v "This email is not formatted property: "
renderError (Err PollExists v) = addToText v "This poll exists already: "
renderError (Err PollNotExist v) = addToText v "This poll does not exist (anymore): "
renderError (Err PollIncomplete v) = addToText v "Your input is not complete, please complete it: "
renderError (Err PollTakenAlready v) =  addToText v "You've taken this poll already: "
renderError (Err NoEmptyString _) = "The string you've submitted was empty."
renderError (Err TokenNotExist v) =  addToText v "This token does not exist (anymore): "
renderError (Err Database _) = "Database error: Couldn't satisfy server request."
renderError (Err UserNotExist v) =  addToText v "This user does not exist. Please ask for a new token."
renderError (Err UserUnverified v) = addToText v "We've got your email address already, but you haven't confirmed your token. Please check your emails."
renderError (Err PollInactive v) = addToText v "You've tried to participate to an inactive poll."
renderError (Err BorkedData _) = "Cannot work on this probably corrupted data."

renderOk :: (Show a) => Ok a -> T.Text
renderOk (Ok val) = T.pack . show $ val
