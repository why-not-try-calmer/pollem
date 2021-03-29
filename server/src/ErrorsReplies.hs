{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module ErrorsReplies where

import qualified Data.Text as T

data ErrT =
    BadEmail | EmailTaken | PollExists | PollNotExist | PollIncomplete |
    PollTakenAlready |NoEmptyString | TokenNotExist | Database |
    UserNotExist | PollInactive | UserUnverified

data Err a = Err ErrT a

newtype Ok a = Ok a

addToText :: Show a => a -> T.Text -> T.Text
addToText v s = T.append s . T.pack . show $ s

encodeError :: (Show a) => Err a -> T.Text
encodeError (Err BadEmail v) = addToText v "This email is not formatted property: "
encodeError (Err EmailTaken v) = addToText v "This email is not formatted property: "
encodeError (Err PollExists v) = addToText v "This poll exists already: "
encodeError (Err PollNotExist v) = addToText v "This poll does not exist (anymore): "
encodeError (Err PollIncomplete v) = addToText v "Your input is not complete, please complete it: "
encodeError (Err PollTakenAlready v) =  addToText v "You've taken this poll already: "
encodeError (Err NoEmptyString _) = addToText "" "The string you've submitted was empty."
encodeError (Err TokenNotExist v) =  addToText v "This token does not exist (anymore): "
encodeError (Err Database _) = "Database error: Couldn't satisfy server request."
encodeError (Err UserNotExist v) =  addToText v "This user does not exist. Please ask for a new token."
encodeError (Err UserUnverified v) = addToText v "We've got your email address already, but you haven't confirmed your token. Please check your emails."
encodeError (Err PollInactive v) = addToText v "You've tried to participate to an inactive poll."

encodeOk :: (Show a) => Ok a -> T.Text
encodeOk (Ok val) = T.pack . show $ val
