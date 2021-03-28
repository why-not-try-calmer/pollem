{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module ErrorsReplies where

import qualified Data.Text as T

data ErrT = BadEmail | EmailTaken | PollExists | PollNotExist | PollIncomplete | PollTakenAlready | NoEmptyString | TokenNotExist | Database | UserNotExist

data Err a = Err ErrT a

newtype Ok a = Ok a

toText :: Show a => a -> T.Text -> T.Text
toText v s = T.append s . T.pack . show $ s

encodeError :: (Show a) => Err a -> T.Text
encodeError (Err BadEmail v) = toText v "This email is not formatted property: "
encodeError (Err EmailTaken v) = toText v "This email is not formatted property: "
encodeError (Err PollExists v) = toText v "This poll exists already: "
encodeError (Err PollNotExist v) = toText v "This poll does not exist (anymore): "
encodeError (Err PollIncomplete v) = toText v "Your input is not complete, please complete it: "
encodeError (Err PollTakenAlready v) =  toText v "You've taken this poll already: "
encodeError (Err NoEmptyString _) = toText "" "The string you've submitted was empty."
encodeError (Err TokenNotExist v) =  toText v "This token does not exist (anymore): "
encodeError (Err Database v) = toText v "Sorry, an error occured with the database. What you were looking for couldn't be found."
encodeError (Err UserNotExist v) =  toText v "This user does not exist. Please ask for a new token."

encodeOk :: (Show a) => Ok a -> T.Text
encodeOk (Ok val) = T.pack . show $ val
