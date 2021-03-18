{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module AppErrors where

import qualified Data.Text as T

data ErrorT = BadEmail | PollExists | PollNotExist | PollIncomplete | PollTakenAlready | NoEmptyString | TokenNotExist | Database

data Error a = Error ErrorT a

toText :: Show a => a -> T.Text -> T.Text
toText v s = T.append s . T.pack . show $ s

encodeError :: (Show a) => Error a -> T.Text
encodeError (Error BadEmail v) = toText v "This email is not formatted property: "
encodeError (Error PollExists v) = toText v "This poll exists already: "
encodeError (Error PollNotExist v) = toText v "This poll does not exist (anymore): "
encodeError (Error PollIncomplete v) = toText v "Your input is not complete, please complete it: "
encodeError (Error PollTakenAlready v) =  toText v "You've taken this poll already: "
encodeError (Error NoEmptyString _) = toText "" "The string you've submitted was empty."
encodeError (Error TokenNotExist v) =  toText v "This token does not exist (anymore): "
encodeError (Error Database v) = toText v "Sorry, an error occured with the database. What you were looking for couldn't be found."
