{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module AppErrors where

import qualified Data.Text as T


data ErrorT = BadEmail | PollExists | PollNotExist | PollIncomplete | PollTakenAlready

data Error a = Error !ErrorT a

toText :: Show a => a -> T.Text -> T.Text
toText v s = T.append s . T.pack . show $ s

encodeError :: (Show a) => Error a -> T.Text
encodeError (Error BadEmail v) = toText v "This email is not formmated property: "
encodeError (Error PollExists v) = toText v "This poll exists already: "
encodeError (Error PollNotExist v) = toText v "This poll does not exist (anymore): "
encodeError (Error PollIncomplete v) = toText v "Your input is not complete, please complete it."
encodeError (Error PollTakenAlready v) =  toText v "You've taken this poll already. You cannot take the same poll more than once."

main = do
    let err = Error BadEmail ("adrien" :: String)
    case err of
        Error BadEmail msg -> print msg
