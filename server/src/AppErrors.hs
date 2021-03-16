module AppErrors where

data ErrorType = BadEmail | PollExists | PollNotExist | PollIncomplete

instance Show ErrorType where
    show BadEmail = "Invalid email."
    show PollExists = "Poll exists already."
    show PollNotExist = "Poll does not or no longer exists. Please correct your input."
    show PollIncomplete = "Poll is incomplete. Please correct your input."

main = do
    let err = BadEmail
    case err of
        BadEmail -> print err
        PollExists -> print err