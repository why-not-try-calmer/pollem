# pollem

## How it works

### Landing (route: /)
User lands on website. Javascript takes a fingerprint and stores it in the `pollem` localStorage objet. It also checks that object for a user token. If a token is found, the user is considered authenticated. They have access to the form creation form. If not, they have access to the _Authenticate_ button, which they can use to request a token.

### Exchanging a user token (route: /)
Clicking the button _Authenticate_ checks if the _Email_ field nearby is set, check if it's a valid email address, and `POST` it to the server. The server expects:
```
{
    user_fingerprint :: T.Text,
    user_email :: T.Text
}
```
The server cryptohashes `user_email` and asks the database if `user_email` if there is key named `email:<cryptohashed user_email>`. If there is, the server responds with `Error: email address taken already. Would you like to reset it? (This will ask you to verify the address again)`. Else it users `cryphohashed user_email` as a salt to generate a cipher from `user_email`. The result is a user token, which is first saved to the database as a field within the hashmap keyed `email:<cryptohashed user_email>`, also setting `statuss` to _pending_. The entry now looks like:
```
email:<cryptohashed user_email>
    fingerprint: <fingerprint>
    token: <token>
    status: "pending"
```
Finally the server sends: 
* an email at `user_email` with a `token` parameter set to the value of the actual token
* a response to the client containing both `cryptohashed email` and `token` set to their appropriate values. 
Javascript saves `cryptohashed email` to a variable. When the user clicks _Confirm token_, Javascript completes the request so that both `token` & `user_hash` are passed.

### Verifying (route : verify_email)
`POST` request called from the user's client when they click the _Confirm token_ button. The server checks that `<token>` passed a parameter matches the token under `email:<cryptohashed user_email> token`. If no match, responds with _Error bad token_. Else sets database to
```
email:<cryptohashed user_email>
    fingerprint: <fingerprint>
    token: <token>
    status: "verified"
```
and responds with _Verified__. Javascript finally saves `user_hash` to localStorage, which now looks:
```
{
    user_fingerprint : ...
    user_hash : ...
}
```
### Create a poll_answers (route: submit_create_request)
Clicking the button _Submit poll_ sends a request which the server expects to be:
```
{
    poll_startDate               :: T.Text,
    poll_endDate                 :: Maybe T.Text,
    poll_question                :: T.Text ,
    poll_description             :: T.Text,
    poll_id                      :: Int,
    poll_multiple                :: Bool,
    poll_visible                 :: Bool,
    poll_answers                 :: M.Map T.Text T.Text,
    poll_other_answers           :: Maybe (M.Map T.Text T.Text),
    poll_requires_verified_email :: Bool,
    poll_creator_fingerprint     :: T.Text,
    poll_creator_token           :: T.Text
}
```
2. The server 
