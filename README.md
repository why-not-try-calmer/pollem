# pollem
## How it works
### Landing, /
User lands on website. Javascript takes a fingerprint and stores it in the `pollem` localStorage web client object. It also checks that object for a user token. If a token is found, the user is considered authenticated. They have access to the form creation form. If not, they have access to the _Authenticate_ button, which they can use to request a token.
### Getting a user token, /
Clicking the button _Authenticate_ checks if the _Email_ field nearby is set, check if it's a valid email address, and `POST` it to the server. The server expects:
```
{
    user_fingerprint :: T.Text,
    user_email :: T.Text
}
```
The server cryptohashes `user_email` and asks the database for an `email:<cryptohashed user_email>` key. If the key already exists, the server responds with `Error: email address taken already. Would you like to reset it? (This will ask you to verify the address again)`. Otherwise it cryptohashes `user_email` and then use it to salt `user_email` and generate a token. Then:
1. The token is saved to the database as a field within the hashmap keyed `email:<cryptohashed user_email>`.
2. The value of `status` is set to _pending_. The entry now looks like:
```
email:<cryptohashed user_email>
    fingerprint: <fingerprint>
    token: <token>
    verified: False
```
3. Finally the server sends: 
* a response to the client containing both `cryptohashed email` and `token` set to their appropriate values. 
* an email at `user_email` with a `token` parameter set to the value of the actual token

Javascript saves `cryptohashed email` to a variable (not to localStorage). When the user clicks _Confirm token_, Javascript completes the request so that both `token` & `cryptohashed email` are passed.

### Verifying a user token, /verify_email
`POST` request called from the user's client when they click the _Confirm token_ button. The server checks that `<token>` passed a parameter matches the token under `email:<cryptohashed user_email> token`. If no match, responds with _Error bad token_. Else sets database to
```
email:<cryptohashed user_email>
    fingerprint: <fingerprint>
    token: <token>
    verified: True
```
and responds with _Verified_. Javascript finally saves `user_hash` to localStorage, which now looks:
```
{
    user_fingerprint : ...
    user_hash : ...
}
```
### Create a poll_answers, /submit_create_request
Clicking the button _Submit poll_ sends a request which the server expects to be:
```
{
    poll_startDate               :: T.Text,
    poll_endDate                 :: Maybe T.Text,
    poll_question                :: T.Text ,
    poll_description             :: T.Text,
    poll_multiple                :: Bool,
    poll_visible_answers         :: Bool,
    poll_answers                 :: M.Map T.Text T.Text,
    poll_other_answers           :: Maybe (M.Map T.Text T.Text),
    poll_requires_verified_email :: Bool,
    poll_user_hash               :: T.Text
}
```
If `user_hash` exists, and the payload is valid, the server generates a random number as `poll_id` and saves a database representation:
```
poll:<poll_id>
    recipe -- the above as a single JSON string
    startDate -- string
    isActive -- boolean
    authenticatedOnly -- boolean
    participants -- set of fingerprints if not authenticateOnly, of user_emails otherwise
    results -- list of int ordered as the questions, initialized to 0
```
Else it responds with the appropriate error message.
### Displaying a poll, /getpoll)
No authentication of verification is performed. The server checks if `poll_id` passed as `GET` parameter exists. If it does it returns the entire poll. If not it returns an error message.
### Participating to poll, /submit_part_request
When the user submits his choices, one of the following objects is sent:
```
{
    poll_id: ...
    user_fingerprint: ...
    user_hash: ...
    answers: list of bool with true for ticked choice and false otherwise
}
```
or
```
{
    poll_id: ...
    user_fingerprint: ...
    answers: list of bool with true for ticked choice and false otherwise
}
```
The server patterns match on the payload, determining which type it is dealing with. It checks the database for `poll:<poll_id>`. Returns error if:
* the `poll_id` does not exist
* the poll is not longer active
* the poll is set to `authenticateOnly` and one of the following obtains:
    * the payload does not have an `user_email` field; or 
    * the value of this field does not exist under `email:<cryptohashed user_email>`; or
    * the user's `verified` field is not set to `True`
    * `user_email` is in the `participants` set
* the poll is not set to `authenticateOnly` and `user_fingerprint` is in the `participants` set

Otherwise the server traverses the list of int and increment whenever the index matches the index of an answer whose value is `True`. It responds with "Thanks for participating!".
