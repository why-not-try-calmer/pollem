# pollem

## Creating a polling
1. Open the URL.
2. Set fields.
3. Share link.

## Frontend
### Participation
- get user's fingerprint
- send to server
### Creation
- send poll fields to backend

## Backend
- default "naked" route: render web page for creating survey
- GET "take poll" route: 
    - if poll id does not exist, respond with ERROR "no such poll id"
        - otherwise render page
- POST "submit poll" route: 
    - match signature (client id = {cookie + localdb} + client fingerprint + poll id) against redis
    - if poll id does not exist respond with ERROR "no such poll id"
    - if poll id exists and client id or client fingerprint exists on poll id and user state is "taken", respond with ERROR "survey taken already, but you can create a new survey by clicking here"
    - check payload:
        - if payload is complete, respond with ERROR "incomplete form; please finish taking the survey or come back in 5 minutes to reset"
        - else set user state to "taken", write payload to user state, respond with OK "thank you"