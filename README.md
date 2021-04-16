## Dev notes
### Heroku
Add or update project to heroku repo:
* heroku git:remote -a my-app
* git subtree push --prefix path/to/subdirectory heroku master
* if stuck do
```
git push heroku `git subtree split --prefix server/ master`:master --force" # (might need bash)
```
### Redis
Use the reddis cli as node: 
```
rdcli -h redis-18910.c247.eu-west-1-1.ec2.cloud.redislabs.com -p 18910 -a PGO5OZQ9M5UFVU2JYfNQUMnnXcMCWtOh
```
### Live demo
* client: https://hardcore-hopper-66afd6.netlify.app/
* server & API endpoint: https://pollem-now.herokuapp.com/
### To do
* [x] add env vars

```
<ul>
        <li>Hash {{ user.hash }}</li>
        <li>Email {{ user.email }}</li>
        <li>Token {{ user.token }}</li>
        <li>Fingerprint {{ user.fingerprint }}</li>
        <li>Taken: {{ user.taken }}</li>
        <li>Created: {{ user.created }}</li>
        <li>MyPoll: {{ mypolls }}</li>
</ul>
```
