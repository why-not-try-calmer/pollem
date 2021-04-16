### Dev notes
* heroku git:remote -a my-app
* git subtree push --prefix path/to/subdirectory heroku master
* if stuck, git push heroku `git subtree split --prefix server/ master`:master --force (might need bash)

### Live demo
* client: https://hardcore-hopper-66afd6.netlify.app/
* server & API endpoint: https://pollem-now.herokuapp.com/

### To do
* add env vars

<ul>
        <li>Hash {{ user.hash }}</li>
        <li>Email {{ user.email }}</li>
        <li>Token {{ user.token }}</li>
        <li>Fingerprint {{ user.fingerprint }}</li>
        <li>Taken: {{ user.taken }}</li>
        <li>Created: {{ user.created }}</li>
        <li>MyPoll: {{ mypolls }}</li>
    </ul>
