# Dartscore

A terminal based scorekeeper for darts.

<img src="screen-01.png" width="425">
<img src="screen-02.png" width="425">

### Database Setup

```
createuser darts --superuser --password
# when prompted, the password is 'darts'

createdb -Udarts darts
psql -Udarts darts -f schema/01_init.sql
```

### Local Setup

Requires Racket v6.7+

```
raco pkg install charterm
raco pkg install rackunit-chk
```

### Running

```
# Interpreted:
racket main.rkt

# Compiled:
raco exe main.rkt
./main
```
