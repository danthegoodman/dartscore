# Dartscore

A terminal based scorekeeper for darts.

<img src="screen-01.png" width="425">
<img src="screen-02.png" width="425">

### Database Setup

```
createuser darts --superuser
createdb -Udarts darts
psql -Udarts darts -f schema/01_init.sql
```

### Local Setup

```
raco pkg charterm
raco pkg rackunit/chk
```

### Compiling

```
rack exe main.rkt
```
