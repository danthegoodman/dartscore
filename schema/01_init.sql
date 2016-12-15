CREATE TABLE player (
  player_id SERIAL PRIMARY KEY,
  name      TEXT   NOT NULL
);

CREATE TABLE countdown_game (
  game_id     SERIAL    PRIMARY KEY,
  start_score INTEGER   NOT NULL,
  start_time  TIMESTAMP NOT NULL DEFAULT current_timestamp,
  end_time    TIMESTAMP,
  finalized   BOOLEAN   NOT NULL DEFAULT false,
  cancelled    BOOLEAN   NOT NULL DEFAULT false
);

CREATE TABLE countdown_game_player (
  game_id    INTEGER NOT NULL REFERENCES countdown_game,
  player_id  INTEGER NOT NULL REFERENCES player,
  player_ndx INTEGER NOT NULL
);

CREATE TABLE countdown_game_player_throw_set (
  set_id    SERIAL PRIMARY KEY,
  set_ndx   INTEGER  NOT NULL,
  game_id   INTEGER  NOT NULL REFERENCES countdown_game,
  player_id INTEGER  NOT NULL REFERENCES player
);

CREATE TABLE countdown_game_player_throw (
  set_id     INTEGER NOT NULL REFERENCES countdown_game_player_throw_set,
  throw_ndx  INTEGER NOT NULL,
  points     INTEGER NOT NULL,
  multiplier INTEGER NOT NULL,
  busted     BOOLEAN NOT NULL
);
