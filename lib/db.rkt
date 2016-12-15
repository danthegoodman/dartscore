#lang racket
(require db)

(provide db:list-players
         db:add-player
         db:start-game
         db:has-active-game?
         db:oldest-active-game
         db:get-player-throw-sets
         db:save-throw-set
         db:finalize-game
         db:cancel-game
         player-id
         player-name
         game-id
         game-start-score
         game-players
         throw
         throw-point
         throw-multiplier
         throw-busted?
         throw-score
         )

(struct player (id name) #:transparent)
(struct game (id start-score players) #:transparent)
(struct throw (point multiplier busted?) #:transparent)
 
(define conn (postgresql-connect #:database "darts" #:user "darts" #:password "darts"))

(define (throw-score t)
  (if (throw-busted? t) 0 (* (throw-point t) (throw-multiplier t))))

(define (db:list-players) ; -> (list <player> ...)
  (map vector->player (query-rows conn "SELECT player_id, name FROM player")))

(define (db:add-player name) ; -> <player>
  (transaction-block
   (define new_id (query-value conn "INSERT INTO player(name) VALUES ($1) RETURNING player_id" name))
   (player new_id name)
   ))

(define (db:start-game players) ; -> void
  (transaction-block
   (define start_score 301)
   (define game_id (query-value conn "INSERT INTO countdown_game(start_score) VALUES ($1) RETURNING game_id" start_score))

   (for-each
    (lambda (pid ndx)
      (query-exec conn "INSERT INTO countdown_game_player(game_id, player_id, player_ndx) VALUES ($1,$2,$3)" game_id pid ndx)
      )
    (shuffle (map player-id players))
    (range (length players))
    )
   (void)
   ))

(define (db:has-active-game?) ; -> boolean
  (query-value conn "SELECT count(*) > 0 FROM countdown_game WHERE finalized = false"))

(define (db:oldest-active-game) ; -> <game>
  (match-define
    (vector game_id start_score)
    (query-row conn "SELECT game_id, start_score FROM countdown_game \
                     WHERE finalized = false \
                     ORDER BY start_time ASC LIMIT 1"))
  (define players
    (query-rows conn "SELECT p.player_id, p.name \
                      FROM countdown_game_player gp NATURAL JOIN player p \
                      WHERE gp.game_id = $1 \
                      ORDER BY gp.player_ndx" game_id))

  (game game_id start_score (map vector->player players))
  )

(define (db:get-player-throw-sets plyr_id gm_id)
  (define all_throws (map transform-player-throw (query-rows conn "\
      SELECT s.set_ndx, t.points, t.multiplier, t.busted \
      FROM countdown_game_player_throw t \
        NATURAL JOIN countdown_game_player_throw_set s \
      WHERE s.player_id = $1 AND s.game_id = $2\
      ORDER BY s.set_ndx, t.throw_ndx" plyr_id gm_id)))
  (map (lambda (l) (map second l)) (group-by first all_throws))
  )

(define (db:save-throw-set gm_id plyr_id throws)
  (define set_id (query-value conn "\
      INSERT INTO countdown_game_player_throw_set(game_id,player_id,set_ndx) VALUES \
        ($1,$2,( \
          SELECT count(*) \
          FROM countdown_game_player_throw_set \
          WHERE game_id = $1 AND player_id = $2 \
        ))
      RETURNING set_id" gm_id plyr_id))

  (for-each
   (lambda (t ndx) (query-exec conn "\
       INSERT INTO countdown_game_player_throw \
       (set_id,throw_ndx,points,multiplier,busted) \
       VALUES ($1,$2,$3,$4,$5)" set_id ndx (throw-point t) (throw-multiplier t) (throw-busted? t))
      )
    throws
    (range (length throws))
    )
  (query-exec conn "UPDATE countdown_game SET end_time = current_timestamp WHERE game_id = $1" gm_id)
  )

(define (db:finalize-game gm_id)
  (query-exec conn "UPDATE countdown_game SET finalized = true WHERE game_id = $1" gm_id)
  )

(define (db:cancel-game gm_id)
  (query-exec conn "UPDATE countdown_game SET cancelled = true, finalized = true WHERE game_id = $1" gm_id)
  )

(define (transform-player-throw v)
  (match v
    [(vector a b c d) (list a (throw b c d))])
  )

(define (vector->player v)
  (player (vector-ref v 0) (vector-ref v 1)))

(define-syntax-rule (transaction-block e ...)
  (call-with-transaction conn (thunk e ...))
  )