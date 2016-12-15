#lang racket
(require "lib/term-ui.rkt"
         "lib/db.rkt"
         "lib/colors.rkt"
         "lib/suggest.rkt"
         "lib/screen.rkt"
         srfi/1)

(provide screen-game-countdown)
(define (screen-game-countdown)
  (define game (db:oldest-active-game))
  (define player-states (map (curry compute-player-state game) (game-players game)))
  (define active_player_id (determine-first-player player-states))
  (define throws empty)


  (define (throw--suggestor input)
    (with-handlers ([(const #t) (const empty)])
      (set! throws (parse-throws input))
      (list (string-join (map throw->string throws)))
      ))
  
  (define (throw--actor input)
    (when (false? active_player_id)
      (error "No more throws can be recorded."))
    
    (define player_ndx (list-index (lambda (ps) (equal? active_player_id (player-state-id ps))) player-states))
    (define score (player-state-score (list-ref player-states player_ndx)))
    (define db_throws
      (filter-map (lambda (t)
                    (if (<= score 0)
                        #f
                        (begin
                          (set! score (- score (apply * t)))
                          (throw (first t) (second t) (< score 0))
                          )))
                  throws
                  ))
    (when (and (< (length db_throws) 3) (< 0 score))
      (error "More throws need to be recorded")
      )

    (db:save-throw-set (game-id game) active_player_id db_throws)
    (set! player-states (list-set player-states player_ndx (compute-player-state game (list-ref (game-players game) player_ndx))))
    (set! active_player_id (determine-next-player player-states player_ndx))
    (set! throws empty)

    (if (false? active_player_id)
        (begin
          (db:finalize-game (game-id game))
          '(goto new-game)
          )
        #t
        )
    )

  (define (skip-to--suggestor name)
    (filter (lambda (n) (string-prefix? (string-downcase n) (string-downcase name)))
            (map player-name (game-players game))
            ))
  
  (define (skip-to--actor name)
    (when (false? active_player_id)
      (error "Game is already finished."))
    (define p (findf (lambda (p) (string-ci=? name (player-name p))) (game-players game)))
    (define ps (findf (lambda (ps) (= (player-id p) (player-state-id ps))) player-states))

    (when (= 0 (player-state-score ps))
      (error (format "Player is already finished: '~a'" name)))

    (set! active_player_id (player-id p))
    #t
    )


  (define (finalize--actor _)
    (unless (false? active_player_id)
      (error "Game must be completed to be finalized."))
    (db:finalize-game (game-id game))
    '(goto new-game))
  

  (define (cancel-game--actor input)
    (unless (equal? input "YES")
      (error "Enter 'YES' to confirm cancellation"))
    (db:cancel-game (game-id game))
    '(goto new-game))
  
  
  (define commands
    (list
     (command "cancel-game" "confirm"   "Scrap the game and all of its throws"
              'mirror-input
              cancel-game--actor)
;     (command "finalize" #f   "Mark the game as completed"
;              'mirror-input
;              finalize--actor)
     (command "skip-to" "name"   "Skip regular ordering"
              skip-to--suggestor
              skip-to--actor)
     (command "throw" "score> <score> <score"  "Record throws and advance game"
              throw--suggestor
              throw--actor)
     ;TODO edit "name> <turn"  "Edit a previous turn's score"
     )
    )

  (define (renderer)
    (renderln)
    (render-players)
    (renderln)
    (when (false? active_player_id)
      (renderln (~a "The game is finished" #:width (screen-cols) #:align 'center))
      )
    (unless (false? active_player_id)
      (render-throws))
    )

  (define (render-players)
    (define players (game-players game))

    (define raw_names (map player-name players))
    (define max_name_len (apply max (map string-length raw_names)))

    (define names (map (lambda (n) (~a n #:width max_name_len)) raw_names))
    (define ids (map player-id players))
    (define scores (map player-state-score player-states))
    (define turns (map player-state-turn player-states))
    (for-each render-player-status names ids scores turns)
    )

  (define (render-player-status name id score turn)
    (define is_active? (equal? active_player_id id))
    (define clr (if is_active? '(fg-cyan) '()))
    (define line
      (string-append
       (if is_active? "->" "  ")
       name
       "    " (~a score #:width 4)
       "    Turn " (~a turn #:width 3)
       )
      )
    (renderln (color clr (~a line #:width (screen-cols) #:align 'center)))
    )

  (define (render-throws)
    (define score (player-state-score (findf (lambda (ps) (equal? active_player_id (player-state-id ps))) player-states)))
    (define (fill-empty lst)
      (append lst (make-list (- 3 (length lst)) "    ")))
    (define throw-strs (map render-throw-str throws))
    (define score-strs (render-throw-scores score throws))

    (define left_pad (~a "" #:width (floor (/ (- (screen-cols) 28) 2))))
    (define (rcf fmtstr args) (renderln left_pad (apply format fmtstr args)))
    (rcf "      ╭─~a─┬─~a─┬─~a─╮" '(──── ──── ────))
    (rcf "Throw │ ~a │ ~a │ ~a │" (fill-empty throw-strs))
    (rcf "      ├─~a─┼─~a─┼─~a─┤" '(──── ──── ────))
    (rcf "Score │ ~a │ ~a │ ~a │" (fill-empty score-strs))
    (rcf "      ╰─~a─┴─~a─┴─~a─╯" '(──── ──── ────))
    )
  
  (screen commands renderer "throw ")
  )

(struct player-state (id score turn) #:transparent)

(define (compute-player-state gm plyr)
  (define sets (db:get-player-throw-sets (player-id plyr) (game-id gm)))
  (define scoring_sets (filter (curry andmap (compose not throw-busted?)) sets))
  (define all_points (apply + (map throw-score (flatten scoring_sets))))
  (player-state
   (player-id plyr)
   (- (game-start-score gm) all_points)
   (add1 (length sets)))
  )

(define (parse-throws input)
  (define parts (regexp-match* #px"[^ ]+" input))
  (when (< 3 (length parts))
    (error "Only 3 throws may be recorded at time"))
  (map parse-throw parts)
  )

(define (parse-throw input)
  (define-values (pts mult)
    (match (regexp-match #px"^([0-9]+)[^0-9]?([0-9]*)$" input)
      [(list _ s "") (values (string->number s) 1)]
      [(list _ s m) (values (string->number s) (string->number m))]
      [_ (error (format "Unable to interpret throw '~a'" input))]
      ))
  (unless (is-valid-points? pts)
    (error (format "Invalid score for throw '~a'" pts)))
  (unless (is-valid-multiplier? mult)
    (error (format "Invalid multiplier for throw '~a'" mult)))
  (when (and (= 25 pts) (= 3 mult))
    (error (format "Invalid multiplier for bullseye '~a'" mult)))

  (if (or (= 0 pts) (= 0 mult))
      (list 0 0)
      (list pts mult))
  )

(define (is-valid-points? s)
  (or (= 25 s)
      (and (<= 0 s) (<= s 20))
      ))
(define (is-valid-multiplier? m)
  (and (<= 0 m) (<= m 3)))

(define (throw->string throw)
  (match throw
    [(list 0 0) "0"]
    [(list s 1) (format "~a" s)]
    [(list s m) (format "~ax~a" s m)]
    ))

(define (render-throw-str throw)
  (match throw
    [(list 0 0)  (color '(fg-dark-gray) "miss")]
    [(list 25 1) (color '(fg-yellow) "  25")]
    [(list 25 2) (color '(fg-green) "25x2")]
    [(list s 1)  (~a s #:width 4 #:align 'right)]
    [(list s 2)  (color '(fg-yellow) (string-append (~a s #:width 2 #:align 'right) "x2"))]
    [(list s 3)  (color '(fg-green)  (string-append (~a s #:width 2 #:align 'right) "x3"))]
    ))

(define (render-throw-scores score throws)
  (map
   (lambda (t)
     (define new_score (if (<= score 0) score (- score (apply * t))))
     (set! score new_score)
     (if (< score 0)
         (color '(fg-red) "bust")
         (if (= score 0)
             (color '(fg-green) "game")
             (~a score #:width 4)
             )))
   throws
   ))

(define (determine-first-player states)
  (define valid_states (filter-not (lambda (s) (= 0 (player-state-score s))) states))
  (define sorted_states (sort states < #:key player-state-turn))
  (if (empty? valid_states)
      #f
      (player-state-id (first sorted_states))
      ))

(define (determine-next-player states curr_ndx)
  (define next_ndx (modulo (add1 curr_ndx) (length states)))
  (define cycle_states (append (drop states next_ndx) (take states (add1 next_ndx))))
  (define valid_states (filter-not (lambda (s) (= 0 (player-state-score s))) cycle_states))
  (if (empty? valid_states)
      #f
      (player-state-id (first valid_states))
      ))

(module+ test
  (require rackunit/chk)
  (chk
   (parse-throw "0")   '(0 0)
   (parse-throw "0x3") '(0 0)
   (parse-throw "3x0") '(0 0)
   (parse-throw "1")   '(1 1)
   (parse-throw "1x1") '(1 1)
   (parse-throw "1*2") '(1 2)
   (parse-throw "1_3") '(1 3)
   #:exn (parse-throw "1x4") "Invalid multiplier"
   (parse-throw "15")   '(15 1)
   (parse-throw "15#1") '(15 1)
   (parse-throw "15a2") '(15 2)
   (parse-throw "15.3") '(15 3)
   (parse-throw "20")   '(20 1)
   (parse-throw "20!1") '(20 1)
   (parse-throw "20^2") '(20 2)
   (parse-throw "20p3") '(20 3)
   #:exn (parse-throw "21") "Invalid score"
   #:exn (parse-throw "24") "Invalid score"
   (parse-throw "25") '(25 1)
   (parse-throw "25x1") '(25 1)
   (parse-throw "25x2") '(25 2)
   #:exn (parse-throw "25x3") "Invalid multiplier"
   #:exn (parse-throw "26") "Invalid score"
   #:exn (parse-throw "50") "Invalid score"
   #:exn (parse-throw "x1") "Unable"
   #:exn (parse-throw "x") "Unable"
   #:exn (parse-throw "") "Unable"
   #:exn (parse-throw "2xx3") "Unable"
   )
  )  