#lang racket
(require "lib/term-ui.rkt"
         "lib/db.rkt"
         "lib/colors.rkt"
         "lib/suggest.rkt"
         "lib/screen.rkt")

(provide screen-new-game)
(define (screen-new-game)
  (define all_players (db:list-players))
  (define players     empty)


  (define (create-player--actor name)
    (when (findf (lambda (p) (string-ci=? name (player-name p))) all_players)
      (error (format "A player already exists for: '~a'" name)))

    (define new_player (db:add-player name))
    (set! all_players (list* new_player all_players))
    (set! players (sort (list* new_player players) string<? #:key player-name))
    #t)


  (define (remove--suggestor name)
    (filter (lambda (n) (string-prefix? (string-downcase n) (string-downcase name)))
            (map player-name players)
            ))

  (define (remove--actor name)
    (define new_players (filter-not (lambda (p) (string-ci=? name (player-name p))) players))
    (when (equal? players new_players)
      (error (format "Unable to find player named '~a'" name)))

    (set! players new_players)
    #t
    )


  (define (player--suggestor name)
    (filter (lambda (n) (string-prefix? (string-downcase n) (string-downcase name)))
            (map player-name
                 (filter (lambda (p) (not (member p players))) all_players)
                 )))

  (define (player--actor name)
    (define new_player (findf (lambda (p) (string-ci=? name (player-name p))) all_players))
    (when (false? new_player)
      (error (format "Unable to find player: '~a'" name)))
    (when (member new_player players)
      (error (format "Player is already playing the game: '~a'" name)))

    (set! players (sort (list* new_player players) string<? #:key player-name))
    #t
    )


  (define (start--actor _)
    (when (empty? players)
      (error "Cannot start a game without players"))

    (db:start-game players)
    '(goto game-countdown)
    )


  (define commands
    (list
     (command "player" "name"  "Register a player for the next game"
              player--suggestor
              player--actor)

     (command "remove" "name"  "Removes a player from the next game"
              remove--suggestor
              remove--actor)

     (command "create-player" "name"  "Create a player for future games"
              'mirror-input
              create-player--actor)

     (command "start" #f  "Starts the game!"
              'mirror-input
              start--actor)
     )
    )

  (define (renderer)
    (renderln (color '(fg-green) (~a "┬─╮╭─╮┬─╮╭┬╮╭─╮" #:width (screen-cols) #:align 'center)))
    (renderln (color '(fg-green) (~a "│ │├─┤├┬╯ │ ╰─╮" #:width (screen-cols) #:align 'center)))
    (renderln (color '(fg-green) (~a "┴─╯┴ ┴┴╰─ ┴ ╰─╯" #:width (screen-cols) #:align 'center)))
    (renderln (color '(fg-green) (~a "" #:width (screen-cols) #:pad-string "─")))
    (renderln "Players " (color '(fg-cyan) (string-join (map player-name players) ", ")))
    (renderln "Game    " (color '(fg-yellow) "301"))
    )

  (screen commands renderer "")
  )
