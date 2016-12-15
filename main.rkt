#lang racket
(require charterm
         racket/exn
         "lib/db.rkt"
         "lib/term-ui.rkt"
         "lib/suggest.rkt"
         "lib/colors.rkt"
         "lib/screen.rkt"
         "screen-new-game.rkt"
         "screen-game-countdown.rkt")

(define (repl scrn buff [err #f])
  (define suggs (list-suggestions (screen-commands scrn) buff))

  (calculate-screen-size)
  (charterm-clear-screen)
  (charterm-cursor 1 1)
  ((screen-renderer scrn))
  (render-suggestions suggs buff err)

  (handle-key scrn buff suggs (charterm-read-key))
  )

(define (render-suggestions suggs buff err)
  (charterm-cursor 1 (- (screen-rows)
                        (length suggs)
                        (if (empty? suggs) 1 0)
                        (if (exn? err) 1 0)
                        ))
  (when (exn? err)
    (renderln (color '(fg-red) (exn-message err)))
    )
  (for-each render-suggestion suggs)
  (when (empty? suggs)
    (renderln (color '(fg-red) "<no command found>"))
    )
  (render "> " buff)
  )

(define (render-suggestion s)
  (define name (suggestion-name s))
  (define arg (match s
                [(app suggestion-arg-name #f) (color '() "")]
                [(app suggestion-arg (or #f "")) (color '(fg-yellow) (string-append "<" (suggestion-arg-name s) ">"))]
                [(app suggestion-arg a) (color '(fg-cyan) a)]
                ))
  (define left (string-append name " " arg))
  (define left_width (- (string-length left) COLOR_LEN))
  
  (define right (~a (suggestion-description s) #:width (- (screen-cols) left_width) #:align 'right))
  (renderln left (color '(fg-dark-gray) right))
  )


(define (handle-key scrn buff suggs key)
  (match key
    ['ctrl-c (match buff
               ["" (void)]         ; terminate
               [_  (repl scrn "")] ; clear buffer
               )]
    ['return (match suggs
               [(list sg) (handle-suggestion-action sg scrn buff)] ; handle single selection
               [_         (repl scrn buff)]                        ; nop
               )]
    ['tab (match suggs
            [(list x) (repl scrn (suggestion-completion x))] ; complete single suggestion
            [_        (repl scrn buff)]                      ; nop
            )]
    [_ (repl scrn (update-buffer buff key))] ; just update buffer 
    )
  )

(define (handle-suggestion-action sugg scrn buff)
  (match (with-handlers ([(const #t) identity]) ((suggestion-actor sugg)))
    [#f (repl scrn buff)]
    [#t (repl scrn (screen-default-buffer scrn))]
    [`(goto ,scrn_id) (let ([new_screen (create-screen scrn_id)]) (repl new_screen (screen-default-buffer new_screen)))]
    [(? exn? err) (repl scrn buff err)]
    )
  )

(define (create-screen scrn_id)
  (match scrn_id
    ['new-game (screen-new-game)]
    ['game-countdown (screen-game-countdown)]
    ))

(define (suggestion-completion s)
  (match (suggestion-arg s)
    [#f (string-append (suggestion-name s) " ")]
    [x  (string-append (suggestion-name s) " " x)]
    ))

(define (update-buffer buff key)
  (match key
    ['backspace (match buff
                  ["" ""]
                  [_ (substring buff 0 (sub1 (string-length buff)))]
                  )]
    ['ctrl-w (string-remove-word buff)]
    [(? symbol?) buff]
    
    [#\  (match buff
           ["" ""]
           [(pregexp #px" $") buff]
           [_ (string-append buff " ")]
           )]
    [#\- (string-append buff "-")]
    [#\. (string-append buff ".")]
    [#\* (string-append buff "*")]
    [(? char-alphabetic?) (string-append buff (~a key))]
    [(? char-numeric?) (string-append buff (~a key))]
    [_ buff]
    )
  )

(define (string-remove-word str)
  (define words (string-split str))
  (string-join (take words (max 0 (sub1 (length words)))))
  )
; used as an error handling predicate, we ensure errors
; are rethrown for default logging.
(define (cleanup . _)
  (charterm-display "\u1b[?1049l") ; exit alt-screen
  (close-charterm)
  #f
  )

(void (open-charterm #:current? #t))
(charterm-display "\u1b[?1049h") ; enter alt-screen
(with-handlers ([cleanup #f])
  (define screen (if (db:has-active-game?)
                     (create-screen 'game-countdown)
                     (create-screen 'new-game)
                     ))
  (repl screen (screen-default-buffer screen))
  (void (cleanup))
  )
