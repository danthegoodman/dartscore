#lang racket/gui
(module+ test (require rackunit))

(provide command
         list-suggestions
         suggestion-name
         suggestion-arg
         suggestion-arg-name
         suggestion-description
         suggestion-actor
         )

(struct command (name arg-name description suggestor actor) #:transparent)
(struct suggestion (command arg actionable?) #:transparent)

(define (suggestion-description sugg)
  (command-description (suggestion-command sugg)))

(define (suggestion-name sugg)
  (command-name (suggestion-command sugg)))

(define (suggestion-arg-name sugg)
  (command-arg-name (suggestion-command sugg)))

(define (suggestion-actor sugg)
  (match sugg
    [(? suggestion-actionable?) (lambda () ((command-actor (suggestion-command sugg)) (suggestion-arg sugg)))]
    [_ (lambda () #f)]
    ))

(define (list-suggestions commands input)
  (define-values (cmd-word arg-word) (split-input input))
  (define cmds (filter (lambda (c) (string-prefix? (command-name c) cmd-word)) commands))
  (define c (if (empty? cmds) #f (first cmds)))

  (if (and c (string=? cmd-word (command-name c)))
      (list-sub-suggestions c arg-word)
      (map build-command-suggestion cmds)
      )
  )

; ------------------------------------

(define (command-effective-suggestor cmd)
  (match (command-suggestor cmd)
    ['mirror-input (if (false? (command-arg-name cmd))
                       (lambda (in) (list #f))
                       (lambda (in) (list in))
                       )]
    [s s]))

(define (split-input input)
  (match (regexp-match-positions #px"\\s+" input)
    [(list (cons a b)) (values (substring input 0 a) (substring input b))]
    [#f (values input "")]
    ))

(define (build-command-suggestion cmd)
  (suggestion cmd #f #f))

(define (list-sub-suggestions cmd inarg)
  (define (build-sugg text) (suggestion cmd text #t))
  (define suggestor (command-effective-suggestor cmd))
  (map build-sugg (suggestor inarg))
  )

; ------------------------------------

(module+ test
  (define test-commands
    (list
     (command "alpha" #f "Does alpha things"
              'mirror-input
              (lambda (_) null))

     (command "beta" "name" "Does beta thing"
              (lambda (name) (filter (curryr string-prefix? name) '("Alice" "Bob" "Craig")))
              (lambda (name) null))

     (command "bolo" "id" "Does bolo things"
              'mirror-input
              (lambda (id) null))
     )
    )
  (define ALPHA (list-ref test-commands 0))
  (define BETA  (list-ref test-commands 1))
  (define BOLO  (list-ref test-commands 2))

  (define (test-suggestions input expected-suggestions)
    (define suggs (list-suggestions test-commands input))
    ;(define suggs (map suggestion-text (list-suggestions test-commands input)))
    (check-equal? expected-suggestions suggs (format "for input: '~a'" input)))

  (test-suggestions "" (list (suggestion ALPHA #f #f)
                             (suggestion BETA #f #f)
                             (suggestion BOLO #f #f)))
  (test-suggestions "trash"  empty)
  (test-suggestions "a" (list (suggestion ALPHA #f #f)))
  (test-suggestions "b" (list (suggestion BETA #f #f)
                              (suggestion BOLO #f #f)))
  (test-suggestions "bet" (list (suggestion BETA #f #f)))
  (test-suggestions "bo" (list (suggestion BOLO #f #f)))
  
  (test-suggestions "alpha"       (list (suggestion ALPHA #f #t)))
  (test-suggestions "alpha stuff" (list (suggestion ALPHA #f #t)))
  (test-suggestions "beta"   (list (suggestion BETA "Alice" #t)
                                   (suggestion BETA "Bob" #t)
                                   (suggestion BETA "Craig" #t)))
  (test-suggestions "beta A" (list (suggestion BETA "Alice" #t)))
  (test-suggestions "beta Z" empty)
  (test-suggestions "bolo"     (list (suggestion BOLO "" #t)))
  (test-suggestions "bolo 200" (list (suggestion BOLO "200" #t)))
  )

