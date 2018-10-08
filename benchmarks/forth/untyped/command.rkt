#lang racket/base

(provide
  command%
  CMD*
)

;; -----------------------------------------------------------------------------

(require
 racket/match
 racket/class
 (only-in racket/string string-join string-split)
 (for-syntax racket/base racket/syntax syntax/parse)
 racket/contract
 "../../../ctcs/precision-config.rkt"
 (only-in racket/function curry)
 (only-in racket/list empty? first second rest)
 (only-in "../../../ctcs/common.rkt" class/c* or-#f/c)
)
(require (only-in "stack.rkt"
  stack-drop
  stack-dup
  stack-init
  stack-over
  stack-pop
  stack-push
  stack-swap
  stack?
))

(define (assert v p)
  (unless (p v) (error 'assert))
  v)

;; =============================================================================
;; -- Commands

;; lltodo can these command% ctcs be any more precise?
;; Yes: by incorporating knowledge about the behavior of exec for each
;; command.

(define command%/c
  (class/c*
   (field/all
    [id symbol?]
    [descr string?]
    [exec ((listof
            (instanceof/c
             (recursive-contract command%/c)))
           stack?
           any/c
           . -> .
           (or-#f/c (cons/c (listof
                             (instanceof/c
                              (recursive-contract command%/c)))
                            stack?)))])))

(define command%? (instanceof/c command%/c))

(define/contract command%
  command%/c
  (class object%
    (super-new)
    (init-field
      id
      descr
      exec)))

(define env? (listof command%?))
(define ((env-with/c cmd-ids) env)
  (cond [(env? env)
         (define env-cmd-ids
           (for/list ([env-cmd (in-list env)])
             (get-field id env-cmd)))
         (for/and ([c (in-list cmd-ids)])
           (member c env-cmd-ids))]
        [else #f]))


(require (for-syntax syntax/parse))
(define-syntax (command%/c-with-exec stx)
  (syntax-parse stx
    #:datum-literals (args type result)
    [(_ (~optional (type command%-c-type))
        (args env-name stack-name val-name)
        [result result-ctc]
        (~optional (~seq (~datum #:post) post-condition)))
     #`(and/c (instanceof/c
               #,(if (attribute command%-c-type)
                     #'command%-c-type
                     #'command%/c))
              (instanceof/c
               (class/c*
                (field/all
                 [exec (->i ([env-name env?]
                             [stack-name stack?]
                             [val-name any/c])
                            [result (env-name stack-name val-name)
                                    result-ctc]
                            #,@(if (attribute post-condition)
                                   #'(#:post
                                      (env-name stack-name val-name)
                                      post-condition)
                                   #'()))]))))]))


;; True if the argument is a list with one element
(define/contract (singleton-list? x)
  (configurable-ctc
   [max (->i ([x list?])
             [result (x) (if (empty? x)
                             #f
                             (empty? (rest x)))])]
   [types (list? . -> . boolean?)])

  (and (list? x)
       (not (null? x))
       (null? (cdr x))))

;; Create a binary operation command.
;; Command is recognized by its identifier,
;;  the identifier is then applied to the top 2 numbers on the stack.
(define binop-command%/c
  (and/c command%/c
         ;; lltodo this may be wrong: not sure if field spec disallows
         ;; other fields from being present (like binop)
         (class/c* (field/all
                    [binop symbol?]
                    [exec any/c #|Same as command%/c|#]))))

(define-syntax (binop-command%/c-for stx)
  (syntax-parse stx
    [(_ binop)
     #'(command%/c-with-exec
        (type binop-command%/c)
        (args E S v)
        [result
         (or-#f/c
          (cons E
                (cons (binop (second S) (first S))
                      (rest (rest S)))))])]))

(define/contract binop-command%
  (configurable-ctc
   [max binop-command%/c]
   [types binop-command%/c])

  (class command%
    (init-field
     binop)
    (super-new
      (id (assert (object-name binop) symbol?))
      (exec (lambda (E S v)
        (if (singleton-list? v)
          (if (eq? (car v) (get-field id this))
             (let*-values ([(v1 S1) (stack-pop S)]
                           [(v2 S2) (stack-pop S1)])
               (cons E (stack-push S2 (binop v2 v1))))
             #f)
           #f))))))

;; Turns a symbol into a stack command parser
(define-syntax make-stack-command
  (syntax-parser
   [(_ opcode:id d:str)
    #:with stack-cmd (format-id #'opcode "stack-~a" (syntax-e #'opcode))
    #`(new command%
        (id '#,(syntax-e #'opcode))
        (descr d)
        (exec (lambda (E S v)
          (and (singleton-list? v)
               (eq? '#,(syntax-e #'opcode) (car v))
               (cons E (stack-cmd S))))))]))

(define (is-or-starts-with? predicate v)
  (or (and (symbol? v)
           (predicate v))
      (and (list? v)
           (not (empty? v))
           (predicate (first v)))))

;; Default environment of commands
(define/contract CMD*
  (configurable-ctc
   [max (and/c env?
               (list/c
                ;; exit
                (command%/c-with-exec
                 (args E S v)
                 [result (if (or (eof-object? v)
                                 (is-or-starts-with? exit? v))
                             'EXIT
                             #f)])
                ;; help
                (command%/c-with-exec
                 (args E S v)
                 [result (if (is-or-starts-with? help? v)
                             (cons E S)
                             #f)])

                (binop-command%/c-for +)
                (binop-command%/c-for -)
                (binop-command%/c-for *)

                ;; drop
                (command%/c-with-exec
                 (args E S v)
                 [result (if (is-or-starts-with? (curry equal? 'stack-drop)
                                                 v)
                             (cons E (rest S))
                             #f)])
                ;; dup
                (command%/c-with-exec
                 (args E S v)
                 [result (if (is-or-starts-with? (curry equal? 'stack-dup)
                                                 v)
                             (cons E (cons (first S) S))
                             #f)])
                ;; over
                (command%/c-with-exec
                 (args E S v)
                 [result (if (is-or-starts-with? (curry equal? 'stack-over)
                                                 v)
                             (cons E (cons (first S)
                                           (cons (second S)
                                                 (cons (first S)
                                                       (rest (rest S))))))
                             #f)])
                ;; swap
                (command%/c-with-exec
                 (args E S v)
                 [result (if (is-or-starts-with? (curry equal? 'stack-swap)
                                                 v)
                             (cons E
                                   (cons (second S)
                                         (cons (first S)
                                               (rest (rest S)))))
                             #f)])
                ;; push
                (command%/c-with-exec
                 (args E S v)
                 [result (if (or (and (list? v)
                                      (>= (length v) 1)
                                      (exact-integer? (first v)))
                                 (and (list? v)
                                      (>= (length v) 2)
                                      (equal? (first v) 'push)
                                      (exact-integer? (second v))))
                             (cons E
                                   (cons (if (exact-integer? (first v))
                                             (first v)
                                             (second v))
                                         S))
                             #f)])
                ;; show
                ;; lltemporal: prints
                (command%/c-with-exec
                 (args E S v)
                 [result (if (is-or-starts-with? (curry equal? 'show))
                             (cons E S)
                             #f)])))]
   [types env?])

  (list
   (new command%
        (id 'exit)
        (descr "End the REPL session")
        (exec (lambda (E S v)
                (if (or (eof-object? v)
                        (and (symbol? v)
                             (exit? v))
                        (and (list? v)
                             (not (null? v))
                             (exit? (car v))))
                    'EXIT
                    #f))))
   (new command%
        (id 'help)
        (descr "Print help information")
        (exec (lambda (E S v)
                (cond
                  [(and (symbol? v) (help? v))
                   (displayln (show-help E))
                   (cons E S)]
                  [(and (list? v) (not (null? v)) (help? (car v)))
                   (displayln (show-help E (and (not (null? (cdr v))) (cdr v))))
                   (cons E S)]
                  [else
                   #f]))))
   (instantiate binop-command% (+)
     (descr "Add the top two numbers on the stack"))
   (instantiate binop-command% (-)
     (descr "Subtract the top item of the stack from the second item."))
   (instantiate binop-command% (*)
     (descr "Multiply the top two item on the stack."))
   #;(instantiate binop-command% (/)
       (descr "Divide the top item of the stack by the second item."))
   (make-stack-command drop
                       "Drop the top item from the stack")
   (make-stack-command dup
                       "Duplicate the top item of the stack")
   (make-stack-command over
                       "Duplicate the top item of the stack, but place the duplicate in the third position of the stack.")
   (make-stack-command swap
                       "Swap the first two numbers on the stack")
   (new command%
        (id 'push)
        (descr "Push a number onto the stack")
        (exec (lambda (E S v)
                (match v
                  [`(push ,(? exact-integer? n))
                   (cons E (stack-push S n))]
                  [`(,(? exact-integer? n))
                   (cons E (stack-push S n))]
                  [_ #f]))))
   (new command%
        (id 'show)
        (descr "Print the current stack")
        (exec (lambda (E S v)
                (match v
                  [`(,(? show?))
                   (displayln S)
                   (cons E S)]
                  [_ #f]))))
   ))

(define/contract (exit? sym)
  (configurable-ctc
   [max (->i ([sym symbol?])
             [result (sym)
                     (memq sym '(exit quit q leave bye))])]
   [types (symbol? . -> . boolean?)])

  (and (memq sym '(exit quit q leave bye)) #t))

;; Search the environment for a command with `id` equal to `sym`
(define/contract (find-command E sym)
  (configurable-ctc
   [max (->i ([E env?]
              [sym symbol?])
             [result (E)
                     (and (not (empty? E))
                          (get-field id (first E)))])]
   [types (env? symbol? . -> . symbol?)])

  (for/or ([c (in-list E)])
    (get-field id c) (error 'no)))
    ;(if (eq? sym (get-field id c)) c #f)))

(define/contract (help? sym)
  (configurable-ctc
   [max (->i ([sym symbol?])
             [result (sym)
                     (memq sym '(help ? ??? -help --help h))])]
   [types (symbol? . -> . boolean?)])

  (and (memq sym '(help ? ??? -help --help h)) #t))

(define/contract (show? sym)
  (configurable-ctc
   [max (->i ([sym symbol?])
             [result (sym)
                     (memq sym '(show print pp ls stack))])]
   [types (symbol? . -> . boolean?)])

  (and (memq sym '(show print pp ls stack)) #t))

;; Print a help message.
;; If the optional argument is given, try to print information about it.
(define/contract (show-help E [v #f])
  (configurable-ctc
   [max (->i ([E env?]
              [v any/c])
             [result string?]
             #:post (E v result)
             (regexp-match?
              (match v
                [#f (and (= (length (string-split result "\n"))
                            (add1 (length E)))
                         "^Available commands:")]
                [(or (list (? symbol? s)) (? symbol? s))
                 (if (find-command E s)
                     (get-field descr (find-command E s))
                     (format "Unknown command '~a'" s))]
                [x (format "Cannot help with '~a'" x)])
              result))]
   [types (env? any/c . -> . string?)])

  (match v
    [#f
     (string-join
      (for/list ([c (in-list E)])
        (format "    ~a : ~a" (get-field id c) (get-field descr c)))
      "\n"
      #:before-first "Available commands:\n")]
    [(or (list (? symbol? s)) (? symbol? s))
     (define c (find-command E (assert s symbol?)))
     (if c
         (get-field descr c)
         (format "Unknown command '~a'" s))]
    [x
     (format "Cannot help with '~a'" x)]))

