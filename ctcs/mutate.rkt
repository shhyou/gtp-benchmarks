#lang racket


(require (for-syntax syntax/parse
                     racket/match))

(define-for-syntax make-mutants #t)

#;(define-syntax (define-mutators stx)
  (syntax-parse stx
    #:datum-literals (-> <->)
    [(_ mutator-name
        (~or (orig -> mutated)
             (left <-> right)) ...
        other-mutations ...)
     #'(define-syntax (mutator-name s)
         (if make-mutants
             (syntax-parse s
               #:literals (orig ... left ... right ...)
               [(_ (orig arg (... ...)))
                #'(mutated arg (... ...))] ...
               (~@ [(_ (left arg (... ...)))
                    #'(right arg (... ...))] ...
                   [(_ (right arg (... ...)))
                    #'(left arg (... ...))] ...)
               other-mutations ...
               [(_ other)
                s])
             (syntax-parse s
               [(_ nomutate)
                s])))]))

(require (for-syntax racket/match))
(define-syntax (define-mutators stx)
  (syntax-parse stx
    #:datum-literals (-> <->)
    [(_ mutator-name
        (~or (orig -> mutated)
             (left <-> right)) ...
        other-mutations ...)
     #'(define-for-syntax (mutator-name s)
         (displayln `(mutating ,s))
         (if make-mutants
             ;; Add invocation syntax around s so that patterns can be
             ;; written like a normal macro
             (syntax-parse #`(mutator-name #,s)
               #:literals (orig ... left ... right ...)
               [(_ (orig arg (... ...)))
                #'(mutated arg (... ...))] ...
               (~@ [(_ (left arg (... ...)))
                    #'(right arg (... ...))] ...
                   [(_ (right arg (... ...)))
                    #'(left arg (... ...))] ...)
               other-mutations ...
               [(_ other)
                (displayln 'found-no-mutations)
                s])
             s))]))

(define =/= (curry not =))

(define-mutators mutate
  ;; operator mutation
  (< <-> <=)
  (> <-> >=)
  (= <-> =/=)
  (+ <-> -)
  (* <-> /)
  (and <-> or)
  (modulo -> *)

  ;; statement deletion
  [(_ ((~datum begin) e1 e2 ...+))
   (displayln `(got begin))
   #'(begin e2 ...)]

  ;; Conditional negation
  [(_ ((~datum if) test-e then-e else-e))
   (displayln `(got if))
   #'(if (not test-e) then-e else-e)]
  ;; ll: Using above instead because seems more interesting
  ;; ;; Conditional removal
  ;; [(_ (if test-e then-e else-e))
  ;;  #'then-e]

  ;; ;; lltodo: came up with this custom one?
  ;; [(_ (unary-fn arg))
  ;;  #'(unary-fn (unary-fn arg))]

  ;; Classes
  ;; access modifier change
  [(_ ((~datum define/public) id+other ...))
   (displayln `(got pub))
   #'(define/private id+other ...)]
  [(_ ((~datum define/override) id+other ...))
   (displayln `(got priv))
   #'(define/augment id+other ...)]
  #;[(_ ((~datum super-new)))
   #'(void)]

  ;; constant mutation
  [(_  (~and value
             (~not (fn arg ...))))
   (displayln `(got value ,#'value))
   (let ([v (syntax->datum #'value)])
     (match v
       [(? boolean?)
        #'(not value)]
       [1 #'0]
       [-1 #'1]
       [5 #'-1]
       [(? integer?) #'(add1 value)]
       [(? number?) #'(- -1 value)]
       [_ #'value]))])

(define-syntax (mutate/expr stx)
  (syntax-parse stx
    ;; modify class methods
    [(_ (class
          e ...
          (~or ((~datum define/public) id+other/pub ...)
               ((~datum define/override) id+other/over ...))
          more-e ...))
     #`(class e ...
         #,(if (attribute id+other/pub)
               (mutate #'(define/public id+other/pub ...))
               (mutate #'(define/override id+other/over ...)))
         more-e ...)]
    ;; leave other classes alone
    [(_ (class e ...))
     #'(class e ...)]

    ;; modify function apps
    [(_ (fn arg ...))
     (displayln `(got app of ,#'fn on ,#'(arg ...)))
     ;; A function application may not have an applicable mutation
     ;; First, see if it does..
     (let ([mutated (mutate #'(fn arg ...))])
       (if (equal? (syntax->datum #'(fn arg ...))
                   (syntax->datum mutated))
           ;; if not, push mutation down to arguments
           #'(fn (mutate/expr arg) ...)
           ;; if it does, stop there
           mutated))]

    ;; anything else, just try mutate
    [(_ e)
     (displayln `(trying mutate on ,#'e))
     (mutate #'e)]))

(define-for-syntax seed 0)
(begin-for-syntax
  (unless (>= seed 0)
    (error "seed must be positive")))
(define-syntax (mutate-program stx)
  (syntax-parse stx
    ;; Mutate definition body (could be class, function, other value)
    [(_ counter
        ((define/contract id/sig ctc body ...) e ...))
     (let* ([counter-val (syntax->datum #'counter)]
            [mutate? (= counter-val seed)])
       #`(begin (define/contract id/sig ctc
                  #,@(if mutate?
                         #'((mutate/expr body) ...)
                         #'(body ...)))
                #,@(if mutate?
                       #'(e ...)
                       #`((mutate-program #,(add1 counter-val)
                                          (e ...))))))]

    ;; Ignore anything else
    [(_ counter
        (other-e e ...))
     #'(begin other-e
              (mutate-program counter
                              (e ...)))]
    [(_ counter
        ())
     (begin
       (displayln "Warning: reached maximum mutatable expressions in module")
       #'(void))]

    ;; Initial invocation: add counter
    [(_ e ...)
     #'(mutate-program 0 (e ...))]))


;; lltodo: want to test as follows
;; but it's not working. Need to find a way to debug.
;;
;; (define pub (class object% (define/public (hi) (+ 2 2)) (super-new)))
;; (send (new pub) hi) ; => 4
;; (define priv (mutate/expr (class object% (define/public (hi) (+ 2 2)) (super-new))))
;; (send (new priv) hi) ; => 0

(module+ test
  (mutate-program
   (define/contract a integer? 5)
   (define/contract b integer? 6)
   (displayln a)
   (displayln b)))
