#lang racket

(provide mutate-program)

(require (for-syntax syntax/parse)
         syntax/parse
         racket/match)

(define make-mutants #t)
(define mutation-index 0)

(unless (>= mutation-index 0)
  (error "mutation-index must be positive"))

(define-syntax (define-mutators stx)
  (syntax-parse stx
    #:datum-literals (-> <->)
    [(_ mutator-name
        (~or (orig -> mutated)
             (left <-> right)) ...
        other-mutations ...)
     #'(define (mutator-name s)
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
   #'(begin e2 ...)]

  ;; Conditional negation
  [(_ ((~datum if) test-e then-e else-e))
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
   #'(define/private id+other ...)]
  [(_ ((~datum define/override) id+other ...))
   #'(define/augment id+other ...)]
  #;[(_ ((~datum super-new)))
   #'(void)]

  ;; constant mutation
  [(_  (~and value
             (~not (fn arg ...))))
   (let ([v (syntax->datum #'value)])
     (match v
       [(? boolean?)
        #'(not value)]
       [1 #'0]
       [-1 #'1]
       [5 #'-1]
       [(? integer?) #'(add1 value)]
       [(? number?) #'(- -1 value)]
       [_
        #'value]))])

(define (mutate-body stx)
  (if make-mutants
      (syntax-parse stx
        ;; modify class methods
        [((~datum class)
          e ...
          (~or ((~datum define/public) id+other/pub ...)
               ((~datum define/override) id+other/over ...))
          more-e ...)
         #`(class e ...
             #,(if (attribute id+other/pub)
                   (mutate #'(define/public id+other/pub ...))
                   (mutate #'(define/override id+other/over ...)))
             more-e ...)]
        ;; leave other classes alone
        [((~datum class) e ...)
         #'(class e ...)]

        ;; modify function apps
        [((~and fn:id (~not (~or (~datum λ)
                                 (~datum lambda)))) arg ...)
         ;; A function application may not have an applicable mutation
         ;; First, see if it does..
         (let ([mutated (mutate #'(fn arg ...))])
           (if (equal? (syntax->datum #'(fn arg ...))
                       (syntax->datum mutated))
               ;; if not, push mutation down to arguments
               #'(fn (mutate-body arg) ...)
               ;; if it does, stop there
               mutated))]

        ;; anything else, just try mutate
        [e
         (mutate #'e)])

      stx))


;; lltodo: decisions about when to mutate should be on an
;; expression-by-expression basis, not definitions
;; Just need to push the mutate? logic down lower, and
;; have mutate-body and mutate-expr return the new counter
;; somehow
(define (mutate-program stx mutation-index [counter 0])
  (if make-mutants
      (syntax-parse stx
        ;; Mutate definition body (could be class, function, other value)
        [((define/contract id/sig ctc body ...) e ...)
         (let* ([mutate? (= counter mutation-index)])
           (if mutate?
               #`((define/contract id/sig ctc
                    #,@(map mutate-body (syntax-e #'(body ...))))
                  e ...)
               #`((define/contract id/sig ctc
                    body ...)
                  #,@(mutate-program #'(e ...)
                                     mutation-index
                                     (add1 counter)))))]

        ;; Ignore anything else
        [(other-e e ...)
         #`(other-e
            #,@(mutate-program #'(e ...)
                               mutation-index
                               counter))]
        [()
         ;; signal no more mutations in this module
         (error 'mutate-program
                "Mutation index exceeds mutatable forms in module")])

      stx))

(module+ test
  (require rackunit)

  (define (programs-equal? a b)
    (equal? (syntax->datum a)
            (syntax->datum b)))

  (define-binary-check (check-programs-equal? actual expected)
    (programs-equal? actual expected))

  #;(define-simple-check (check-mutation index orig-prog new-prog)
    (programs-equal? (mutate-program orig-prog 0)
                     new-prog))
  (define-syntax-rule (check-mutation index orig-prog new-prog)
    (check-programs-equal? (mutate-program orig-prog index)
                           new-prog))

  ;; constants
  (check-mutation
   0
   #'{(define/contract a any/c #t)}
   #'{(define/contract a any/c (not #t))})
  (check-mutation
   0
   #'{(define/contract a any/c #f)}
   #'{(define/contract a any/c (not #f))})

  (check-mutation
   0
   #'{(define/contract a positive? 1)}
   #'{(define/contract a positive? 0)})

  (check-mutation
   0
   #'{(define/contract a positive? -1)}
   #'{(define/contract a positive? 1)})
  (check-mutation
   0
   #'{(define/contract a positive? 5)}
   #'{(define/contract a positive? -1)})
  (check-mutation
   0
   #'{(define/contract a positive? 3)}
   #'{(define/contract a positive? (add1 3))})
  (check-mutation
   0
   #'{(define/contract a positive? 3.5)}
   #'{(define/contract a positive? (- -1 3.5))})
  (check-mutation
   0
   #'{(define/contract a positive? (λ (x) x))}
   #'{(define/contract a positive? (λ (x) x))})

  ;; Check adding more exprs and moving index works
  (check-mutation
   0
   #'{(define/contract a positive? 1)
      (define/contract b positive? 2)}
   #'{(define/contract a positive? 0)
      (define/contract b positive? 2)})
  (check-mutation
   1
   #'{(define/contract a positive? 1)
      (define/contract b positive? 2)}
   #'{(define/contract a positive? 1)
      (define/contract b positive? (add1 2))})
  (check-mutation
   0
   #'{(define/contract (f x)
        (-> positive? positive?)
        1)
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        (-> positive? positive?)
        0)
      (define/contract b positive? 2)})

  ;; operators
  (check-mutation
   0
   #'{(define/contract (f x)
        any/c
        (< x 2))
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        any/c
        (<= x 2))
      (define/contract b positive? 2)})

  (check-mutation
   0
   #'{(define/contract (f x)
        any/c
        (<= x 2))
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        any/c
        (< x 2))
      (define/contract b positive? 2)})
  (check-mutation
   0
   #'{(define/contract (f x)
        any/c
        (> x 2))
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        any/c
        (>= x 2))
      (define/contract b positive? 2)})

  (check-mutation
   0
   #'{(define/contract (f x)
        any/c
        (>= x 2))
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        any/c
        (> x 2))
      (define/contract b positive? 2)})
  (check-mutation
   0
   #'{(define/contract (f x)
        any/c
        (= x 2))
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        any/c
        (=/= x 2))
      (define/contract b positive? 2)})

  (check-mutation
   0
   #'{(define/contract (f x)
        any/c
        (=/= x 2))
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        any/c
        (= x 2))
      (define/contract b positive? 2)})
  (check-mutation
   0
   #'{(define/contract (f x)
        any/c
        (=/= x 2))
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        any/c
        (= x 2))
      (define/contract b positive? 2)})

  (check-mutation
   0
   #'{(define/contract (f x)
        any/c
        (+ x 2))
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        any/c
        (- x 2))
      (define/contract b positive? 2)})
  (check-mutation
   0
   #'{(define/contract (f x)
        any/c
        (- x 2))
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        any/c
        (+ x 2))
      (define/contract b positive? 2)})


  (check-mutation
   0
   #'{(define/contract (f x)
        any/c
        (* x 2))
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        any/c
        (/ x 2))
      (define/contract b positive? 2)})
  (check-mutation
   0
   #'{(define/contract (f x)
        any/c
        (/ x 2))
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        any/c
        (* x 2))
      (define/contract b positive? 2)})

  (check-mutation
   0
   #'{(define/contract (f x)
        any/c
        (and x #t))
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        any/c
        (or x #t))
      (define/contract b positive? 2)})
  (check-mutation
   0
   #'{(define/contract (f x)
        any/c
        (or x #t))
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        any/c
        (and x #t))
      (define/contract b positive? 2)})

  (check-mutation
   1
   #'{(define/contract (f x)
        any/c
        (or x #t))
      (define/contract b positive? 2)}
   #'{(define/contract (f x)
        any/c
        (or x #t))
      (define/contract b positive? (add1 2))})

  ;; begin
  (check-mutation
   1
   #'{(define/contract (f x)
        any/c
        (or x #t))
      (define/contract b positive? (begin 1 2))}
   #'{(define/contract (f x)
        any/c
        (or x #t))
      (define/contract b positive? (begin 2))})

  ;; if
  (check-mutation
   2
   #'{(define/contract (f x)
        any/c
        (or x #t))
      (define/contract b positive? (begin 1 2))
      (define/contract (g x)
        any/c
        (if x 1 2))}
   #'{(define/contract (f x)
        any/c
        (or x #t))
      (define/contract b positive? (begin 1 2))
      (define/contract (g x)
        any/c
        (if (not x) 1 2))})


  ;; classes
  (check-mutation
   0
   #'{(define/contract c
        any/c
        (class (define/public (f x) x)))}
   #'{(define/contract c
        any/c
        (class (define/private (f x) x)))})
  (check-mutation
   0
   #'{(define/contract c
        any/c
        (class
          (define/private (g x) x)
          (define/override (f x) x)))}
   #'{(define/contract c
        any/c
        (class
          (define/private (g x) x)
          (define/augment (f x) x)))}))
