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

;; mutate: stx -> stx
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
   (match (syntax->datum #'value)
     [(? boolean?)
      #'(not value)]
     [1 #'0]
     [-1 #'1]
     [5 #'-1]
     [(? integer?) #'(add1 value)]
     [(? number?) #'(- -1 value)]
     [_
      #'value])])

;; lltodo: wiw: refactoring mutate-body and mutate-program
;; to allow creating multiple mutants from a single expression.
;; To do so, need to return not only the (possibly mutated) syntax,
;; but also a new counter indicating if mutation occurred.
(define-syntax-class method/public-or-override
  #:description "define/public or /override method"
  (pattern (~or ((~datum define/public) id+other/pub ...)
                ((~datum define/override) id+other/over ...))
           #:with pub #'(~? (define/public id+other/pub ...))
           #:with over #'(~? (define/override id+other/over ...))))

(define (drop-index lst idx)
  (append (take lst idx)
          (take-right lst (- (length lst) idx 1))))

(struct mutated-seq (result new-counter))

(define/contract (mutate-in-sequence stxs mutation-index counter)
  ((listof syntax?) natural? natural? . -> . mutated-seq?)

  (define stxs-count (length stxs))
  (define index-to-mutate (- mutation-index counter))
  (define should-mutate? (< index-to-mutate stxs-count))
  (define new-counter (+ counter stxs-count)) ;; record each el was considered
  (define stxs-with-mutant
    (if should-mutate?
        (append (take stxs index-to-mutate)
                (mutate (list-ref stxs index-to-mutate))
                (drop stxs (add1 index-to-mutate)))
        stxs))
  (mutated-seq stxs-with-mutant new-counter))

;; mutate-body: stx nat nat -> (values stx <counter val>)
;;
;; This is the only function that calls mutate, so it needs to do the
;; bookkeeping for it.
(define (mutate-body stx mutation-index counter)
  (if (and make-mutants
           (<= counter mutation-index))
      (syntax-parse stx
        ;; modify class methods
        [((~datum class)
          e ...
          m:method/public-or-override ...
          more-e ...)
         (define pub-methods (syntax-e #'(m.pub ...)))
         (define over-methods (syntax-e #'(m.over ...)))
         (define all-methods (append pub-methods over-methods))
         (define mutated-methods (mutate-in-sequence all-methods
                                                     mutation-index
                                                     counter))
         (values #`(class e ...
                         #,@(mutated-seq-result mutated-methods)
                         more-e ...)
                 (mutated-seq-new-counter mutated-methods))]
        ;; leave other classes alone
        [((~datum class) e ...)
         (values #'(class e ...)
                 counter)]

        ;; modify function apps
        [((~and fn:id (~not (~or (~datum λ)
                                 (~datum lambda)))) arg ...)
         ;; A function application may not have an applicable mutation
         ;; First, see if it does..
         (define mutated (mutate #'(fn arg ...)))
         (define app-mutation-failed? (equal? (syntax->datum #'(fn arg ...))
                                              (syntax->datum mutated)))
         (define args (syntax-e #'(arg ...)))
         (define mutated-args (mutate-in-sequence args mutation-index counter))
         (if app-mutation-failed?
             ;; if no applicable mutation, push it down to arguments
             (values #`(fn #,@(mutated-seq-result mutated-args))
                     (mutated-seq-new-counter mutated-args))
             ;; if there was one, stop there
             (values mutated
                     (add1 counter)))]

        ;; anything else, just try mutate
        [e
         (values (mutate #'e)
                 (add1 counter))])

      (values stx counter)))


(define/contract (mutate-body-in-sequence stxs mutation-index counter)
  ((listof syntax?) natural? natural? . -> . mutated-seq?)

  (define-values (mutated new-counter)
    (for/fold ([mutated-seq/rev '()]
               [counter-so-far counter])
              #:result (values (reverse mutated-seq/rev) counter-so-far)
              ([body-stx (in-list stxs)])
      (define-values (mutated-stx new-counter)
        (mutate-body body-stx mutation-index counter-so-far))
      (define new-seq (cons mutated-stx mutated-seq/rev))
      (values new-seq new-counter)))

  (mutated-seq mutated new-counter))

;; lltodo: decisions about when to mutate should be on an
;; expression-by-expression basis, not definitions
;; Just need to push the mutate? logic down lower, and
;; have mutate-body and mutate-expr return the new counter
;; somehow

(define-syntax-class contracted-definition
  #:description "define/contract form"
  (pattern (define/contract id/sig ctc body ...)))

;; mutate-program: stx nat nat -> (values stx <counter val>)
(define (mutate-program stx mutation-index [counter 0])
  (if (and make-mutants
           (<= counter mutation-index))
      (syntax-parse stx
        ;; Mutate definition body (could be class, function, other value)
        [(def:contracted-definition e ...)
         (define-values (mutated-body-forms)
           (mutate-body-in-sequence #'(def.body ...)))
         (define-values (mutated-program-rest new-counter)
           (mutate-program #'(e ...)
                           mutation-index
                           (mutated-seq-new-counter mutated-body-forms)))
         (values #`((define/contract def.id/sig def.ctc
                      #,@(mutated-seq-result mutated-body-forms))
                    #,@mutated-program-rest)
                 new-counter)]

        ;; Ignore anything else
        [(other-e e ...)
         ;; lltodo: if this works then should refactor to use defines
         (define-values (mutated-program-rest new-counter)
           (mutate-program #'(e ...)
                           mutation-index
                           counter))
         (values #`(other-e #,@mutated-program-rest)
                 new-counter)]
        [()
         ;; signal no more mutations in this module
         (error 'mutate-program
                "Mutation index exceeds mutatable forms in module")])

      (values stx counter)))

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
