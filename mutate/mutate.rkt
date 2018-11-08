#lang racket

(provide
 (contract-out
  [rename mutate-program-wrapper
          mutate-program
          ;; note: result may be unchanged!
          (syntax? natural? . -> . syntax?)])
 mutation-index-exception?)

(require (for-syntax syntax/parse)
         syntax/parse
         racket/match)

;; Data
(define make-mutants #t)
(struct mutation-index-exception ())
(struct mutated-seq (result new-counter) #:transparent)

;; Test setup
(module+ test
  (require rackunit)
  (require "custom-check.rkt")

  (define (programs-equal? a b)
    (equal? (syntax->datum a)
            (syntax->datum b)))

  (define-binary-check (check-programs-equal? actual expected)
    (programs-equal? actual expected))

  (define-syntax-rule (check-mutation index orig-prog new-prog)
    (check-programs-equal?
     (mutate-program-wrapper orig-prog index)
     new-prog)))

;; Utilities
(define-syntax (define-mutators stx)
  (syntax-parse stx
    #:datum-literals (-> <->)
    [(_ (mutator-name stx-name)
        (~or (orig -> mutated)
             (left <-> right)) ...
        other-mutations ...)
     #'(define (mutator-name stx-name)
         (if make-mutants
             (syntax-parse stx-name
               #:literals (orig ... left ... right ...)
               [(orig arg (... ...))
                (syntax/loc stx-name (mutated arg (... ...)))] ...
               (~@ [(left arg (... ...))
                    (syntax/loc stx-name (right arg (... ...)))] ...
                   [(right arg (... ...))
                    (syntax/loc stx-name (left arg (... ...)))] ...)
               other-mutations ...
               [other
                stx-name])
             stx-name))]))

(define/contract (unmutated? orig mutated)
  (syntax? syntax? . -> . boolean?)
  (equal? (syntax->datum orig) (syntax->datum mutated)))

(define =/= (curry not =))


#|----------------------------------------------------------------------|#
;; Mutators: functions that actually mutate syntax

;; mutate: stx -> stx
(define-mutators (mutate stx)
  ;; operator mutation
  (< <-> <=)
  (> <-> >=)
  (= <-> =/=)
  (+ <-> -)
  (* <-> /)
  (and <-> or)
  (modulo -> *)

  ;; statement deletion
  [((~datum begin) e1 e2 ...+)
   (syntax/loc stx (begin e2 ...))]

  ;; Classes
  ;; access modifier change
  [((~datum define/public) id+other ...)
   (syntax/loc stx (define/private id+other ...))]
  [((~datum define/override) id+other ...)
   (syntax/loc stx (define/augment id+other ...))]

  ;; constant mutation
  [(~and value
         (~not (fn arg ...)))
   (match (syntax->datum #'value)
     [(? boolean?)  (syntax/loc stx (not value))]
     [1             (syntax/loc stx 0)]
     [-1            (syntax/loc stx 1)]
     [5             (syntax/loc stx -1)]
     [(? integer?)  (syntax/loc stx (add1 value))]
     [(? number?)   (syntax/loc stx (- -1 value))]
     [_             (syntax/loc stx value)])])


(define/contract (mutate-condition c)
  (syntax? . -> . syntax?)

  ;; design decision: only try negating conditions
  (if (equal? (syntax->datum c) 'else)
      c
      (quasisyntax/loc c
        (not #,c))))

;; end mutators
#|----------------------------------------------------------------------|#

#|----------------------------------------------------------------------|#
;; Syntax traversers
;;
;; These functions traverse the program syntax finding where to apply
;; the above mutators

(define-syntax-class method/public-or-override
  #:description "define/public or /override method"
  (pattern (~or ((~datum define/public) id+other/pub ...)
                ((~datum define/override) id+other/over ...))))
(define-syntax-class contracted-definition
  #:description "define/contract form"
  (pattern (define/contract id/sig ctc body ...)))

;; mutate-body: stx nat nat -> (values stx <counter val>)
(define (mutate-body stx mutation-index counter)
  (if (and make-mutants
           (<= counter mutation-index))
      (syntax-parse stx
        ;; modify class methods
        [((~datum class)
          (~and (~not _:method/public-or-override)
                e) ...
          m:method/public-or-override ...
          more-e ...)
         (define methods (syntax-e (syntax/loc stx (m ...))))
         (define mutated-methods (mutate-in-sequence methods
                                                     mutation-index
                                                     counter))
         (values (quasisyntax/loc stx
                   (class e ...
                     #,@(mutated-seq-result mutated-methods)
                     more-e ...))
                 (mutated-seq-new-counter mutated-methods))]
        ;; leave other classes alone
        [((~datum class) e ...)
         (values (syntax/loc stx (class e ...))
                 counter)]

        ;; modify conditionals
        [((~datum cond) clause ...)
         (define clauses (syntax-e (syntax/loc stx (clause ...))))
         (define mutated-clauses (mutate-cond-in-sequence clauses
                                                  mutation-index
                                                  counter))
         (values (quasisyntax/loc stx
                   (cond #,@(mutated-seq-result mutated-clauses)))
                 (mutated-seq-new-counter mutated-clauses))]
        [((~datum if) cond-e then-e else-e)
         (define clause (list (quasisyntax/loc stx
                                [#,@(rest (syntax-e stx))])))
         (define mutated-clause (mutate-cond-in-sequence clause
                                                 mutation-index
                                                 counter))
         (values (quasisyntax/loc stx
                   (if #,@(first (mutated-seq-result mutated-clause))))
                 (mutated-seq-new-counter mutated-clause))]

        ;; modify function apps
        [((~and fn:id (~not (~or (~datum λ)
                                 (~datum lambda)))) arg ...)
         ;; A function application may not have an applicable mutation
         ;; First, see if it does..
         (define mutated (mutate (syntax/loc stx (fn arg ...))))
         (define app-mutation-failed? (unmutated? #'(fn arg ...) mutated))

         (cond [(or app-mutation-failed?
                    (< counter mutation-index))
                ;; If app-mutation was successful but counter is not
                ;; yet at mutation-index, we also try mutating the
                ;; args but record that we skipped mutating the app
                (define args-muation-counter (if app-mutation-failed?
                                                 counter
                                                 (add1 counter)))
                (define args (syntax-e (syntax/loc stx (arg ...))))
                ;; lltodo: this should call mutate-body-in-sequence
                (define mutated-args (mutate-in-sequence args
                                                         mutation-index
                                                         args-muation-counter))
                (values (quasisyntax/loc stx
                          (fn #,@(mutated-seq-result mutated-args)))
                        (mutated-seq-new-counter mutated-args))]

               [(and (not app-mutation-failed?)
                     (= counter mutation-index))
                (values mutated
                        (add1 counter))]

               [else
                (values stx
                        (add1 counter))])]

        ;; anything else, just try mutate
        [e
         (values (if (= counter mutation-index)
                     (mutate (syntax/loc stx e))
                     stx)
                 (add1 counter))])

      (values stx counter)))


;; mutate-program: stx nat nat -> (values stx <counter val>)
(define (mutate-program stx mutation-index [counter 0])
  (if (and make-mutants
           (<= counter mutation-index))
      (syntax-parse stx
        ;; Mutate definition body (could be class, function, other value)
        [(def:contracted-definition e ...)
         (define-values (mutated-body-forms)
           (mutate-body-in-sequence (syntax-e (syntax/loc stx
                                                (def.body ...)))
                                    mutation-index
                                    counter))
         (define-values (mutated-program-rest new-counter)
           (mutate-program (syntax/loc stx (e ...))
                           mutation-index
                           (mutated-seq-new-counter mutated-body-forms)))
         (values (quasisyntax/loc stx
                   ((define/contract def.id/sig def.ctc
                      #,@(mutated-seq-result mutated-body-forms))
                    #,@mutated-program-rest))
                 new-counter)]

        ;; Ignore anything else
        [(other-e e ...)
         (define-values (mutated-program-rest new-counter)
           (mutate-program (syntax/loc stx (e ...))
                           mutation-index
                           counter))
         (values (quasisyntax/loc stx
                   (other-e #,@mutated-program-rest))
                 new-counter)]
        [()
         ;; signal no more mutations in this module
         (raise (mutation-index-exception))])

      (values stx counter)))

;; mutate-program-wrapper: syntax? natural? -> syntax?
(define (mutate-program-wrapper stx mutation-index)
  (define-values (mutated _) (mutate-program stx mutation-index))
  mutated)

;; end syntax traversers
#|----------------------------------------------------------------------|#

#|----------------------------------------------------------------------|#
;; Sequence helpers
;;
;; These functions select expressions to mutate from a sequence of
;; expressions, primarily to handle the bookkeeping of the counter
;; value for such cases

(define/contract (mutate-in-sequence stxs mutation-index counter
                                     [mutator mutate])
  (((listof syntax?) natural? natural?)
   ((syntax? . -> . syntax?))
   . ->* .
   mutated-seq?)

  (define stxs-count (length stxs))
  (define index-to-mutate (- mutation-index counter))
  (define should-mutate? (< index-to-mutate stxs-count))
  (define new-counter (+ counter stxs-count)) ;; record each el was considered
  (define stxs-with-mutant
    (if should-mutate?
        (append (take stxs index-to-mutate)
                (list (mutator (list-ref stxs index-to-mutate)))
                (drop stxs (add1 index-to-mutate)))
        stxs))
  (mutated-seq stxs-with-mutant new-counter))

(module+ test
  (define-syntax-rule (sequence-check/mutate-in-sequence
                       seq mutation-index mutator
                       [counter expected] ...)
    (begin
      (check-true (andmap programs-equal?
                          (mutated-seq-result
                           (mutate-in-sequence seq
                                               mutation-index
                                               counter mutator))
                          expected))
      ...))

  (sequence-check/mutate-in-sequence
   (list #'#t #''a #'(+ 1 2)) 3 mutate
   [3 (list #'(not #t)
            #'(quote a)
            #'(+ 1 2))]
   [2 (list #'#t
            #'(quote a)
            #'(+ 1 2))]
   [1 (list #'#t
            #'(quote a)
            #'(- 1 2))]
   [0 (list #'#t
            #'(quote a)
            #'(+ 1 2))])

  (sequence-check/mutate-in-sequence
   (list #'#t #''a #'(test? 2)) 3 mutate-condition
   [3 (list #'(not #t)
            #'(quote a)
            #'(test? 2))]
   [2 (list #'#t
            #'(not (quote a))
            #'(test? 2))]
   [1 (list #'#t
            #'(quote a)
            #'(not (test? 2)))]
   [0 (list #'#t
            #'(quote a)
            #'(test? 2))]))


(define/contract (mutate-body-in-sequence stxs mutation-index counter)
  ((listof syntax?) natural? natural? . -> . mutated-seq?)

  (define-values (mutated new-counter)
    (for/fold ([mutated-seq/rev '()]
               [counter-so-far counter]
               #:result (values (reverse mutated-seq/rev) counter-so-far))
              ([body-stx (in-list stxs)])
      (define-values (mutated-stx new-counter)
        (mutate-body body-stx mutation-index counter-so-far))
      (define new-seq (cons mutated-stx mutated-seq/rev))
      (values new-seq new-counter)))

  (mutated-seq mutated new-counter))



(define cond-clause? (syntax-parser
                       [[condition:expr b:expr r:expr ...]
                        #t]
                       [_ #f]))

(define/contract (mutate-body-in-sequence* bodies mutation-index counter)
  ((listof (listof syntax?)) natural? natural? . -> . mutated-seq?)

  (for/fold ([mutated-so-far (mutated-seq '() counter)])
            ([body-exprs (in-list bodies)])
    (define mutated-seq-so-far (mutated-seq-result mutated-so-far))
    (define counter-so-far (mutated-seq-new-counter mutated-so-far))
    (define mutated-body (mutate-body-in-sequence body-exprs
                                                  mutation-index
                                                  counter-so-far))
    (mutated-seq (append mutated-seq-so-far
                         (list (mutated-seq-result mutated-body)))
                 (mutated-seq-new-counter mutated-body))))


(define/contract (mutate-cond-in-sequence clauses mutation-index counter)
  ((listof cond-clause?) natural? natural? . -> . mutated-seq?)

  (define unwrapped-clauses (map syntax-e clauses))
  (define conditions (map first unwrapped-clauses))
  (define/contract bodies ;; each condition has a list of body-exprs
    (listof (listof syntax?))
    (map rest unwrapped-clauses))
  (define mutated-conditions-seq
    (mutate-in-sequence conditions
                        mutation-index
                        counter
                        mutate-condition))
  (define mutated-conditions (mutated-seq-result mutated-conditions-seq))
  (define after-conditions-counter
    (mutated-seq-new-counter mutated-conditions-seq))

  (define mutated-bodies-seq
    (mutate-body-in-sequence* bodies
                              mutation-index
                              after-conditions-counter))
  (define mutated-bodies (mutated-seq-result mutated-bodies-seq))
  (define new-counter (mutated-seq-new-counter mutated-bodies-seq))
  (mutated-seq (map cons
                    mutated-conditions
                    mutated-bodies)
               new-counter))

;; end sequence helpers
#|----------------------------------------------------------------------|#

#|----------------------------------------------------------------------|#
;; Full program mutation tests

;; lltodo: class mutation should ALSO descend into the bodies of
;; methods

(module+ test
  (define-syntax-rule (check-mutation/sequence orig-program
                                               [index expect] ...)
    (check-mutation/sequence* orig-program
                              (list (list index expect) ...)))
  (define-check/loc (check-mutation/sequence* orig-program expects)
    #:ignore-parameters
    (let/cc fail
      (for ([expect (in-list expects)])
        (match-define (list mutation-index expected) expect)
        (define actual (mutate-program-wrapper orig-program
                                               mutation-index))
        (unless (programs-equal? actual expected)
          (fail
           (string-append
            (format
             "Result does not match expected output.
Mutaton index: ~v
Expected:
~a

Actual:
~a
"
             mutation-index
             (pretty-format (syntax->datum expected))
             (pretty-format (syntax->datum actual)))))))
      #t))

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

  ;; Check sequence
  (check-mutation/sequence
   #'{(define/contract a positive? 1)
      (define/contract b positive? 2)}
   [0 #'{(define/contract a positive? 0)
         (define/contract b positive? 2)}]
   [1 #'{(define/contract a positive? 1)
         (define/contract b positive? (add1 2))}])

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
  (check-mutation/sequence
   #'{(define/contract a any/c (+ 1 2))}
   [0 #'{(define/contract a any/c (- 1 2))}]
   [1 #'{(define/contract a any/c (+ 0 2))}]
   [2 #'{(define/contract a any/c (+ 1 (add1 2)))}])
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

  ;; Test choices of index
  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (or x #t))
      (define/contract b positive? 2)}
   [1 #'{(define/contract (f x)
           any/c
           (or x #t)) ;; tries to mutate `x` but it's a no-op
         (define/contract b positive? 2)}]
   [2 #'{(define/contract (f x)
           any/c
           (or x (not #t)))
         (define/contract b positive? 2)}]
   [3 #'{(define/contract (f x)
           any/c
           (or x #t))
         (define/contract b positive? (add1 2))}])

  ;; begin
  (check-mutation
   3
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
   6
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


  ;; Another sequence check on this more complex if program
  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (or x #t))
      (define/contract b positive? (begin 1 2))
      (define/contract (g x)
        any/c
        (if x 1 2))}
   [3 #'{(define/contract (f x)
           any/c
           (or x #t))
         (define/contract b positive? (begin 2))
         (define/contract (g x)
           any/c
           (if x 1 2))}]
   [4 #'{(define/contract (f x)
           any/c
           (or x #t))
         (define/contract b positive? (begin 0 2))
         (define/contract (g x)
           any/c
           (if x 1 2))}]
   [5 #'{(define/contract (f x)
           any/c
           (or x #t))
         (define/contract b positive? (begin 1 (add1 2)))
         (define/contract (g x)
           any/c
           (if x 1 2))}]
   [6 #'{(define/contract (f x)
           any/c
           (or x #t))
         (define/contract b positive? (begin 1 2))
         (define/contract (g x)
           any/c
           (if (not x) 1 2))}]
   [7 #'{(define/contract (f x)
           any/c
           (or x #t))
         (define/contract b positive? (begin 1 2))
         (define/contract (g x)
           any/c
           (if x 0 2))}]
   [8 #'{(define/contract (f x)
           any/c
           (or x #t))
         (define/contract b positive? (begin 1 2))
         (define/contract (g x)
           any/c
           (if x 1 (add1 2)))}])

  ;; Test exception thrown when run out of mutations
  (check-exn
   mutation-index-exception?
   (λ ()
     (mutate-program #'{(define/contract (f x)
                          any/c
                          (or x #t))
                        (define/contract b positive? (begin 1 2))
                        (define/contract (g x)
                          any/c
                          (if x 1 2))}
                     10)))


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
          (define/augment (f x) x)))})

  ;; Test sequencing of class methods
  (check-mutation/sequence
   #'{(define/contract c
        any/c
        (class
          (define/override (f x) x)
          (define/public (g x) x)))}
   [0 #'{(define/contract c
           any/c
           (class
             (define/augment (f x) x)
             (define/public (g x) x)))}]
   [1 #'{(define/contract c
           any/c
           (class
             (define/override (f x) x)
             (define/private (g x) x)))}])

  ;; Test that condition expressions are only ever considered for
  ;; mutation once: to negate them
  (check-mutation/sequence
   #'{(displayln "B")
      (define/contract c any/c 1)
      (define/contract d any/c (if #t 1 (error (quote wrong))))
      (displayln (quasiquote (c (unquote c))))
      (displayln (quasiquote (d (unquote d))))}
   [1 #'{(displayln "B")
         (define/contract c any/c 1)
         (define/contract d any/c (if (not #t) 1 (error (quote wrong))))
         (displayln (quasiquote (c (unquote c))))
         (displayln (quasiquote (d (unquote d))))}]
   [2 #'{(displayln "B")
         (define/contract c any/c 1)
         (define/contract d any/c (if #t 0 (error (quote wrong))))
         (displayln (quasiquote (c (unquote c))))
         (displayln (quasiquote (d (unquote d))))}])
  (check-mutation/sequence
   #'{(displayln "B")
      (define/contract c any/c 1)
      (define/contract d any/c
        (cond [#t 1]
              [else (error (quote wrong))]))
      (displayln (quasiquote (c (unquote c))))
      (displayln (quasiquote (d (unquote d))))}
   [1 #'{(displayln "B")
         (define/contract c any/c 1)
         (define/contract d any/c
           (cond [(not #t) 1]
                 [else (error (quote wrong))]))
         (displayln (quasiquote (c (unquote c))))
         (displayln (quasiquote (d (unquote d))))}]
   ;; tries all conditions in sequence, then the bodies in sequence
   [2 #'{(displayln "B")
         (define/contract c any/c 1)
         (define/contract d any/c
           (cond [#t 1]
                 [else (error (quote wrong))]))
         (displayln (quasiquote (c (unquote c))))
         (displayln (quasiquote (d (unquote d))))}]
   [3 #'{(displayln "B")
         (define/contract c any/c 1)
         (define/contract d any/c
           (cond [#t 0]
                 [else (error (quote wrong))]))
         (displayln (quasiquote (c (unquote c))))
         (displayln (quasiquote (d (unquote d))))}])


  ;; lltodo: bug here
  ;; This is the applying mutate-body to args of a function one
  ;; It's doing:
  ;; (+ a b) -> (- a b) -> (+ (mutate a) b) -> (+ a (mutate b))
  ;; Should be:
  ;; (+ a b) -> (- a b) -> (+ (mutate-body a) b) -> (+ a (mutate-body b))
  ;;
  ;;
  (check-mutation/sequence
   #'{(define/contract a any/c (+ (+ 1 2)
                                  (- 3 4)))}
   [0 #'{(define/contract a any/c (- (+ 1 2)
                                     (- 3 4)))}]
   [1 #'{(define/contract a any/c (+ (- 1 2)
                                     (- 3 4)))}]
   [2 #'{(define/contract a any/c (+ (+ 0 2)
                                     (- 3 4)))}]
   [3 #'{(define/contract a any/c (+ (+ 1 (add1 2))
                                     (- 3 4)))}]
   [4 #'{(define/contract a any/c (+ (+ 1 2)
                                     (+ 3 4)))}]
   [5 #'{(define/contract a any/c (+ (+ 1 2)
                                     (- (add1 3) 4)))}]
   [6 #'{(define/contract a any/c (+ (+ 1 2)
                                     (- 3 (add1 4))))}])

  ;; This makes me realize: the problem here is just that I treat function
  ;; applications specially
  ;; I should just treat it as a sequence of body-exprs to mutate
  ;; Then my mutate primitive should handle operators by mutating
  ;; + <-> - etc instead of (+ a b) <-> (- a b)
  ;;
  ;; here's a test chrystallizing this in two ways:
  ;; 1. The function being applied is an expression
  ;; 2. Primitives that should be mutated (+, -) appear in expression ctx
  ;;    rather than application ctx
  (check-mutation/sequence
   #'{(define/contract a any/c ((if #t + -) 1 2))}
   [0 #'{(define/contract a any/c ((if (not #t) + -) 1 2))}]
   [1 #'{(define/contract a any/c ((if #t - -) 1 2))}]
   [2 #'{(define/contract a any/c ((if #t + +) 1 2))}]
   [3 #'{(define/contract a any/c ((if #t + -) 0 2))}]
   [4 #'{(define/contract a any/c ((if #t + -) 1 (add1 2)))}])

  ;; lltodo: bug here
  ;; Very interesting: define -> define/contract
  (check-mutation
   1
   #'{(define (foo x) x)
      (define/contract a any/c (+ (foo 1) 2))}
   #'{(define (foo x) x)
      (define/contract a any/c (+ (foo 0) 2))}))

;; ll: wiw:
;; change the function application handling as follows:
;; 1. Treat function apps as just a sequence of body-exprs to mutate
;; 2. Change mutate to just match their primitive operators alone
;;
;; Ok, then what about `begin`, `define/public`?
;; I think I should just move them over into mutate-body
;; Then I should rename mutate-body -> mutate-expr
;; and rename mutate -> mutate-datum
;;
;; Thus, mutate-body (mutate-expr) will have all of the rules for
;; performing expression-level mutations, while mutate (mutate-datum)
;; will just have single-element/datum mutations
;;
;; So new plan:
;; 1. [] Move expression-level mutations into mutate-body
;; 2. [] Change mutate to be datums-only
;; 3. [] Rename the functions
;; 4. [] Change handling of function apps
