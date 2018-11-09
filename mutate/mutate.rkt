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
(define mutation-index? natural?)
(define counter? natural?)
(struct mutated (stx new-counter) #:transparent)
(define (mutated/c a) (struct/c mutated a counter?))
;;(struct mutated-seq (stxs new-counter) #:transparent)

(define (exprs-equal? a b)
    (equal? (syntax->datum a)
            (syntax->datum b)))

;; Test setup
(module+ test
  (require rackunit)
  (require "custom-check.rkt")

  (define programs-equal? exprs-equal?)

  (define-binary-check (check-programs-equal? actual expected)
    (programs-equal? actual expected))

  (define-syntax-rule (check-mutation index orig-prog new-prog)
    (check-programs-equal?
     (mutate-program-wrapper orig-prog index)
     new-prog)))

;; Utilities

;; Manage the decision of whether or not to apply a ready, valid
;; mutation based on `mutation-index` and `counter`.
;; Assumes that `old-stx` and `new-stx` are different.
(define/contract (maybe-mutate old-stx new-stx mutation-index counter)
  (->i ([old-stx syntax?]
        [new-stx (old-stx)
                 (and/c syntax?
                        (not/c (curry exprs-equal? old-stx)))]
        [mutation-index natural?]
        [counter (mutation-index)
                 (and/c natural?
                        (<=/c mutation-index))])
       [result mutated?])
  (mutated
   (if (= mutation-index counter)
       new-stx
       old-stx)
   ;; since this function ONLY gets real mutations that could be applied
   ;; we always increment counter, indicating that a mutatable expr has
   ;; been considered.
   (add1 counter)))

(define-syntax (define-mutators stx)
  (syntax-parse stx
    #:datum-literals (-> <->)
    [(_ (mutator-name stx-name mutation-index counter)
        (~or (orig -> new)
             (left <-> right)) ...
        (match-datum datum-name:id
                     [pat res] ...)
        other-mutations ...)
     #'(define (mutator-name stx-name mutation-index counter)
         (define (maybe-mutate* new-stx)
           (maybe-mutate stx-name
                         new-stx
                         mutation-index
                         counter))
         (if (and make-mutants
                  (<= counter mutation-index))
             (syntax-parse stx-name
               #:literals (orig ... left ... right ...)
               [orig
                (maybe-mutate* (syntax/loc stx-name new))]
               ...
               [left
                (maybe-mutate* (syntax/loc stx-name right))]
               ...
               [right
                (maybe-mutate* (syntax/loc stx-name left))]
               ...
               [(~and datum-name
                      (~not (fn arg (... ...))))
                (match (syntax->datum #'datum-name)
                  [pat (maybe-mutate* (syntax/loc stx-name res))]
                  ...
                  [_ (mutated stx-name
                              counter)])]
               other-mutations
               ...
               [other
                (mutated stx-name
                         counter)])
             (mutated stx-name
                      counter)))]))

(define/contract (unmutated? orig mutated)
  (syntax? syntax? . -> . boolean?)
  (equal? (syntax->datum orig) (syntax->datum mutated)))

(define =/= (compose not =))

(define A any/c)
(define B any/c)
;; maps over mutated-stx
(define/contract (mmap f m)
  ((A . -> . B)
   (mutated/c A)
   . -> .
   (mutated/c B))

  (mutated (f (mutated-stx m))
           (mutated-new-counter m)))

(define/contract (mbind f m)
  ((A counter? . -> . (mutated/c B))
   (mutated/c A)
   . -> .
   (mutated/c B))

  (f (mutated-stx m)
     (mutated-new-counter m)))


#|----------------------------------------------------------------------|#
;; Mutators: functions that actually mutate syntax

;; mutate-datum: stx nat nat -> mutated
(define-mutators (mutate-datum stx mutation-index counter)
  ;; operator mutation
  (< <-> <=)
  (> <-> >=)
  (= <-> =/=)
  (+ <-> -)
  (* <-> /)
  (and <-> or)
  (modulo -> *)
  (define/public -> define/private)
  (define/override <-> define/augment)

  ;; constant mutation
  (match-datum value
               [(? boolean?)  (not value)]
               [1             0]
               [-1            1]
               [5             -1]
               [(? integer?)  (add1 value)]
               [(? number?)   (- -1 value)]))

(module+ test
  (define-syntax (check-datum-mutations stx)
    (syntax-parse stx
      [(_ (~or (orig -> mutated)
               (left <-> right)) ...)
       #'(begin
           (check-programs-equal? (mutated-stx (mutate-datum #'orig 0 0))
                                  #'mutated) ...
           (check-programs-equal? (mutated-stx (mutate-datum #'left 0 0))
                                  #'right) ...
           (check-programs-equal? (mutated-stx (mutate-datum #'right 0 0))
                                  #'left) ...)]))
  (check-datum-mutations
   (< <-> <=)
   (> <-> >=)
   (= <-> =/=)
   (+ <-> -)
   (* <-> /)
   (and <-> or)
   (modulo -> *)
   (define/public -> define/private)
   (define/override <-> define/augment)

   (#t -> (not #t))
   (#f -> (not #f))
   (1 -> 0)
   (-1 -> 1)
   (5 -> -1)
   (7 -> (add1 7))
   (2 -> (add1 2))
   (2.2 -> (- -1 2.2))
   (5.7 -> (- -1 5.7))

   (variable -> variable)
   ('symbol -> 'symbol)
   ('a -> 'a)))


(define/contract (mutate-condition c mutation-index counter)
  (syntax? natural? natural? . -> . mutated?)

  ;; design decision: only try negating conditions
  (if (or (equal? (syntax->datum c) 'else)
          (> counter mutation-index))
      (mutated c counter)
      (maybe-mutate c
                    (quasisyntax/loc c
                      (not #,c))
                    mutation-index
                    counter)))

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
  (pattern ((~datum define/contract) id/sig ctc body ...)))

(define/contract (mutate-expr stx mutation-index counter)
  (syntax? natural? natural? . -> . mutated?)

  (if (and make-mutants
           (<= counter mutation-index))
      (syntax-parse stx
        ;; begin: statement deletion
        [((~datum begin) e1 e2 ...+)
         (define top-level-mutation
           (maybe-mutate stx
                         (syntax/loc stx (begin e2 ...))
                         mutation-index
                         counter))
         ;; if top-level succeeded, then the following will have no effect
         (mbind (λ (stx/mutated new-counter)
                  (mutate-expr* stx/mutated
                                mutation-index
                                new-counter))
                top-level-mutation)]

        ;; conditionals: negate condition, or mutate result exprs
        [((~datum cond) clause ...)
         (define clauses (syntax-e (syntax/loc stx (clause ...))))
         (define mutated-clauses (mutate-cond-in-seq clauses
                                                     mutation-index
                                                     counter))
         (mutated (quasisyntax/loc stx
                    (cond #,@(mutated-stx mutated-clauses)))
                  (mutated-new-counter mutated-clauses))]
        [((~datum if) cond-e then-e else-e)
         (define clause (list (quasisyntax/loc stx
                                [#,@(rest (syntax-e stx))])))
         (define mutated-clause (mutate-cond-in-seq clause
                                                    mutation-index
                                                    counter))
         (mutated (quasisyntax/loc stx
                    (if #,@(first (mutated-stx mutated-clause))))
                  (mutated-new-counter mutated-clause))]

        ;; mutate arbitrary sexp
        [(e ...)
         (mutate-expr* stx mutation-index counter)]

        ;; anything else, just try mutate-datum
        [e
         (mutate-datum stx mutation-index counter)])

      (mutated stx counter)))

;; just applies `mutate-expr` to all subexprs of `stx`
(define (mutate-expr* stx mutation-index counter)
  (define parts (syntax-e stx))
  (define mutated-parts (mutate-in-seq parts
                                       mutation-index
                                       counter
                                       mutate-expr))
  (mutated (quasisyntax/loc stx
             (#,@(mutated-stx mutated-parts)))
           (mutated-new-counter mutated-parts)))


;; mutate-program: stx nat nat -> mutated
;; Note: distinction between mutate-program and mutate-expr
;; is necessary because mutate-program should descend only into
;; contracted top level forms, while mutate-expr can descend into
;; everything (mutate-program acts like its gatekeeper)
(define/contract (mutate-program stx mutation-index [counter 0])
  ((syntax? natural?)
   (natural?)
   . ->* .
   mutated?)

  (if (and make-mutants
           (<= counter mutation-index))
      (syntax-parse stx
        ;; Mutate contracted definition bodies
        ;; (could be class, function, other value)
        [(def:contracted-definition e ...)
         (define mutated-body-forms
           (mutate-in-seq (syntax-e (syntax/loc stx
                                      (def.body ...)))
                          mutation-index
                          counter
                          mutate-expr))
         (match-define (mutated mutated-program-rest new-counter)
           (mutate-program (syntax/loc stx (e ...))
                           mutation-index
                           (mutated-new-counter mutated-body-forms)))
         (mutated (quasisyntax/loc stx
                    ((define/contract def.id/sig def.ctc
                       #,@(mutated-stx mutated-body-forms))
                     #,@mutated-program-rest))
                  new-counter)]

        ;; Ignore anything else
        [(other-e e ...)
         (match-define (mutated mutated-program-rest new-counter)
           (mutate-program (syntax/loc stx (e ...))
                           mutation-index
                           counter))
         (mutated (quasisyntax/loc stx
                    (other-e #,@mutated-program-rest))
                  new-counter)]
        [()
         ;; signal no more mutations in this module
         (raise (mutation-index-exception))])

      (mutated stx counter)))

;; mutate-program-wrapper: syntax? natural? -> syntax?
(define (mutate-program-wrapper stx mutation-index)
  (match-define (mutated p _) (mutate-program stx mutation-index))
  p)

;; end syntax traversers
#|----------------------------------------------------------------------|#

#|----------------------------------------------------------------------|#
;; Sequence helpers
;;
;; These functions select expressions to mutate from a sequence of
;; expressions, primarily to handle the bookkeeping of the counter
;; value for such cases

#;(define/contract (mutate-in-seq stxs mutation-index counter
                                [mutator mutate-datum])
  (((listof syntax?) natural? natural?)
   ((syntax? natural? natural? . -> . mutated?))
   . ->* .
   mutated?)

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
  (mutated stxs-with-mutant new-counter))

(define/contract (mutate-in-seq stxs mutation-index counter
                                mutator)
  (parametric->/c
   [A]
   ((listof (and/c (or/c syntax?
                         (listof syntax?))
                   A))
    natural?
    natural?
    (A natural? natural? . -> . mutated?)
    . -> .
    mutated?))

  (define (add-mutated-element stx stxs-so-far counter-so-far)
    (define mutated-element (mutator stx
                                     mutation-index
                                     counter-so-far))
    (define add-stxs-so-far (curryr cons stxs-so-far))
    (mmap add-stxs-so-far
          mutated-element))

  (for/fold ([mutated-so-far (mutated '() counter)]
             #:result (mmap reverse mutated-so-far))
            ([stx (in-list stxs)])
    (mbind (curry add-mutated-element stx)
           mutated-so-far)))

(module+ test
  ;; lltodo: I'm doing this pat a lot. I should really have a macro
  ;; version of define-check/loc.
  (define-check/loc (check-mutation/in-seq* orig-seq
                                            seq-mutator
                                            mutator
                                            pretty-printer
                                            expects)
    #:ignore-parameters
    (let/cc fail
      (for ([index (in-list (map first expects))]
            [expect (in-list (map second expects))])
        (define ms (mutated-stx (seq-mutator orig-seq
                                             index
                                             0
                                             mutator)))
        (unless (andmap programs-equal?
                        (flatten ms)
                        (flatten expect))
          (fail (format "Result does not match expected output.
Mutation index: ~v
Expected:
~a

Actual:
~a
"
                        index
                        (pretty-printer expect)
                        (pretty-printer ms)))))
      #t))
  (define-syntax-rule (check-mutation/in-seq
                       seq seq-mutator mutator
                       pretty-printer
                       [mutation-index expected] ...)
    (check-mutation/in-seq* seq seq-mutator mutator pretty-printer
                            (list (list mutation-index
                                        expected) ...)))

  (check-mutation/in-seq
   (list #'#t
         #''a
         #'1
         #'(+ 1 2))
   mutate-in-seq
   mutate-datum
   (curry map syntax->datum)
   ;; only elements 0 and 2 are mutatable datums
   [0 (list #'(not #t)
            #''a
            #'1
            #'(+ 1 2))]
   [1 (list #'#t
            #''a
            #'0
            #'(+ 1 2))]
   ;; other elements aren't touched, regardless of mutation-index
   [2 (list #'#t
            #''a
            #'1
            #'(+ 1 2))]
   [3 (list #'#t
            #''a
            #'1
            #'(+ 1 2))]
   [4 (list #'#t
            #''a
            #'1
            #'(+ 1 2))]
   #| ... |#)

  (check-mutation/in-seq
   (list #'#t
         #''a
         #'(test? 2))
   mutate-in-seq
   mutate-condition
   (curry map syntax->datum)
   [0 (list #'(not #t)
            #''a
            #'(test? 2))]
   [1 (list #'#t
            #'(not 'a)
            #'(test? 2))]
   [2 (list #'#t
            #''a
            #'(not (test? 2)))]
   ;; mutation index exceeded size of list, no more will be mutated
   [3 (list #'#t
            #''a
            #'(test? 2))]
   #| ... |#))



(define/contract (mutate-in-seq* stxs mutation-index counter
                                 mutator)
  ((listof (listof syntax?))
   natural?
   natural?
   (syntax? natural? natural? . -> . mutated?)
   . -> .
   mutated?)

  (mutate-in-seq stxs mutation-index counter
                 (curryr mutate-in-seq mutator)))

(module+ test
  (check-mutation/in-seq
   (list (list #'#t #'#f)
         (list #''a #'1)
         (list #'(test? 2)))
   mutate-in-seq*
   mutate-datum
   (curry map
          (curry map syntax->datum))
   ;; template
   #;[ (list (list #'#t #'#f)
            (list #''a #'1)
            (list #'(test? 2)))]
   [0 (list (list #'(not #t) #'#f)
            (list #''a #'1)
            (list #'(test? 2)))]
   [1 (list (list #'#t #'(not #f))
            (list #''a #'1)
            (list #'(test? 2)))]
   [2 (list (list #'#t #'#f)
            (list #''a #'0)
            (list #'(test? 2)))]
   [3 (list (list #'#t #'#f)
            (list #''a #'1)
            ;; mutate-datum won't change applications...
            ;; [[but%20mutate-expr%20will!]]
            (list #'(test? 2)))]
   ;; mutation index exceeded size of list, no more will be mutated
   [4 (list (list #'#t #'#f)
            (list #''a #'1)
            (list #'(test? 2)))]
   #| ... |#)

  (check-mutation/in-seq
   (list (list #'#t #'#f)
         (list #''a #'1)
         (list #'(test? 2)))
   mutate-in-seq*
   mutate-expr
   (curry map
          (curry map syntax->datum))
   [3 (list (list #'#t #'#f)
            (list #''a #'1)
            ;; ... but mutate-expr will!
            (list #'(test? (add1 2))))]))


#;(define/contract (mutate-expr-in-seq* bodies mutation-index counter)
  ((listof (listof syntax?)) natural? natural? . -> . mutated?)

  (for/fold ([mutated-so-far (mutated '() counter)])
            ([body-exprs (in-list bodies)])
    (define mutated-so-far (mutated-stx mutated-so-far))
    (define counter-so-far (mutated-new-counter mutated-so-far))
    (define mutated-body (mutate-expr-in-seq body-exprs
                                             mutation-index
                                             counter-so-far))
    (mutated (append mutated-so-far
                     (list (mutated-stx mutated-body)))
             (mutated-new-counter mutated-body))))

(define cond-clause? (syntax-parser
                       [[condition:expr b:expr r:expr ...]
                        #t]
                       [_ #f]))

(define/contract (mutate-cond-in-seq clauses mutation-index counter)
  ((listof cond-clause?) natural? natural? . -> . mutated?)

  (define unwrapped-clauses (map syntax-e clauses))
  (define conditions (map first unwrapped-clauses))
  (define/contract bodies ;; each condition has a list of body-exprs
    (listof (listof syntax?))
    (map rest unwrapped-clauses))
  (define mutated-conditions-seq
    (mutate-in-seq conditions
                   mutation-index
                   counter
                   mutate-condition))
  (define mutated-conditions (mutated-stx mutated-conditions-seq))
  (define after-conditions-counter
    (mutated-new-counter mutated-conditions-seq))

  (define mutated-bodies-seq
    (mutate-in-seq* bodies
                    mutation-index
                    after-conditions-counter
                    mutate-expr))
  (define mutated-bodies (mutated-stx mutated-bodies-seq))
  (define new-counter (mutated-new-counter mutated-bodies-seq))
  (mutated (map cons
                mutated-conditions
                mutated-bodies)
           new-counter))

(module+ test
  (define-check/loc (check-mutation/in-conds orig-seq
                                             expects)
    #:ignore-parameters
    (let/cc fail
      (for ([index (in-list (map first expects))]
            [expect (in-list (map second expects))])
        (define cond-lists
          (mutated-stx (mutate-cond-in-seq orig-seq
                                           index
                                           0)))
        (define cond-clauses
          (map (λ (cond-clause) #`[#,@cond-clause])
               cond-lists))
        (unless (andmap programs-equal?
                        cond-clauses
                        expect)
          (fail (format "Result does not match expected output.
Mutation index: ~v
Expected:
~a

Actual:
~a
"
                        index
                        (map syntax->datum expect)
                        (map syntax->datum cond-clauses)))))
      #t))

  (check-mutation/in-conds
   (list #'[#t 1 2 3]
         #'[(bool? 5) (foo 7)]
         #'[else (bar (+ x 2) #f)])
   ;; template
   #;[ (list #'[#t 1 2 3]
            #'[(bool? 5) (foo 7)]
            #'[else (bar (+ x 2) #f)])]
   `([0 (,#'[(not #t) 1 2 3]
         ,#'[(bool? 5) (foo 7)]
         ,#'[else (bar (+ x 2) #f)])]
     [1 (,#'[#t 1 2 3]
         ,#'[(not (bool? 5)) (foo 7)]
         ,#'[else (bar (+ x 2) #f)])]
     ;; end conditions, enter bodies
     [2 (,#'[#t 0 2 3]
         ,#'[(bool? 5) (foo 7)]
         ,#'[else (bar (+ x 2) #f)])]
     [3 (,#'[#t 1 (add1 2) 3]
         ,#'[(bool? 5) (foo 7)]
         ,#'[else (bar (+ x 2) #f)])]
     [4 (,#'[#t 1 2 (add1 3)]
         ,#'[(bool? 5) (foo 7)]
         ,#'[else (bar (+ x 2) #f)])]
     [5 (,#'[#t 1 2 3]
         ,#'[(bool? 5) (foo (add1 7))]
         ,#'[else (bar (+ x 2) #f)])]
     [6 (,#'[#t 1 2 3]
         ,#'[(bool? 5) (foo 7)]
         ,#'[else (bar (- x 2) #f)])]
     [7 (,#'[#t 1 2 3]
         ,#'[(bool? 5) (foo 7)]
         ,#'[else (bar (+ x (add1 2)) #f)])]
     [8 (,#'[#t 1 2 3]
         ,#'[(bool? 5) (foo 7)]
         ,#'[else (bar (+ x 2) (not #f))])]
     ;; no more left
     [9 (,#'[#t 1 2 3]
         ,#'[(bool? 5) (foo 7)]
         ,#'[else (bar (+ x 2) #f)])]
     #| ... |#)))

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
  (check-exn
   mutation-index-exception?
   (λ ()
     (mutate-program-wrapper
      #'{(define/contract a positive? (λ (x) x))}
      0)))

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
   ;; Fixed this! ⇓
   #;[1 #'{(define/contract (f x)
           any/c
           (or x #t)) ;; tries to mutate `x` but it's a no-op
         (define/contract b positive? 2)}]
   [1 #'{(define/contract (f x)
           any/c
           (or x (not #t)))
         (define/contract b positive? 2)}]
   [2 #'{(define/contract (f x)
           any/c
           (or x #t))
         (define/contract b positive? (add1 2))}])

  ;; begin
  (check-mutation
   2
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
   5
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
   [2 #'{(define/contract (f x)
           any/c
           (or x #t))
         (define/contract b positive? (begin 2))
         (define/contract (g x)
           any/c
           (if x 1 2))}]
   [3 #'{(define/contract (f x)
           any/c
           (or x #t))
         (define/contract b positive? (begin 0 2))
         (define/contract (g x)
           any/c
           (if x 1 2))}]
   [4 #'{(define/contract (f x)
           any/c
           (or x #t))
         (define/contract b positive? (begin 1 (add1 2)))
         (define/contract (g x)
           any/c
           (if x 1 2))}]
   [5 #'{(define/contract (f x)
           any/c
           (or x #t))
         (define/contract b positive? (begin 1 2))
         (define/contract (g x)
           any/c
           (if (not x) 1 2))}]
   [6 #'{(define/contract (f x)
           any/c
           (or x #t))
         (define/contract b positive? (begin 1 2))
         (define/contract (g x)
           any/c
           (if x 0 2))}]
   [7 #'{(define/contract (f x)
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
              [(foobar (+ 1 2)) -1 5]
              [else (error (quote wrong))]))
      (displayln (quasiquote (c (unquote c))))
      (displayln (quasiquote (d (unquote d))))}
   ;; tries all conditions in sequence,
   [1 #'{(displayln "B")
         (define/contract c any/c 1)
         (define/contract d any/c
           (cond [(not #t) 1]
                 [(foobar (+ 1 2)) -1 5]
                 [else (error (quote wrong))]))
         (displayln (quasiquote (c (unquote c))))
         (displayln (quasiquote (d (unquote d))))}]
   [2 #'{(displayln "B")
         (define/contract c any/c 1)
         (define/contract d any/c
           (cond [#t 1]
                 [(not (foobar (+ 1 2))) -1 5]
                 [else (error (quote wrong))]))
         (displayln (quasiquote (c (unquote c))))
         (displayln (quasiquote (d (unquote d))))}]
   ;; then the bodies in sequence
   [3 #'{(displayln "B")
         (define/contract c any/c 1)
         (define/contract d any/c
           (cond [#t 0]
                 [(foobar (+ 1 2)) -1 5]
                 [else (error (quote wrong))]))
         (displayln (quasiquote (c (unquote c))))
         (displayln (quasiquote (d (unquote d))))}]
   [4 #'{(displayln "B")
         (define/contract c any/c 1)
         (define/contract d any/c
           (cond [#t 1]
                 [(foobar (+ 1 2)) 1 5]
                 [else (error (quote wrong))]))
         (displayln (quasiquote (c (unquote c))))
         (displayln (quasiquote (d (unquote d))))}]
   [5 #'{(displayln "B")
         (define/contract c any/c 1)
         (define/contract d any/c
           (cond [#t 1]
                 [(foobar (+ 1 2)) -1 -1]
                 [else (error (quote wrong))]))
         (displayln (quasiquote (c (unquote c))))
         (displayln (quasiquote (d (unquote d))))}])


  ;; lltodo: bug here
  ;; This is the applying mutate-expr to args of a function one
  ;; It's doing:
  ;; (+ a b) -> (- a b) -> (+ (mutate-datum a) b) -> (+ a (mutate-datum b))
  ;; Should be:
  ;; (+ a b) -> (- a b) -> (+ (mutate-expr a) b) -> (+ a (mutate-expr b))
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
  ;; Then my mutate-datum primitive should handle operators by mutating
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
;; 1. [✓] Move expression-level mutations into mutate-body
;; 2. [✓] Change mutate to be datums-only
;; 3. [✓] Rename the functions
;; 4. [✓] Change handling of function apps



;; lltodo: also, still need to make the mutate functions insert the
;; invocation to enter the "bug" region of the program.

;; lltodo: first, should refactor all the places that I take apart
;; mutated and put it back together to use mmap and mbind
