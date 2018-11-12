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
         racket/match
         "mutated.rkt")

#|----------------------------------------------------------------------|#
;; Data; see mutated.rkt
(define make-mutants #t)
(struct mutation-index-exception ())

#|----------------------------------------------------------------------|#

#|----------------------------------------------------------------------|#
;; Test setup
(define (exprs-equal? a b)
  (equal? (syntax->datum a)
          (syntax->datum b)))
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

#|----------------------------------------------------------------------|#
;; Utilities

;; Manages the decision of whether or not to apply a ready, valid
;; mutation based on `mutation-index` and `counter`.
;; Assumes that `old-stx` and `new-stx` are different.
(define/contract (maybe-mutate old-stx new-stx mutation-index counter)
  (->i ([old-stx syntax?]
        [new-stx (old-stx)
                 (and/c syntax?
                        (not/c (curry exprs-equal? old-stx)))]
        [mutation-index mutation-index?]
        [counter (mutation-index)
                 (and/c counter?
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

(define =/= (compose not =))


#|----------------------------------------------------------------------|#
;; Mutators: perform simple syntactic mutations

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
  ;; (define/override <-> define/augment)

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
   ;; (define/override <-> define/augment)

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


;; negates
(define/contract (mutate-condition c mutation-index counter)
  (syntax? mutation-index? counter? . -> . mutated?)

  ;; design decision: only try negating conditions
  (if (or (equal? (syntax->datum c) 'else)
          (> counter mutation-index))
      (mutated c counter)
      (maybe-mutate c
                    (quasisyntax/loc c
                      (not #,c))
                    mutation-index
                    counter)))

;; swaps
(define/contract (mutate-arg-pair args mutation-index counter)
  ((list/c syntax? syntax?) mutation-index? counter? . -> . mutated?)

  (match-define (list arg1 arg2) args)
  (define unmutated-pair (quasisyntax/loc arg1
                           (#,arg1 #,arg2)))
  (if (> counter mutation-index)
      (mutated unmutated-pair counter)
      (maybe-mutate unmutated-pair
                    (quasisyntax/loc arg1
                      (#,arg2 #,arg1))
                    mutation-index
                    counter)))

;; end mutators
#|----------------------------------------------------------------------|#

#|----------------------------------------------------------------------|#
;; Syntax traversers
;;
;; These functions traverse the program syntax finding where to apply
;; mutations.
;; Traversers can only:
;; 1. Apply structural/semantic mutations like moving expressions around
;;    or negating conditionals
;; 2. Use one of the above mutators to perform syntax substitions

(define-syntax-class method/public-or-override
  #:description "define/public or /override method"
  (pattern (~or ((~datum define/public) id+other/pub ...)
                ((~datum define/override) id+other/over ...))))
(define-syntax-class contracted-definition
  #:description "define/contract form"
  (pattern ((~datum define/contract) id/sig ctc body ...)))
(define-syntax-class non-keyword
  #:description "non-keyword form"
  (pattern (~not (~or (~datum #%module-begin)
                      (~datum #%datum)
                      (~datum quote)
                      (~datum define)
                      (~datum define-values)
                      (~datum define/contract)
                      (~datum #%app)
                      (~datum if)
                      (~datum lambda)
                      (~datum λ)
                      (~datum struct)
                      (~datum class)
                      (~datum instantiate)
                      (~datum new)
                      (~datum super-instantiate)
                      (~datum super-new)
                      (~datum send)
                      (~datum get-field)
                      (~datum cond)
                      (~datum when)
                      (~datum unless)
                      (~datum match)
                      (~datum match*)
                      (~datum match-define)
                      (~datum =>)
                      (~datum ==)
                      (~datum begin0)
                      (~datum let)
                      (~datum set!)
                      (~datum define-syntax-rule)
                      (~datum for)
                      (~datum for*)
                      (~datum for/fold)
                      (~datum for*/fold)
                      (~datum for/list)
                      (~datum for*/list)
                      (~datum for/vector)
                      (~datum for*/vector)
                      (~datum this)
                      (~datum init)
                      (~datum init-field)
                      (~datum inherit-field)
                      (~datum define/public)
                      (~datum define/override)
                      (~datum delay)))))

(define/contract (mutate-expr stx mutation-index counter)
  (syntax? mutation-index? counter? . -> . mutated?)

  (if (and make-mutants
           (<= counter mutation-index))
      (syntax-parse stx
        ;; begin: statement deletion
        [((~datum begin) e1 e2 ...+)
         (mdo [count-with (__ counter)]
              (def begin-stx (maybe-mutate stx
                                           (syntax/loc stx (begin e2 ...))
                                           mutation-index
                                           __))
              ;; applying both mutations works because of counter tracking!
              [in (mutate-expr* begin-stx
                                mutation-index
                                __)])]
        [((~datum begin0) e1 e2 e3 ...)
         (mdo [count-with (__ counter)]
              (def begin-stx (maybe-mutate stx
                                           (syntax/loc stx (begin0 e1 e3 ...))
                                           mutation-index
                                           __))
              ;; applying both mutations works because of counter tracking!
              [in (mutate-expr* begin-stx
                                mutation-index
                                __)])]

        ;; conditionals: negate condition, or mutate result exprs
        [((~datum cond) clause ...)
         (define clauses-stxs (syntax-e (syntax/loc stx
                                          (clause ...))))
         (mdo* (def clauses (mutate-cond-in-seq clauses-stxs
                                                mutation-index
                                                counter))
               [return (quasisyntax/loc stx
                         (cond #,@clauses))])]
        [((~datum if) cond-e then-e else-e)
         (define clause-stx (list (quasisyntax/loc stx
                                    [#,@(rest (syntax-e stx))])))
         (mdo* (def clause (mutate-cond-in-seq clause-stx
                                               mutation-index
                                               counter))
               [return (quasisyntax/loc stx
                         (if #,@(first clause)))])]

        ;; mutate function applications:
        ;; Move around arguments, or mutate argument expressions
        [(f:non-keyword arg ...)
         (define args-stxs (syntax-e (syntax/loc stx (arg ...))))
         (mdo [count-with (__ counter)]
              (def args/rotated (rotate-app-args args-stxs
                                                 mutation-index
                                                 __))
              (def f/mutated (mutate-expr (syntax/loc stx f)
                                          mutation-index
                                          __))
              (def args/mutated (mutate-in-seq args/rotated
                                               mutation-index
                                               __
                                               mutate-expr))
              [return (quasisyntax/loc stx
                        (#,f/mutated #,@args/mutated))])]

        ;; mutate arbitrary sexp
        [(e ...)
         (mutate-expr* stx mutation-index counter)]

        ;; anything else, just try mutate-datum
        [e
         (mutate-datum stx mutation-index counter)])

      (mutated stx counter)))

;; applies `mutate-expr` to all subexprs of `stx`
(define (mutate-expr* stx mutation-index counter)
  (mdo* (def parts (mutate-in-seq (syntax-e stx)
                                  mutation-index
                                  counter
                                  mutate-expr))
        [return (quasisyntax/loc stx
                  (#,@parts))]))

(define/contract (rotate-app-args args-stxs mutation-index counter)
  ((listof syntax?)
   mutation-index?
   counter?
   . -> .
   (mutated/c (listof syntax?)))

  ;; idea: in order to re-use mutate-in-seq
  ;; partition args-stxs into list of pairs of args
  ;; and use mutate-in-seq on that with a mutator that swaps the two args
  ;;
  ;; Use maybe-mutate
  (define-values (pairs remainder) (pair-off args-stxs))
  (mdo* (def pairs/swapped (mutate-in-seq pairs
                                          mutation-index
                                          counter
                                          mutate-arg-pair))
        [return (unpair-off pairs/swapped remainder)]))

(define (pair-off lst)
  (for/fold ([paired empty]
             [current-pair empty]
             #:result (values (reverse paired) current-pair))
            ([el (in-list lst)])
    (if (empty? current-pair)
        (values paired (list el))
        (values (cons (list (first current-pair) el)
                      paired)
                empty))))

(define (unpair-off pairs remainder)
  (append (flatten (map syntax-e pairs)) remainder))

(module+ test
  (check-programs-equal?
   #`(#,@(mutated-stx (rotate-app-args
                       (syntax-e #'(a (+ 1 2)))
                       0 0)))
   #'((+ 1 2) a))
  (check-programs-equal?
   #`(#,@(mutated-stx (rotate-app-args
                       (syntax-e #'(a (+ 1 2) b))
                       0 0)))
   #'((+ 1 2) a b))
  (check-programs-equal?
   #`(#,@(mutated-stx (rotate-app-args
                       (syntax-e #'(a (+ 1 2) b (foo 3)))
                       1 0)))
   #'(a (+ 1 2) (foo 3) b))
  (check-programs-equal?
   #`(#,@(mutated-stx (rotate-app-args
                       (syntax-e #'(a (+ 1 2) b (foo 3) (bar 3 4 5)))
                       2 0)))
   #'(a (+ 1 2) b (foo 3) (bar 3 4 5)))
  #| ... |#)




;; Note: distinction between mutate-program and mutate-expr
;; is necessary because mutate-program should descend only into
;; contracted top level forms, while mutate-expr can descend into
;; everything (mutate-program acts like its gatekeeper)
(define/contract (mutate-program stx mutation-index [counter 0])
  ((syntax? mutation-index?)
   (counter?)
   . ->* .
   mutated?)

  (if (and make-mutants
           (<= counter mutation-index))
      (syntax-parse stx
        ;; Mutate contracted definition bodies
        ;; (could be class, function, other value)
        [(def:contracted-definition e ...)
         (define body-stxs (syntax-e (syntax/loc stx
                                       (def.body ...))))
         (mdo [count-with (__ counter)]
              (def body-forms (mutate-in-seq body-stxs
                                             mutation-index
                                             __
                                             mutate-expr))
              (def program-rest (mutate-program (syntax/loc stx (e ...))
                                                mutation-index
                                                __))
              [return (quasisyntax/loc stx
                        ((define/contract def.id/sig def.ctc
                           #,@body-forms)
                         #,@program-rest))])]

        ;; Ignore anything else
        [(other-e e ...)
         (mdo* (def rest-stxs (mutate-program (syntax/loc stx (e ...))
                                              mutation-index
                                              counter))
               [return (quasisyntax/loc stx
                         (other-e #,@rest-stxs))])]
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

(define/contract (mutate-in-seq stxs mutation-index counter
                                mutator)
  (parametric->/c
   [A]
   ((listof (and/c (or/c syntax?
                         (listof syntax?))
                   A))
    mutation-index?
    counter?
    (A mutation-index? counter? . -> . mutated?)
    . -> .
    mutated?))

  (for/fold ([mutated-so-far (mutated '() counter)]
             #:result (mmap reverse mutated-so-far))
            ([stx (in-list stxs)])
    (mdo [count-with (__ #f)]
         (def stxs-so-far mutated-so-far)
         (def element (mutator stx
                               mutation-index
                               __))
         [return (cons element stxs-so-far)])))

(module+ test
  (define-check/loc (check-mutation/in-seq orig-seq
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

  (check-mutation/in-seq
   (list #'#t
         #''a
         #'1
         #'(+ 1 2))
   mutate-in-seq
   mutate-datum
   (curry map syntax->datum)
   ;; only elements 0 and 2 are mutatable datums
   `([0 (,#'(not #t)
         ,#''a
         ,#'1
         ,#'(+ 1 2))]
     [1 (,#'#t
         ,#''a
         ,#'0
         ,#'(+ 1 2))]
     ;; other elements aren't touched, regardless of mutation-index
     [2 (,#'#t
         ,#''a
         ,#'1
         ,#'(+ 1 2))]
     [3 (,#'#t
         ,#''a
         ,#'1
         ,#'(+ 1 2))]
     [4 (,#'#t
         ,#''a
         ,#'1
         ,#'(+ 1 2))]
     #| ... |#))

  (check-mutation/in-seq
   (list #'#t
         #''a
         #'(test? 2))
   mutate-in-seq
   mutate-condition
   (curry map syntax->datum)
   `([0 (,#'(not #t)
         ,#''a
         ,#'(test? 2))]
     [1 (,#'#t
         ,#'(not 'a)
         ,#'(test? 2))]
     [2 (,#'#t
         ,#''a
         ,#'(not (test? 2)))]
     ;; mutation index exceeded size of list, no more will be mutated
     [3 (,#'#t
         ,#''a
         ,#'(test? 2))]
     #| ... |#)))



(define/contract (mutate-in-seq* stxs mutation-index counter
                                 mutator)
  ((listof (listof syntax?))
   mutation-index?
   counter?
   (syntax? mutation-index? counter? . -> . mutated?)
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
   `([0 ((,#'(not #t) ,#'#f)
         (,#''a ,#'1)
         (,#'(test? 2)))]
     [1 ((,#'#t ,#'(not #f))
         (,#''a ,#'1)
         (,#'(test? 2)))]
     [2 ((,#'#t ,#'#f)
         (,#''a ,#'0)
         (,#'(test? 2)))]
     [3 ((,#'#t ,#'#f)
         (,#''a ,#'1)
         ;; mutate-datum won't change applications...
         ;; [[but%20mutate-expr%20will!]]
         (,#'(test? 2)))]
     ;; mutation index exceeded size of list, no more will be mutated
     [4 ((,#'#t ,#'#f)
         (,#''a ,#'1)
         (,#'(test? 2)))]
     #| ... |#))

  (check-mutation/in-seq
   (list (list #'#t #'#f)
         (list #''a #'1)
         (list #'(test? 2)))
   mutate-in-seq*
   mutate-expr
   (curry map
          (curry map syntax->datum))
   `([3 ((,#'#t ,#'#f)
         (,#''a ,#'1)
         ;; ... but mutate-expr will!
         (,#'(test? (add1 2))))])))

(define cond-clause? (syntax-parser
                       [[condition:expr b:expr r:expr ...]
                        #t]
                       [_ #f]))

(define/contract (mutate-cond-in-seq clauses mutation-index counter)
  ((listof cond-clause?) mutation-index? counter? . -> . mutated?)

  (define unwrapped-clauses (map syntax-e clauses))
  (define condition-stxs (map first unwrapped-clauses))
  (define/contract body-stxs* ;; each condition has a list of body-exprs
    (listof (listof syntax?))
    (map rest unwrapped-clauses))
  (mdo [count-with (__ counter)]
       (def conditions (mutate-in-seq condition-stxs
                                      mutation-index
                                      __
                                      mutate-condition))
       (def bodies* (mutate-in-seq* body-stxs*
                                    mutation-index
                                    __
                                    mutate-expr))
       [return (map cons conditions bodies*)]))

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
         ,#'[else (bar #f (+ x 2))])]
     [7 (,#'[#t 1 2 3]
         ,#'[(bool? 5) (foo 7)]
         ,#'[else (bar (+ 2 x) #f)])]
     [8 (,#'[#t 1 2 3]
         ,#'[(bool? 5) (foo 7)]
         ,#'[else (bar (- x 2) #f)])]
     [9 (,#'[#t 1 2 3]
         ,#'[(bool? 5) (foo 7)]
         ,#'[else (bar (+ x (add1 2)) #f)])]
     [10 (,#'[#t 1 2 3]
          ,#'[(bool? 5) (foo 7)]
          ,#'[else (bar (+ x 2) (not #f))])]
     ;; no more left
     [11 (,#'[#t 1 2 3]
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
  (define-check/loc (check-mutation/sequence orig-program expects)
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
   `([0 ,#'{(define/contract a positive? 0)
            (define/contract b positive? 2)}]
     [1 ,#'{(define/contract a positive? 1)
            (define/contract b positive? (add1 2))}]))

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
   `([0 ,#'{(define/contract a any/c (+ 2 1))}]
     [1 ,#'{(define/contract a any/c (- 1 2))}]
     [2 ,#'{(define/contract a any/c (+ 0 2))}]
     [3 ,#'{(define/contract a any/c (+ 1 (add1 2)))}]))
  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (< x 2))
      (define/contract b positive? 2)}
   `([0 ,#'{(define/contract (f x)
              any/c
              (< 2 x))
            (define/contract b positive? 2)}]
     [1 ,#'{(define/contract (f x)
              any/c
              (<= x 2))
            (define/contract b positive? 2)}]))

  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (<= x 2))
      (define/contract b positive? 2)}
   `([0 ,#'{(define/contract (f x)
              any/c
              (<= 2 x))
            (define/contract b positive? 2)}]
     [1 ,#'{(define/contract (f x)
              any/c
              (< x 2))
            (define/contract b positive? 2)}]))
  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (> x 2))
      (define/contract b positive? 2)}
   `([0 ,#'{(define/contract (f x)
              any/c
              (> 2 x))
            (define/contract b positive? 2)}]
     [1 ,#'{(define/contract (f x)
              any/c
              (>= x 2))
            (define/contract b positive? 2)}]))

  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (>= x 2))
      (define/contract b positive? 2)}
   `([0 ,#'{(define/contract (f x)
              any/c
              (>= 2 x))
            (define/contract b positive? 2)}]
     [1 ,#'{(define/contract (f x)
              any/c
              (> x 2))
            (define/contract b positive? 2)}]))

  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (= x 2))
      (define/contract b positive? 2)}
   `([0 ,#'{(define/contract (f x)
              any/c
              (= 2 x))
            (define/contract b positive? 2)}]
     [1 ,#'{(define/contract (f x)
              any/c
              (=/= x 2))
            (define/contract b positive? 2)}]))

  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (=/= x 2))
      (define/contract b positive? 2)}
   `([0 ,#'{(define/contract (f x)
              any/c
              (=/= 2 x))
            (define/contract b positive? 2)}]
     [1 ,#'{(define/contract (f x)
              any/c
              (= x 2))
            (define/contract b positive? 2)}]))

  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (+ x 2))
      (define/contract b positive? 2)}
   `([0 ,#'{(define/contract (f x)
              any/c
              (+ 2 x))
            (define/contract b positive? 2)}]
     [1 ,#'{(define/contract (f x)
              any/c
              (- x 2))
            (define/contract b positive? 2)}]))
  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (- x 2))
      (define/contract b positive? 2)}
   `([0 ,#'{(define/contract (f x)
              any/c
              (- 2 x))
            (define/contract b positive? 2)}]
     [1 ,#'{(define/contract (f x)
              any/c
              (+ x 2))
            (define/contract b positive? 2)}]))


  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (* x 2))
      (define/contract b positive? 2)}
   `([0 ,#'{(define/contract (f x)
              any/c
              (* 2 x))
            (define/contract b positive? 2)}]
     [1 ,#'{(define/contract (f x)
              any/c
              (/ x 2))
            (define/contract b positive? 2)}]))
  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (/ x 2))
      (define/contract b positive? 2)}
   `([0 ,#'{(define/contract (f x)
              any/c
              (/ 2 x))
            (define/contract b positive? 2)}]
     [1 ,#'{(define/contract (f x)
              any/c
              (* x 2))
            (define/contract b positive? 2)}]))

  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (and x #t))
      (define/contract b positive? 2)}
   `([0 ,#'{(define/contract (f x)
              any/c
              (and #t x))
            (define/contract b positive? 2)}]
     [1 ,#'{(define/contract (f x)
              any/c
              (or x #t))
            (define/contract b positive? 2)}]))
  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (or x #t))
      (define/contract b positive? 2)}
   `([0 ,#'{(define/contract (f x)
              any/c
              (or #t x))
            (define/contract b positive? 2)}]
     [1 ,#'{(define/contract (f x)
              any/c
              (and x #t))
            (define/contract b positive? 2)}]))

  ;; Test choices of index
  (check-mutation/sequence
   #'{(define/contract (f x)
        any/c
        (or x #t))
      (define/contract b positive? 2)}
   ;; Fixed this! ⇓
   `(#;[1 ,#'{(define/contract (f x)
                any/c
                (or x #t)) ;; tries to mutate `x` but it's a no-op
              (define/contract b positive? 2)}]
     [0 ,#'{(define/contract (f x)
              any/c
              (or #t x))
            (define/contract b positive? 2)}]
     [1 ,#'{(define/contract (f x)
              any/c
              (and x #t))
            (define/contract b positive? 2)}]
     [2 ,#'{(define/contract (f x)
              any/c
              (or x (not #t)))
            (define/contract b positive? 2)}]
     [3 ,#'{(define/contract (f x)
              any/c
              (or x #t))
            (define/contract b positive? (add1 2))}]))

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
  (check-mutation
   3
   #'{(define/contract (f x)
        any/c
        (or x #t))
      (define/contract b positive? (begin0 1 2))}
   #'{(define/contract (f x)
        any/c
        (or x #t))
      (define/contract b positive? (begin0 1))})

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
   `([3 ,#'{(define/contract (f x)
              any/c
              (or x #t))
            (define/contract b positive? (begin 2))
            (define/contract (g x)
              any/c
              (if x 1 2))}]
     [4 ,#'{(define/contract (f x)
              any/c
              (or x #t))
            (define/contract b positive? (begin 0 2))
            (define/contract (g x)
              any/c
              (if x 1 2))}]
     [5 ,#'{(define/contract (f x)
              any/c
              (or x #t))
            (define/contract b positive? (begin 1 (add1 2)))
            (define/contract (g x)
              any/c
              (if x 1 2))}]
     [6 ,#'{(define/contract (f x)
              any/c
              (or x #t))
            (define/contract b positive? (begin 1 2))
            (define/contract (g x)
              any/c
              (if (not x) 1 2))}]
     [7 ,#'{(define/contract (f x)
              any/c
              (or x #t))
            (define/contract b positive? (begin 1 2))
            (define/contract (g x)
              any/c
              (if x 0 2))}]
     [8 ,#'{(define/contract (f x)
              any/c
              (or x #t))
            (define/contract b positive? (begin 1 2))
            (define/contract (g x)
              any/c
              (if x 1 (add1 2)))}]))

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
  ;; lltodo: class tests here

  ;; Test that condition expressions are only ever considered for
  ;; mutation once: to negate them
  (check-mutation/sequence
   #'{(displayln "B")
      (define/contract c any/c 1)
      (define/contract d any/c (if #t 1 (error (quote wrong))))
      (displayln (quasiquote (c (unquote c))))
      (displayln (quasiquote (d (unquote d))))}
   `([1 ,#'{(displayln "B")
            (define/contract c any/c 1)
            (define/contract d any/c (if (not #t) 1 (error (quote wrong))))
            (displayln (quasiquote (c (unquote c))))
            (displayln (quasiquote (d (unquote d))))}]
     [2 ,#'{(displayln "B")
            (define/contract c any/c 1)
            (define/contract d any/c (if #t 0 (error (quote wrong))))
            (displayln (quasiquote (c (unquote c))))
            (displayln (quasiquote (d (unquote d))))}]))
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
   `([1 ,#'{(displayln "B")
            (define/contract c any/c 1)
            (define/contract d any/c
              (cond [(not #t) 1]
                    [(foobar (+ 1 2)) -1 5]
                    [else (error (quote wrong))]))
            (displayln (quasiquote (c (unquote c))))
            (displayln (quasiquote (d (unquote d))))}]
     [2 ,#'{(displayln "B")
            (define/contract c any/c 1)
            (define/contract d any/c
              (cond [#t 1]
                    [(not (foobar (+ 1 2))) -1 5]
                    [else (error (quote wrong))]))
            (displayln (quasiquote (c (unquote c))))
            (displayln (quasiquote (d (unquote d))))}]
     ;; then the bodies in sequence
     [3 ,#'{(displayln "B")
            (define/contract c any/c 1)
            (define/contract d any/c
              (cond [#t 0]
                    [(foobar (+ 1 2)) -1 5]
                    [else (error (quote wrong))]))
            (displayln (quasiquote (c (unquote c))))
            (displayln (quasiquote (d (unquote d))))}]
     [4 ,#'{(displayln "B")
            (define/contract c any/c 1)
            (define/contract d any/c
              (cond [#t 1]
                    [(foobar (+ 1 2)) 1 5]
                    [else (error (quote wrong))]))
            (displayln (quasiquote (c (unquote c))))
            (displayln (quasiquote (d (unquote d))))}]
     [5 ,#'{(displayln "B")
            (define/contract c any/c 1)
            (define/contract d any/c
              (cond [#t 1]
                    [(foobar (+ 1 2)) -1 -1]
                    [else (error (quote wrong))]))
            (displayln (quasiquote (c (unquote c))))
            (displayln (quasiquote (d (unquote d))))}]))


  ;; Test handling of nested exprs
  (check-mutation/sequence
   #'{(define/contract a any/c (+ (+ 1 2)
                                  (- 3 4)))}
   `(;; flip outer args
     [0 ,#'{(define/contract a any/c (+ (- 3 4)
                                        (+ 1 2)))}]
     ;; negate outer +
     [1 ,#'{(define/contract a any/c (- (+ 1 2)
                                        (- 3 4)))}]
     ;; flip inner args 1
     [2 ,#'{(define/contract a any/c (+ (+ 2 1)
                                        (- 3 4)))}]
     ;; negate inner +
     [3 ,#'{(define/contract a any/c (+ (- 1 2)
                                        (- 3 4)))}]
     ;; mutate inner + args
     [4 ,#'{(define/contract a any/c (+ (+ 0 2)
                                        (- 3 4)))}]
     [5 ,#'{(define/contract a any/c (+ (+ 1 (add1 2))
                                        (- 3 4)))}]
     ;; flip inner args 2
     [6 ,#'{(define/contract a any/c (+ (+ 1 2)
                                        (- 4 3)))}]
     ;; negate inner -
     [7 ,#'{(define/contract a any/c (+ (+ 1 2)
                                        (+ 3 4)))}]
     ;; mutate inner - args
     [8 ,#'{(define/contract a any/c (+ (+ 1 2)
                                        (- (add1 3) 4)))}]
     [9 ,#'{(define/contract a any/c (+ (+ 1 2)
                                        (- 3 (add1 4))))}]))

  ;; Test the mutator's ability to handle higher order expressions
  ;; 1. The function being applied is an expression
  ;; 2. Primitives that should be mutated (+, -) appear in expression ctx
  ;;    rather than application ctx
  (check-mutation/sequence
   #'{(define/contract a any/c ((if #t + -) 1 2))}
   `([0 ,#'{(define/contract a any/c ((if #t + -) 2 1))}]
     [1 ,#'{(define/contract a any/c ((if (not #t) + -) 1 2))}]
     [2 ,#'{(define/contract a any/c ((if #t - -) 1 2))}]
     [3 ,#'{(define/contract a any/c ((if #t + +) 1 2))}]
     [4 ,#'{(define/contract a any/c ((if #t + -) 0 2))}]
     [5 ,#'{(define/contract a any/c ((if #t + -) 1 (add1 2)))}]))

  (check-mutation
   2
   #'{(define (foo x) x)
      (define/contract a any/c (+ (foo 1) 2))}
   #'{(define (foo x) x)
      (define/contract a any/c (+ (foo 0) 2))}))

;; lltodo: wiw:
;; lltodo: also, still need to make the mutate functions insert the
;; invocation to enter the "bug" region of the program.

;; lltodo: change some class mutations
;; 1. [] Add swapping field initializers
;; 2. [✓] Don't mutate define/override & augment
;; 3. [✓] Add swapping order of arguments to funcalls
;; 4. [] Move super-new's around in class exprs
;; 5. [] handle begin0
