#lang racket

(provide mutate-program)

(require (for-syntax syntax/parse
                     racket/match))

(define-for-syntax make-mutants #t)
(define-for-syntax mutation-index 0)

(begin-for-syntax
  (unless (>= mutation-index 0)
    (error "mutation-index must be positive")))

(define-syntax (define-mutators stx)
  (syntax-parse stx
    #:datum-literals (-> <->)
    [(_ mutator-name
        (~or (orig -> mutated)
             (left <-> right)) ...
        other-mutations ...)
     #'(define-for-syntax (mutator-name s)
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

(define-syntax (mutate-body stx)
  (if make-mutants
      (syntax-parse stx
        ;; modify class methods
        [(_ ((~datum class)
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
        [(_ ((~datum class) e ...))
         #'(class e ...)]

        ;; modify function apps
        [(_ (fn arg ...))
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
        [(_ e)
         (mutate #'e)])
      (syntax-parse stx
        [(_ e)
         (syntax/loc stx e)])))


(define-syntax (mutate-program stx)
  (if make-mutants
      (syntax-parse stx
        ;; Mutate definition body (could be class, function, other value)
        [(_ counter
            ((define/contract id/sig ctc body ...) e ...))
         (let* ([counter-val (syntax->datum #'counter)]
                [mutate? (= counter-val mutation-index)])
           #`(begin (define/contract id/sig ctc
                      #,@(if mutate?
                             #'((mutate-body body) ...)
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
         #'(mutate-program 0 (e ...))])
      (syntax-parse stx
        [(_ e ...)
         (syntax/loc stx (begin e ...))])))


