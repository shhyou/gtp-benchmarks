#lang racket/base

(require (for-syntax syntax/parse
                     racket/list
                     racket/base
                     (only-in racket/function
                              curryr))
         racket/contract)

(provide configurable-ctc)

(define-for-syntax precision-configs '(none types max))
;; Must be a member of ^; modify to change all configurable-ctc precision levels
(define-for-syntax current-precision-config 'max)

;; Usage:
;; (configurable-ctc [<unquoted-precision-config> contract?] ...)
;;
;; No need to specify the 'none case: any unspecified cases will
;; become any/c when that configuration is used.
;;
;; Example:
;; (define/contract (f x)
;;   (configurable-ctc [max (-> number? string?)]
;;                     [types procedure?]))
(define-syntax (configurable-ctc stx)
  (syntax-parse stx
    [(_ [level ctc] ...)
     (let* ([levels (syntax->datum #'(level ...))]
            [ctcs (flatten (syntax->list #'(ctc ...)))]
            [current-level-index (index-of levels
                                           current-precision-config)]
            [current-ctc-stx (if (number? current-level-index)
                                 (list-ref ctcs current-level-index)
                                 #'any/c)])
       (begin
         (unless (andmap (curryr member precision-configs) levels)
           (error 'configurable-ctc
                  "Unknown precision config provided at ~a" stx))
         (with-syntax ([ctc-for-current-level current-ctc-stx])
           (syntax/loc current-ctc-stx
             ctc-for-current-level))))]))



(define-syntax (c-> stx)
  (syntax-parse stx
    #:datum-literals (any values)
    [(_ (~or (~seq mandatory-kw:keyword mandatory-kw-dom)
              mandatory-dom:expr)
        ... rng)
     #'(-> (wrap/c mandatory-dom)
           ...
           (~@ mandatory-kw (wrap/c mandatory-kw-dom))
           ...
           (wrap/c rng))]))

(define-syntax (c->* stx)
  (syntax-parse stx
    #:datum-literals (any values)
    [(_ ((~or (~seq mandatory-kw:keyword mandatory-kw-dom)
              mandatory-dom:expr)
         ...)
        ((~or (~seq optional-kw:keyword optional-kw-dom)
              optional-dom:expr)
         ...)
        (~optional (~seq (~datum #:pre) pre-cond))
        rng
        (~optional (~seq (~datum #:post) post-cond)))
     #'(->* ((wrap/c mandatory-dom)
             ...
             (~@ mandatory-kw (wrap/c mandatory-kw-dom))
             ...)
            ((wrap/c optional-dom)
             ...
             (~@ optional-kw (wrap/c optional-kw-dom))
             ...)
            (~? (~@ #:pre pre-cond))
            (wrap/c rng)
            (~? (~@ #:post post-cond)))]))

(define-syntax (c->i stx)
  (define-syntax-class dependent-rng
    #:description "dependent-range"
    (pattern [result:id (~optional (result-arg:id ...))
                        result-ctc:expr]))
  (syntax-parse stx
    #:datum-literals (any values)
    [(_ ([param:id (~optional (param-arg:id ...)) param-ctc:expr] ...)
        (~optional (~seq (~datum #:pre)
                         (pre-arg:id ...)
                         pre-cond:expr))
        (~or rng:dependent-rng
             (values values-rng:dependent-rng ...))
        (~optional (~seq (~datum #:post)
                         (post-arg:id ...)
                         post-cond:expr)))
     #'(->i ([param (~? (param-arg ...)) (wrap/c param-ctc)] ...)
            (~? (~@ #:pre (pre-arg ...) pre-cond))
            (~? [rng.result (~? ((~@ rng.result-arg ...)))
                            (wrap/c rng.result-ctc)])
            (~? (values [values-rng.result (~? ((~@ values-rng.result-arg ...)))
                                           (wrap/c values-rng.result-ctc)] ...))
            (~? (~@ #:post (post-arg ...) post-cond)))]))

;; lltodo: note that the above doesn't work when `values` range
;; contracts don't refer to arguments. (???)

(define-syntax (wrap/c stx)
  (syntax-parse stx
    #:datum-literals (any values)
    [(_ any) #'any]
    [(_ (values c ...)) #'(values (wrap/c c ...))]
    [(_ s) #'(wrap s)]))

;; If it's not a flat contract, then
;; applying higher order ctcs to a value will produce a contract-wrapped value.
;; We may not want to mess with these, because the only thing that checks actual
;; values is flat contracts.
;; Except these contracts do check if they're given a value of the right shape
;; immediately.

;; So we should replace higher order contracts with a function that
;; first checks if the given value is tainted, and if so pushes the
;; contract down into the taint wrapper, and if not is just the normal
;; contract.
(define (wrap ctc)
  (if (flat-contract? ctc)
      (λ (x) (ctc (tainted-value* x)))
      (curry tainted-map ctc)))
