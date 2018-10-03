#lang racket/base

(require racket/contract
         "../../../ctcs/precision-config.rkt")

(provide
  index?
  assert
)

(define/contract (assert v p)
  (configurable-ctc
   [max (parametric->/c [A] (A (A . -> . boolean?) . -> . A))]
   [types (any/c (any/c . -> . boolean?) . -> . any/c)])

  (if (p v)
    v
    (raise-user-error 'assert)))

(define/contract (index? v)
  (configurable-ctc
   [max (any/c . -> . boolean?)]
   [types (any/c . -> . boolean?)])

  (and (exact-nonnegative-integer? v) (< v 9999999)))
