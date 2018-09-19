#lang racket/base

(require racket/contract)

(provide
  index?
  assert
)

(define/contract (assert v p)
  (parametric->/c [A] (A (A . -> . boolean?) . -> . A))

  (if (p v)
    v
    (raise-user-error 'assert)))

(define/contract (index? v)
  (any/c . -> . boolean?)

  (and (exact-nonnegative-integer? v) (< v 9999999)))
