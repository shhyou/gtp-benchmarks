#lang racket/base

(require racket/contract)

(provide (all-defined-out))

(define ((memberof/c l) x)
  (member x l))
