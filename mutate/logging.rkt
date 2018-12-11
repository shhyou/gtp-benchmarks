#lang racket/base

(provide (all-defined-out))

(define-logger mutation)

(define mutated-syntax #f)

(define (set-mutated-syntax! old new)
  (set! mutated-syntax (list old new)))

(define (get-mutated-syntax)
  (values (car mutated-syntax) (cadr mutated-syntax)))
