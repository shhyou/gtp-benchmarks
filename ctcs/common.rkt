#lang racket/base

(require racket/contract
         (for-syntax syntax/parse
                     racket/base)
         racket/class)

(provide (all-defined-out))

(define ((memberof/c l) x)
  (member x l))

(define-syntax (class/c* stx)
  (syntax-parse stx
    #:datum-literals (field/all all inherit+super)
    [(_ (~alt (~optional (field/all f-spec ...))
              (~optional (all all-spec ...))
              (~optional (inherit+super i+s-spec ...))) ...
        other-specs ...)
     #'(class/c (~? (field f-spec ...))
                (~? (inherit-field f-spec ...))
                ;; all
                (~? (inherit all-spec ...))
                (~? (super all-spec ...))
                (~? (override all-spec ...))
                ;; i+s
                (~? (inherit i+s-spec ...))
                (~? (super i+s-spec ...))
                ;; rest
                other-specs ...)]))
