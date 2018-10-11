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

(define (or-#f/c ctc)
  (or/c ctc
        #f))



(define stack? list?)

(define command%/c
  (class/c*
   (field/all
    [id symbol?]
    [descr string?]
    [exec ((listof
            (instanceof/c
             (recursive-contract command%/c)))
           stack?
           any/c
           . -> .
           (or-#f/c (cons/c (listof
                             (instanceof/c
                              (recursive-contract command%/c)))
                            stack?)))])))


(define command%? (instanceof/c command%/c))
(define env? (listof command%?))

(define-syntax (command%?-with-exec stx)
  (syntax-parse stx
    #:datum-literals (args type result)
    [(_ (~optional (type command%-c-type))
        (args env-name stack-name val-name)
        [result result-ctc]
        (~optional (~seq (~datum #:post) post-condition)))
     #`(and/c (instanceof/c
               #,(if (attribute command%-c-type)
                     #'command%-c-type
                     #'command%/c))
              (instanceof/c
               (class/c*
                (field/all
                 [exec (->i ([env-name env?]
                             [stack-name stack?]
                             [val-name any/c])
                            [result (env-name stack-name val-name)
                                    result-ctc]
                            #,@(if (attribute post-condition)
                                   #'(#:post
                                      (env-name stack-name val-name)
                                      post-condition)
                                   #'()))]))))]))

(define ((list-with-min-size/c n) S)
  (and (list? S)
       (>= (length S) n)))

(define ((equal?/c c/v) v)
  (equal? c/v v))
