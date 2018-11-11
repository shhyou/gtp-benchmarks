#lang racket

(provide define-check/loc
         )

(require rackunit
         (for-syntax syntax/parse))

(define (basename path)
  (define-values (_1 name _2) (split-path path))
  name)

(define-syntax (define-check/loc stx)
  (syntax-parse stx
    [(_ (check-id arg-id ...)
        (~optional (~seq (~and #:ignore-parameters
                               ignore-kw)))
        body ...)
     #`(begin
         (define-syntax (check-id s)
           (syntax-parse s
             [(_ arg-id ...)
              #`(actual-check arg-id ... (quote-syntax #,s))]))
         (define-check (actual-check arg-id ... check-src-stx)
           (with-check-info (['name 'check-id]
                             ['location
                              (check-info-value
                               (make-check-location
                                (list
                                 (basename (syntax-source check-src-stx))
                                 (syntax-line check-src-stx)
                                 (syntax-column check-src-stx)
                                 #f
                                 #f)))]
                             ['params
                              #,(if (attribute ignore-kw)
                                    #'"omitted"
                                    #'(list arg-id ...))])
             (let ([res (begin body ...)])
               (unless (equal? res #t)
                 (fail-check res))))))]))
