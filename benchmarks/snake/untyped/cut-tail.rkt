#lang racket

(require "data.rkt"
         racket/contract
         "../../../ctcs/precision-config.rkt")
;; NeSegs is one of:
;; - (cons Posn empty)
;; - (cons Posn NeSegs)

;; cut-tail : NeSegs -> Segs
;; Cut off the tail.
(define/contract (cut-tail segs)
  (configurable-ctc
   [types (-> (non-empty-listof posn?) (listof posn?))]
   [max (-> (non-empty-listof posn?) (listof posn?))])
  (let ([r (cdr segs)])
    (cond [(empty? r) empty]
          [else (cons (car segs) (cut-tail r))])))

(provide
 cut-tail)
