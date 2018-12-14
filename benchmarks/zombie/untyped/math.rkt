#lang racket/base

(require racket/contract
         "../../../ctcs/precision-config.rkt")

(provide
 min  ;(number? number? . -> . number?)]
 max  ;(number? number? . -> . number?)]
 abs  ;(number? . -> . number?)]
 msqrt ;(number? . -> . number?)]
 sqr  ;(number? . -> . number?)]
)

;; =============================================================================

(define/contract (min x y)
  (configurable-ctc
   [types (-> real? real? real?)]
   [max (-> real? real? real?)])
  (if (<= x y) x y))

(define/contract (max x y)
  (configurable-ctc
   [types (-> real? real? real?)]
   [max (-> real? real? real?)])
  (if (>= x y) x y))

(define/contract (abs x)
  (configurable-ctc
   [types (-> real? number?)]
   [max (-> real? number?)])
  (if (>= x 0) x (- 0 x)))

(define/contract (sqr x)
  (configurable-ctc
   [types (-> number? number?)]
   [max (-> number? number?)])
  (* x x))

(define/contract (msqrt x)
  (configurable-ctc
   [types (-> number? number?)]
   [max (-> number? number?)])
  (sqrt x))
