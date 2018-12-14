#lang racket/base

(require racket/contract
         "../../../ctcs/precision-config.rkt")

(require (only-in "zombie.rkt"
  w0
  world-on-mouse
  world-on-tick
  world/c
))

;; =============================================================================

(define/contract (replay w0 hist)
  (configurable-ctc
   [types (-> world/c (listof any/c) any)]
   [max (-> world/c (listof any/c) any)])
 (let loop ((w  w0)
            (h  hist))
  (cond
   [(null? h)
    (void)]
   [(not (list? (car h)))
    (error "input error")]
   [else
    (define m (caar h))
    (define as (cdar h))
    (case m
     ;; no rendering
     [(to-draw stop-when)
       (loop w (cdr h))]
     [(on-mouse)
      (define r (apply (world-on-mouse w) (if (and (list? as) (real? (car as)) (real? (cadr as)) (string? (caddr as)) (null? (cdddr as)))
                                              as (error "cast error"))))
      (loop r (cdr h))]
     [(on-tick)
      (define r ((world-on-tick w)))
      (loop r (cdr h))])])))

(define DATA
  (with-input-from-file "../base/zombie-hist.rktd" read))

(define/contract (main hist)
  (configurable-ctc
   [types (-> any/c any)]
   [max (-> any/c any)])
  (cond
   [(list? hist)
    (for ((i (in-range 100)))
      (replay w0 hist))]
   [else
    (error "bad input")]))

(time (main DATA))
