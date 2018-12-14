#lang racket

(require "data.rkt"
         "const.rkt"
         "handlers.rkt"
         "motion.rkt"
         racket/contract
         "../../../ctcs/precision-config.rkt")

(define/contract (replay w0 hist)
  (configurable-ctc
   [types (-> world? (listof any/c) any)]
   [max (-> world? (listof any/c) any)])
  (reset!)
  (let loop ((w w0) (h hist))
    (if (empty? h)
        w
        (let ()
          (loop
           (match (car h)
             [`(on-key ,(? string? ke))
              (handle-key w ke)]
             [`(on-tick)
              (world->world w)]
             [`(stop-when)
              (game-over? w)
              w])
           (cdr h)))))
  (void))

(define DATA (with-input-from-file "../base/snake-hist.rktd" read))
(define LOOPS 200)

(define/contract (main hist)
  (configurable-ctc
   [types (-> any/c any)]
   [max (-> any/c any)])
  (define w0 (WORLD))
  (cond [(list? hist)
         (for ((_i (in-range LOOPS)))
           (replay w0 hist))]
        [else
         (error "bad input")]))

(time (main DATA))
