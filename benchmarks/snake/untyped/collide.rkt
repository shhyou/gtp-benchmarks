#lang racket  
(require "data.rkt"
         "const.rkt"
         racket/contract
         "../../../ctcs/precision-config.rkt")

;; snake-wall-collide? : Snake -> Boolean
;; Is the snake colliding with any of the walls?
(define/contract (snake-wall-collide? snk)
  (configurable-ctc
   [types (-> snake? boolean?)]
   [max (-> snake? boolean?)])
  (head-collide? (car (snake-segs snk))))

;; head-collide? : Posn -> Boolean
(define (head-collide? p)(configurable-ctc
   [types (-> posn? boolean?)]
   [max (-> posn? boolean?)])
  (or (<= (posn-x p) 0)
      (>= (posn-x p) BOARD-WIDTH)
      (<= (posn-y p) 0)
      (>= (posn-y p) BOARD-HEIGHT)))

;; snake-self-collide? : Snake -> Boolean
(define (snake-self-collide? snk)
  (configurable-ctc
   [types (-> snake? boolean?)]
   [max (-> snake? boolean?)])
  (segs-self-collide? (car (snake-segs snk))
                      (cdr (snake-segs snk))))

;; segs-self-collide? : Posn Segs -> Boolean
(define (segs-self-collide? h segs)
  (configurable-ctc
   [types (-> posn? (listof posn?) boolean?)]
   [max (-> posn? (listof posn?) boolean?)])
  (cond [(empty? segs) #f]
        [else (or (posn=? (car segs) h)
                  (segs-self-collide? h (cdr segs)))]))
(provide
 snake-wall-collide?
 snake-self-collide?)
