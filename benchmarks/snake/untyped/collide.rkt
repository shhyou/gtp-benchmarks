#lang racket
(require "data.rkt"
         "const.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt"
         (only-in racket/function curry))


;; snake-wall-collide? : Snake -> Boolean
;; Is the snake colliding with any of the walls?
(define/contract (snake-wall-collide? snk)
  (configurable-ctc
   [max (->i ([snk snake?])
             [result (snk)
                     (head-collide? (car (snake-segs snk)))])]
   [types (snake? . -> . boolean?)])

  (head-collide? (car (snake-segs snk))))

;; head-collide? : Posn -> Boolean
(define/contract (head-collide? p)
  (configurable-ctc
   [max (->i ([p posn?])
             [result (p)
                     (not (and (<= 0 (posn-x p) BOARD-WIDTH)
                               (<= 0 (posn-y p) BOARD-HEIGHT)))])]
   [types (posn? . -> . boolean?)])

  (or (<= (posn-x p) 0)
      (>= (posn-x p) BOARD-WIDTH)
      (<= (posn-y p) 0)
      (>= (posn-y p) BOARD-HEIGHT)))

;; snake-self-collide? : Snake -> Boolean
(define/contract (snake-self-collide? snk)
  (configurable-ctc
   [max (->i ([snk snake?])
             [result (snk)
                     (let ([h (car (snake-segs snk))]
                           [segs (cdr (snake-segs snk))])
                       (if (empty? segs)
                           #f
                           (memf (curry posn=? h) segs)))])]
   [types (snake? . -> . boolean?)])

  (segs-self-collide? (car (snake-segs snk))
                      (cdr (snake-segs snk))))

;; segs-self-collide? : Posn Segs -> Boolean
(define/contract (segs-self-collide? h segs)
  (configurable-ctc
   [max (->i ([h posn?]
              [segs snake-segs?])
             [result (h segs)
                     (if (empty? segs)
                         #f
                         (memf (curry posn=? h) segs))])]
   [types (posn? snake-segs? . -> . boolean?)])

  (cond [(empty? segs) #f]
        [else (or (posn=? (car segs) h)
                  (segs-self-collide? h (cdr segs)))]))
(provide
 snake-wall-collide?
 snake-self-collide?)
