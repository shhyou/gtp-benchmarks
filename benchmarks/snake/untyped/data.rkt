#lang racket

(require racket/contract
         "../../../ctcs/precision-config.rkt")

(struct snake (dir segs))
(struct world (snake food))
(struct posn (x y))

(define dir/c (or/c "up" "down" "left" "right"))

(define/contract (posn=? p1 p2)
  (configurable-ctc
   [types (-> posn? posn? boolean?)]
   [max (-> posn? posn? boolean?)])
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))  

(provide [struct-out posn])

(provide
 posn=?
 [struct-out snake]
 [struct-out world]
 dir/c)
