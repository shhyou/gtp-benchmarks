#lang racket

(require "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(struct snake (dir segs))
(struct world (snake food))
(struct posn (x y))

(define snake-segs? (listof posn?))

(define ((posn=?/c p1) p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define/contract (posn=? p1 p2)
  (configurable-ctc
   [max (->i ([p1 posn?]
              [p2 posn?])
             [result (p1 p2)
                     (match* (p1 p2)
                       [((posn x y) (posn x y)) #t]
                       [(_ _) #f])])]
   [types (posn? posn? . -> . boolean?)])

  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(provide [struct-out posn])

(provide
 posn=?
 [struct-out snake]
 [struct-out world]
 posn?
 snake-segs?
 posn=?/c)
