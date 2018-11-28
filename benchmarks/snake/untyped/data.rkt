#lang racket

(require "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")


(struct posn (x y))
(define food? (listof posn?))

(define snake-segs? (listof posn?))
(define snake-dir? (or/c "up"
                         "down"
                         "left"
                         "right"))
(struct snake (dir segs))

(define (snake-type? s)
  (match s
    [(snake (? string?)
            (? snake-segs?)) #t]
    [_ #f]))

(define ((snake/c dir/c segs/c) s)
  (match s
    [(snake (? (and/c snake-dir?
                      dir/c))
            (? (and/c snake-segs?
                      segs/c)))
     #t]
    [_ #f]))

(struct world (snake food))

(define ((world/c snake/c food/c) x)
  (match x
    [(world (? snake/c) (? food/c)) #t]
    [_ #f]))

(define world-type? (world/c snake-type? food?))


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
 snake-dir?
 snake-type?
 snake/c
 posn=?/c
 world/c
 world-type?)
