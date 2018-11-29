#lang racket

(require "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")


(struct posn (x y))
(define ((posn=?/c p1) p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))
(define ((posn/c x/c y/c) x)
  (match x
    [(posn (? x/c) (? y/c)) #t]
    [_ #f]))

(define snake-segs? (listof posn?))
(define (snake-segs=?/c segs)
  (apply list/c (map posn=?/c segs)))

;; lltodo: I think this is wrong, should just be one
(define food? posn?)
(define food=?/c posn=?/c)

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

(define ((snake=?/c s1) s2)
  ;; lltodo: this doesn't work
  (match* (s1 s2)
    [((snake dir segs1)
      (snake dir segs2))
     ((snake-segs=?/c segs1) segs2)]
    [(_ _) #f]))

(struct world (snake food))

(define ((world/c snake/c food/c) x)
  (match x
    [(world (? snake/c) (? food/c)) #t]
    [_ #f]))

(define ((world=?/c w1) w2)
  (match* (w1 w2)
    [((world snake1 food1)
      (world snake2 food2))
     (and ((snake=?/c snake1) snake2)
          ((food=?/c food1) food2))]
    [(_ _) #f]))

(define world-type? (world/c snake-type? food?))

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
 posn/c
 snake-segs?
 snake-segs=?/c
 snake-dir?
 snake-type?
 snake/c
 snake=?/c
 posn=?/c
 world/c
 world-type?
 world=?/c
 food=?/c)
