#lang racket
(require "data.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

(define/contract GRID-SIZE
  (configurable-ctc
   [max (=/c 30)]
   [types natural?])
  30)
(define/contract BOARD-HEIGHT
  (configurable-ctc
   [max (=/c 20)]
   [types natural?])
  20)
(define/contract BOARD-WIDTH
  (configurable-ctc
   [max (=/c 30)]
   [types natural?])
  30)
(define/contract (BOARD-HEIGHT-PIXELS)
  (configurable-ctc
   [max (-> (=/c (* GRID-SIZE BOARD-HEIGHT)))]
   [types (-> natural?)])
  (* GRID-SIZE BOARD-HEIGHT))
(define/contract (BOARD-WIDTH-PIXELS)
  (configurable-ctc
   [max (-> (=/c (* GRID-SIZE BOARD-WIDTH)))]
   [types (-> natural?)])
  (* GRID-SIZE BOARD-WIDTH))
(define/contract (SEGMENT-RADIUS)
  (configurable-ctc
   [max (-> (=/c (/ GRID-SIZE 2)))]
   [types (-> number?)])
  (/ GRID-SIZE 2))
(define/contract (FOOD-RADIUS)
  (configurable-ctc
   [max (-> (=/c (/ GRID-SIZE 2)))]
   [types (-> number?)])
  (SEGMENT-RADIUS))
(define/contract (WORLD)
  (configurable-ctc
   [max (-> (world/c "right"
                     (snake-segs=?/c (list (posn 5 3)))))]
   [types (-> world-type?)])
  (world (snake "right" (cons (posn 5 3) empty))
         (posn 8 12)))

(provide
 WORLD
 GRID-SIZE
 BOARD-HEIGHT-PIXELS
 BOARD-WIDTH
 BOARD-HEIGHT)

