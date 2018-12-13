#lang racket  
(require "data.rkt"
         "const.rkt"
         "motion-help.rkt"
         racket/contract
         "../../../ctcs/precision-config.rkt")

(provide reset!)
(define r (make-pseudo-random-generator)) 
(define (reset!)
  (parameterize ((current-pseudo-random-generator r))
    (random-seed 1324)))

;; world->world : World -> World
(define/contract (world->world w)
  (configurable-ctc
   [types (-> world? world?)]
   [max (-> world? world?)])
  (cond [(eating? w) (snake-eat w)]
        [else
         (world (snake-slither (world-snake w))
                (world-food w))]))
;; eating? : World -> Boolean
;; Is the snake eating the food in the world.
(define/contract (eating? w)
  (configurable-ctc
   [types (-> world? boolean?)]
   [max (-> world? boolean?)])
  (posn=? (world-food w)
          (car (snake-segs (world-snake w)))))

;; snake-change-direction : Snake Direction -> Snake
;; Change the direction of the snake.
(define/contract (snake-change-direction snk dir)
  (configurable-ctc
   [types (-> snake? dir/c snake?)]
   [max (-> snake? dir/c snake?)])
  (snake dir
         (snake-segs snk)))
;; world-change-dir : World Direction -> World
;; Change direction of the world.
(define/contract (world-change-dir w dir)
  (configurable-ctc
   [types (-> world? dir/c world?)]
   [max (-> world? dir/c world?)])
  (world (snake-change-direction (world-snake w) dir)
         (world-food w)))
;; snake-eat : World -> World
;; Eat the food and generate a new one.
(define/contract (snake-eat w)
  (configurable-ctc
   [types (-> world? world?)]
   [max (-> world? world?)])
  (define i (add1 (random (sub1 BOARD-WIDTH) r)))
  (define j (add1 (random (sub1 BOARD-HEIGHT) r)))
  (world (snake-grow (world-snake w))
         (posn i j)
         
         #;(posn (- BOARD-WIDTH 1) (- BOARD-HEIGHT 1))))
(provide
 world-change-dir
 world->world)
