#lang racket
;; Movie handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(require "data.rkt"
         "motion.rkt"
         "collide.rkt"
         racket/contract
         "../../../ctcs/precision-config.rkt")

;; handle-key : World String -> World
(define/contract (handle-key w ke)
  (configurable-ctc
   [types (-> world? string? world?)]
   [max (-> world? string? world?)])
  (cond [(equal? ke "w") (world-change-dir w "up")]
        [(equal? ke "s") (world-change-dir w "down")]
        [(equal? ke "a") (world-change-dir w "left")]
        [(equal? ke "d") (world-change-dir w "right")]
        [else w]))

;; game-over? : World -> Boolean
(define/contract (game-over? w)
  (configurable-ctc
   [types (-> world? boolean?)]
   [max (-> world? boolean?)])
  (or (snake-wall-collide? (world-snake w))
      (snake-self-collide? (world-snake w))))

(provide
 handle-key 
 game-over?)
