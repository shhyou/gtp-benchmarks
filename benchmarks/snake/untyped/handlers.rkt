#lang racket
;; Movie handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(require "data.rkt"
         "motion.rkt"
         "collide.rkt"
         "../../../ctcs/precision-config.rkt"
         "../../../ctcs/common.rkt")

;; handle-key : World String -> World
(define/contract (handle-key w ke)
  (configurable-ctc
   [max (->i ([w world-type?]
              [ke string?])
             [result (w ke)
                     (world=?/c
                      (let ([keymap (hash "w" "up"
                                          "s" "down"
                                          "a" "left"
                                          "d" "right")])
                        (match* (ke w)
                          [((? (curry hash-has-key? keymap))
                            (world (snake _ segs)
                                   food))
                           (world (snake (hash-ref keymap ke)
                                         segs)
                                  food)]
                          [(_ w) w])))])]
   [types (world-type? string? . -> . world-type?)])

  (cond [(equal? ke "w") (world-change-dir w "up")]
        [(equal? ke "s") (world-change-dir w "down")]
        [(equal? ke "a") (world-change-dir w "left")]
        [(equal? ke "d") (world-change-dir w "right")]
        [else w]))

;; game-over? : World -> Boolean
(define/contract (game-over? w)
  (configurable-ctc
   [max (->i ([w world-type?])
             [result (w)
                     (match w
                       [(world s _)
                        (or (snake-wall-collide? s)
                            (snake-self-collide? s))])])]
   [types (world-type? . -> . boolean?)])

  (or (snake-wall-collide? (world-snake w))
      (snake-self-collide? (world-snake w))))

(provide
 handle-key 
 game-over?)
