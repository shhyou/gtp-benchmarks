#lang racket/base

(provide
 w0
 world-on-mouse
 world-on-tick
)

(require racket/contract
         "../../../ctcs/precision-config.rkt")

(require (only-in "image.rkt"
  empty-scene
  place-image
  circle
))

(require (only-in "math.rkt"
  min
  max
  abs
  sqr
  msqrt
))

(require "data.rkt")

;; =============================================================================

(define WIDTH 400)
(define HEIGHT 400)
(define MT-SCENE (empty-scene WIDTH HEIGHT))
(define PLAYER-SPEED 4)
(define ZOMBIE-SPEED 2)
(define ZOMBIE-RADIUS 20)
(define PLAYER-RADIUS 20)
(define PLAYER-IMG (circle PLAYER-RADIUS "solid" "green"))

(define/contract (new-world player mouse zombies)
  (configurable-ctc
   [types (-> player/c posn/c horde/c world/c)]
   [max (-> player/c posn/c horde/c world/c)])
 (lambda (msg)
  (cond
   [(equal? msg 'on-mouse)
   (cons 'on-mouse
   (lambda (x y me)
    (new-world player
     (if (equal? me "leave") ((player-posn player)) (new-posn x y))
     zombies)))]
   [(equal? msg 'on-tick)
   (cons 'on-tick
    (lambda ()
    (new-world ((player-move-toward player) mouse)
     mouse
     ((horde-move-toward ((horde-eat-brains zombies))) ((player-posn player))))))]
   [(equal? msg 'to-draw)
   (cons 'to-draw
   (lambda ()
    ((player-draw-on player) ((horde-draw-on zombies) MT-SCENE))))]
   [(equal? msg 'stop-when)
   (cons 'stop-when
   (lambda ()
    ((horde-touching? zombies) ((player-posn player)))))]
   [else (error 'world "unknown message")])))

(define/contract (new-player p)
  (configurable-ctc
   [types (-> posn/c player/c)]
   [max (-> posn/c player/c)])
  (lambda (msg)
   (cond
    [(equal? msg 'posn) (cons 'posn (lambda () p))]
    [(equal? msg 'move-toward)
    (cons 'move-toward
     (lambda (q)
     (new-player ((posn-move-toward/speed p) q PLAYER-SPEED))))]
    [(equal? msg 'draw-on)
    (cons 'draw-on
     (lambda (scn)
     ((posn-draw-on/image p) PLAYER-IMG scn)))]
    [else (error 'player "unknown message")])))

(define/contract (new-horde undead dead)
  (configurable-ctc
   [types (-> zombies/c zombies/c horde/c)]
   [max (-> zombies/c zombies/c horde/c)])
 (lambda (msg)
  (cond
   [(equal? msg 'dead) (cons 'dead (lambda () dead))]
   [(equal? msg 'undead) (cons 'undead (lambda () undead))]
   [(equal? msg 'draw-on)
   (cons 'draw-on
     (lambda (scn)
    ((zombies-draw-on/color undead) "yellow" ((zombies-draw-on/color dead) "black" scn))))]
   [(equal? msg 'touching?)
   (cons 'touching?
    (lambda (p)
    (or ((zombies-touching? undead) p) ((zombies-touching? dead) p))))]
   [(equal? msg 'move-toward)
   (cons 'move-toward
    (lambda (p)
    (new-horde ((zombies-move-toward undead) p) dead)))]
   [(equal? msg 'eat-brains)
    (cons 'eat-brains
     (lambda () ((zombies-kill-all undead) dead)))]
   [else (error 'horde "unknown message")])))

(define/contract (new-cons-zombies z r)
  (configurable-ctc
   [types (-> zombie/c zombies/c zombies/c)]
   [max (-> zombie/c zombies/c zombies/c)])
 (lambda (msg)
  (cond
   [(equal? msg 'move-toward)
   (cons 'move-toward
   (lambda (p)
    (new-cons-zombies ((zombie-move-toward z) p) ((zombies-move-toward r) p))))]
   [(equal? msg 'draw-on/color)
   (cons 'draw-on/color
    (lambda (c s)
    ((zombie-draw-on/color z) c ((zombies-draw-on/color r) c s))))]
   [(equal? msg 'touching?)
   (cons 'touching?
    (lambda (p)
    (or ((zombie-touching? z) p) ((zombies-touching? r) p))))]
   [(equal? msg 'kill-all)
   (cons 'kill-all
   (lambda (dead)
    (cond
     [(or ((zombies-touching? r) ((zombie-posn z)))
         ((zombies-touching? dead) ((zombie-posn z))))
     ((zombies-kill-all r) (new-cons-zombies z dead))]
     [else (let ([res ((zombies-kill-all r) dead)])
         (new-horde
          (new-cons-zombies z ((horde-undead res)))
          ((horde-dead res))))])))]
   [else (error 'zombies "unknown message")])))

(define/contract (new-mt-zombies)
  (configurable-ctc
   [types (-> zombies/c)]
   [max (-> zombies/c)])
 (lambda (msg)
  (cond
   [(equal? msg 'move-toward) (cons 'move-toward (lambda (p) (new-mt-zombies)))]
   [(equal? msg 'draw-on/color) (cons 'draw-on/color (lambda (c s) s))]
   [(equal? msg 'touching?) (cons 'touching? (lambda (p) #f))]
   [(equal? msg 'kill-all)
   (cons 'kill-all
    (lambda (dead)
    (new-horde (new-mt-zombies) dead)))]
   [else (error 'zombies "unknown message")])))

(define/contract (new-zombie p)
  (configurable-ctc
   [types (-> posn/c zombie/c)]
   [max (-> posn/c zombie/c)])
 (lambda (msg)
  (cond
   [(equal? msg 'posn) (cons 'posn (lambda () p))]
   [(equal? msg 'draw-on/color)
   (cons 'draw-on/color
   (lambda (c s)
    ((posn-draw-on/image p)
     (circle ZOMBIE-RADIUS "solid" c)
     s)))]
   [(equal? msg 'touching?)
   (cons 'touching?
   (lambda (q)
    (<= ((posn-dist p) q) ZOMBIE-RADIUS)))]
   [(equal? msg 'move-toward)
   (cons 'move-toward
    (lambda (q)
    (new-zombie ((posn-move-toward/speed p) q ZOMBIE-SPEED))))]
   [else (error 'zombie "unknown message")])))

(define/contract (new-posn x y)
  (configurable-ctc
   [types (-> real? real? posn/c)]
   [max (-> real? real? posn/c)])
     (lambda (msg)
      (let ([this (new-posn x y)]) ; FIXME
       (cond
        [(equal? msg 'x) (cons 'x (lambda () x))]
        [(equal? msg 'y) (cons 'y (lambda () y))]
        [(equal? msg 'posn) (cons 'posn (lambda () this))]
        [(equal? msg 'move-toward/speed)
        (cons msg
        (lambda (p speed)
         (let* ([x2 (- ((posn-x p)) x)]
                [y2 (- ((posn-y p)) y)]
                [move-distance (min speed (max (abs x2) (abs y2)))])
          (cond
           [(< (abs x2) (abs y2))
           ((posn-move this)
            0
            (if (positive? y2) move-distance (- 0 move-distance)))]
           [else
           ((posn-move this)
            (if (positive? x2) move-distance (- 0 move-distance))
            0)]))))]
        [(equal? msg 'move)
        (cons 'move
         (lambda (x2 y2)
         (new-posn (+ x x2) (+ y y2))))]
        [(equal? msg 'draw-on/image)
         (cons 'draw-on/image
          (lambda (img scn)
           (place-image img x y scn)))]
        [(equal? msg 'dist)
         (cons 'dist
           (lambda (p)
            (msqrt (+ (sqr (- ((posn-y p)) y))
                   (sqr (- ((posn-x p)) x))))))]
        [else (error 'posn "unknown message")]))))

(define/contract w0
  (configurable-ctc
   [types world/c]
   [max world/c])
 (new-world
  (new-player (new-posn 0 0))
  (new-posn 0 0)
  (new-horde
   (new-cons-zombies
    (new-zombie (new-posn 100 300))
    (new-cons-zombies
     (new-zombie (new-posn 100 200))
     (new-mt-zombies)))
   (new-cons-zombies
    (new-zombie (new-posn 200 200))
    (new-mt-zombies)))))

