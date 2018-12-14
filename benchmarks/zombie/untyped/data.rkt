#lang racket/base

(provide (all-defined-out))

(require racket/contract
         "../../../ctcs/precision-config.rkt"
         (for-syntax racket/sequence racket/base syntax/parse racket/syntax))

(require (only-in "image.rkt" image?))

;; Create an object type (-> Symbol (U (Pairof Symbol ?) ...))
;;  and getters for each member of the codomain union
(define-syntax make-fake-object-type*
  ;;bg; the untyped version ignores all types
  (syntax-parser #:datum-literals (max types)
   [(_ ty [f* t* [types ct*] [max cm*]] ...)
    #:with ty-method-id/c
    (format-id #'ty "~a-method-id/c" (string-downcase (symbol->string (syntax-e #'ty))))
    #:with ty/c (format-id #'ty "~a/c" (string-downcase (symbol->string (syntax-e #'ty))))
    #:with (id* ...) (for/list ([f (in-list (syntax->list #'(f* ...)))])
                       (format-id #'ty "~a-~a" (string-downcase (symbol->string (syntax-e #'ty))) f))
    #:with (f-sym* ...) (for/list ([f (in-list (syntax->list #'(f* ...)))]) (syntax-e f))
    (syntax-local-lift-module-end-declaration
     #'(begin
         (begin
           ;(: id* (-> ty t*))
           (define/contract (id* v)
             (configurable-ctc
              [types (-> ty/c ct*)]
              [max (-> ty/c cm*)])
             (let ([r (v 'f-sym*)])
               (if (eq? 'f-sym* (car r))
                   (cdr r)
                   (error 'id* "type error"))))) ...))
    #'(begin
        ;(define-type ty (-> Symbol (U (Pairof 'f-sym* t*) ...)))
        (define ty-method-id/c
          (flat-named-contract 'ty-method-id/c
                               (or/c 'f-sym* ...)))
        (define ty/c (-> ty-method-id/c (cons/c ty-method-id/c any/c))))]))

;(define posn/c
; (->i ([msg (one-of/c 'x 'y 'posn 'move-toward/speed 'draw-on/image 'dist)])
;  (res (msg)
;   (cond
;    [(equal? msg 'x) (-> number?)]
;    [(equal? msg 'y) (-> number?)]
;    [(equal? msg 'posn) (-> posn/c)]
;    [(equal? msg 'move-toward/speed) (posn/c number? . -> . posn/c)]
;    [(equal? msg 'draw-on/image) (image? image? . -> . image?)]
;    [(equal? msg 'dist) (posn/c . -> . number?)]
;    [else "error"]))))

(make-fake-object-type* Posn
  [x (-> Real)
     [types (-> real?)]
     [max (-> real?)]]
  [y (-> Real)
     [types (-> real?)]
     [max (-> real?)]]
  [posn (-> Posn)
        [types (-> posn/c)]
        [max (-> posn/c)]]
  [move-toward/speed (-> Posn Real Posn)
                     [types (-> posn/c real? posn/c)]
                     [max (-> posn/c real? posn/c)]]
  [move (-> Real Real Posn)
        [types (-> real? real? posn/c)]
        [max (-> real? real? posn/c)]]
  [draw-on/image (-> Image Image Image)
                 [types (-> image? image? image?)]
                 [max (-> image? image? image?)]]
  [dist (-> Posn Real)
        [types (-> posn/c real?)]
        [max (-> posn/c real?)]])

;(define player/c
; (->i ([msg (one-of/c 'posn 'move-toward 'draw-on)])
;  (res (msg)
;   (cond
;    [(equal? msg 'posn) (-> posn/c)]
;    [(equal? msg 'move-toward) (posn/c . -> . player/c)]
;    [(equal? msg 'draw-on) (image? . -> . image?)]
;    [else "error"]))))

(make-fake-object-type* Player
  (posn (-> Posn)
        [types (-> posn/c)]
        [max (-> posn/c)])
  (move-toward (-> Posn Player)
               [types (-> posn/c player/c)]
               [max (-> posn/c player/c)])
  (draw-on (-> Image Image)
           [types (-> image? image?)]
           [max (-> image? image?)]))

;(define zombie/c
; (->i ([msg (one-of/c 'posn 'draw-on/color 'touching? 'move-toward)])
;  (res (msg)
;   (cond
;    [(equal? msg 'posn) (-> posn/c)]
;    [(equal? msg 'draw-on/color) (string? image? . -> . image?)]
;    [(equal? msg 'touching?) (posn/c . -> . boolean?)]
;    [(equal? msg 'move-toward) (posn/c . -> . zombie/c)]
;    [else "error"]))))

(make-fake-object-type* Zombie
  (posn (-> Posn)
        [types (-> posn/c)]
        [max (-> posn/c)])
  (draw-on/color (-> String Image Image)
                 [types (-> string? image? image?)]
                 [max (-> string? image? image?)])
  (touching? (-> Posn Boolean)
             [types (-> posn/c boolean?)]
             [max (-> posn/c boolean?)])
  (move-toward (-> Posn Zombie)
               [types (-> posn/c zombie/c)]
               [max (-> posn/c zombie/c)]))

;(define horde/c
; (->i ([msg (one-of/c 'dead 'undead 'draw-on 'touching? 'move-toward 'eat-brains)])
;  (res (msg)
;   (cond
;    [(equal? msg 'dead) (-> zombies/c)]
;    [(equal? msg 'undead) (-> zombies/c)]
;    [(equal? msg 'draw-on) (image? . -> . image?)]
;    [(equal? msg 'touching?) (posn/c . -> . boolean?)]
;    [(equal? msg 'move-toward) (posn/c . -> . horde/c)]
;    [(equal? msg 'eat-brains) (-> horde/c)]
;    [else "error"]))))

(make-fake-object-type* Horde
  (dead (-> Zombies)
        [types (-> zombies/c)]
        [max (-> zombies/c)])
  (undead (-> Zombies)
          [types (-> zombies/c)]
          [max (-> zombies/c)])
  (draw-on (-> Image Image)
           [types (-> image? image?)]
           [max (-> image? image?)])
  (touching? (-> Posn Boolean)
             [types (-> posn/c boolean?)]
             [max (-> posn/c boolean?)])
  (move-toward (-> Posn Horde)
               [types (-> posn/c horde/c)]
               [max (-> posn/c horde/c)])
  (eat-brains (-> Horde)
              [types (-> horde/c)]
              [max (-> horde/c)]))

;(define zombies/c
; (->i ([msg (one-of/c 'move-toward 'draw-on/color 'touching? 'kill-all)])
;  (res (msg)
;   (cond
;    [(equal? msg 'move-toward) (posn/c . -> . zombies/c)]
;    [(equal? msg 'draw-on/color) (string? image? . -> . image?)]
;    [(equal? msg 'touching?) (posn/c . -> . boolean?)]
;    [(equal? msg 'kill-all) (zombies/c . -> . horde/c)]
;    [else "error"]))))

(make-fake-object-type* Zombies
  (move-toward (-> Posn Zombies)
               [types (-> posn/c zombies/c)]
               [max (-> posn/c zombies/c)])
  (draw-on/color (-> String Image Image)
                 [types (-> string? image? image?)]
                 [max (-> string? image? image?)])
  (touching? (-> Posn Boolean)
             [types (-> posn/c boolean?)]
             [max (-> posn/c boolean?)])
  (kill-all (-> Zombies Horde)
            [types (-> zombies/c horde/c)]
            [max (-> zombies/c horde/c)]))

;(define world/c
; (->i ([msg (one-of/c 'on-mouse 'on-tick 'to-draw 'stop-when)])
;  (res (msg)
;   (cond
;    [(equal? msg 'on-mouse) (number? number? string? . -> . world/c)]
;    [(equal? msg 'on-tick) (-> world/c)]
;    [(equal? msg 'to-draw) (-> image?)]
;    [(equal? msg 'stop-when) (-> boolean?)]
;    [else "error"]))))

(make-fake-object-type* World
  (on-mouse (-> Real Real String World)
            [types (-> real? real? string? world/c)]
            [max (-> real? real? string? world/c)])
  (on-tick (-> World)
           [types (-> world/c)]
           [max (-> world/c)])
  (to-draw (-> Image)
           [types (-> image?)]
           [max (-> image?)])
  (stop-when (-> Boolean)
             [types (-> boolean?)]
             [max (-> boolean?)]))
