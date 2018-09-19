#lang racket
(define (TODO) (error "Not implemented yet."))

(provide
  left
  right
  up
  down
  grid-ref
  grid-height
  grid-width
  show-grid
  array-set!
  build-array
  array-coord?
  direction?
  or-#f/c
)

(require
  "../base/un-types.rkt"
  require-typed-check
  ;math/array ;; TODO it'd be nice to use this
 racket/contract
)
(require (only-in "cell.rkt"
  char->cell%
  void-cell%
  cell%?
))

;; =============================================================================

(define array-coord? (vector/c index? index?))
(define (arrayof val-ctc)
  (vectorof (vectorof val-ctc)))

;; TODO refine
(define/contract (array-set! g p v)
  (parametric->/c [A]
                  ((arrayof A) array-coord? A . -> . void?))
  (vector-set! (vector-ref g (vector-ref p 0)) (vector-ref p 1) v))

(define/contract (build-array p f)
  (parametric->/c [A]
                  (array-coord? (array-coord? . -> . A) . -> . (arrayof A)))

  (for/vector
    ([x (in-range (vector-ref p 0))])
   (for/vector
                ([y (in-range (vector-ref p 1))])
    (f (vector (assert x index?) (assert y index?))))))
  ;(build-array p f)))

;; a Grid is a math/array Mutable-Array of cell%
;; (mutability is required for dungeon generation)
(define grid? (arrayof cell%?))

;; parses a list of strings into a grid, based on the printed representation
;; of each cell
(define/contract (parse-grid los)
  ((listof string?) . -> . grid?)

  (for/vector
              ; #:shape (vector (length los)
              ;                (apply max (map string-length los)))
              ;#:fill (new void-cell%)
              ([s (in-list los)])
            (for/vector
               ([c (in-string s)])
     (new (char->cell% c)))))

(define/contract (show-grid g)
  (grid? . -> . string?)
  (with-output-to-string
    (lambda ()
      (for ([r (in-vector g)])
        (for ([c (in-vector r)])
          (display (send c show)))
        (newline)))))

(define/contract (grid-height g)
  (grid? . -> . index?)

  (vector-length g))

(define/contract (grid-width g)
  (grid? . -> . index?)

  (vector-length (vector-ref g 0)))

(define/contract (within-grid? g pos)
  (grid? array-coord? . -> . boolean?)

  (and (<= 0 (vector-ref pos 0) (sub1 (grid-height g)))
       (<= 0 (vector-ref pos 1) (sub1 (grid-width  g)))))

(define ((within-grid/c g) pos)
  (within-grid? g pos))

(define (or-#f/c ctc)
  (or/c ctc
        #f))

(define/contract (grid-ref g pos)
  ;; TODO should this more precise version be used?
  ;; Concerned that somewhere result is checked for #f and usefully responds
  (->i ([g grid?]
        [pos (g) (and/c array-coord?
                        (within-grid/c g))])
       [result cell%?])
  ;; Simpler version:
  ;; (grid? array-coord? . -> . (or-#f/c cell%?))


  (and (within-grid? g pos)
       (vector-ref (vector-ref g (vector-ref pos 0)) (vector-ref pos 1))))

(define direction? (->* (array-coord?) [index?]
                        array-coord?))

(define/contract (left pos [n 1])
  direction?

  (vector (vector-ref pos 0)
          (max (- (vector-ref pos 1) n) 0)))

(define/contract (right pos [n 1])
  direction?

  (vector (vector-ref pos 0)
          (max (+ (vector-ref pos 1) n) 0)))

(define/contract (up pos [n 1])
  direction?

  (vector (max (- (vector-ref pos 0) n) 0)
          (vector-ref pos 1)))

(define/contract (down pos [n 1])
  direction?

  (vector (max (+ (vector-ref pos 0) n) 0)
          (vector-ref pos 1)))


;(module+ test
;  (require typed/rackunit)
;
;  (: parse-and-show (-> (Listof String) String))
;  (define (parse-and-show los) (show-grid (parse-grid los)))
;  (: render-grid (-> (Listof String) String))
;  (define (render-grid g) (string-join g "\n" #:after-last "\n"))
;
;  (define g1
;    '(" "))
;  (check-equal? (parse-and-show g1) " \n")
;
;  (define g2
;    '("**********"
;      "*        *"
;      "*        *"
;      "*        *"
;      "**********"))
;  (check-equal? (parse-and-show g2) (render-grid g2))
;
;  (define g3 ; padding should work
;    '("**********"
;      "*        *"
;      "*        *"
;      "*        *"
;      "*****"))
;  (define g3*
;    '("**********"
;      "*        *"
;      "*        *"
;      "*        *"
;      "*****....."))
;  (check-equal? (parse-and-show g3) (render-grid g3*))
;
;  (define g2* (parse-grid g2))
;  (check-true (within-grid? g2* '#(0 0)))
;  (check-true (within-grid? g2* '#(0 1)))
;  (check-true (within-grid? g2* '#(1 0)))
;  (check-true (within-grid? g2* '#(4 4)))
;  (check-false (within-grid? g2* '#(0 10)))
;  (check-false (within-grid? g2* '#(5 0)))
;  (check-false (within-grid? g2* '#(5 10)))
;  )
