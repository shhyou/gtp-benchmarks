#lang racket/base

(require racket/contract
         "../../../ctcs/precision-config.rkt")

;; Placeholder for images.
;; Pretends to render data.

(provide
 (struct-out image)
 empty-scene ;(number? number? . -> . image?)]
 place-image ;(image? number? number? image? . -> . image?)]
 circle      ;(number? string? string? . -> . image?)]
 mode/c
)

;; =============================================================================

(struct image (
 impl
))

(define mode/c ;; Didn't include symbols since other files only use strings
  (or/c "solid" "outline"))

(define/contract (empty-scene w h)
  (configurable-ctc
   [types (-> (and/c real? (not/c negative?)) (and/c real? (not/c negative?)) image?)]
   [max (-> (and/c real? (not/c negative?)) (and/c real? (not/c negative?)) image?)])
  (when (or (negative? w) (negative? h))
    (error 'image "Arguments must be non-negative real numbers"))
  (image (cons w h)))

(define/contract (place-image i1 w h i2)
  (configurable-ctc
   [types (-> image? real? real? image? image?)]
   [max (-> image? real? real? image? image?)])
  (image (list i1 w h i2)))

(define/contract (circle radius style color)
  (configurable-ctc
   [types (-> (and/c real? (not/c negative?)) mode/c string? image?)]
   [max (-> (and/c real? (not/c negative?)) mode/c string? image?)])
  (image (list radius style color)))
