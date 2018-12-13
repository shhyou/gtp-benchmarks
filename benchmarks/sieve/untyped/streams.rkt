#lang racket/base

(require racket/contract
         "../../../ctcs/precision-config.rkt")

;; Simple streams library.
;; For building and using infinite lists.

(provide (struct-out stream)
         make-stream
         stream-unfold
         stream-get
         stream-take)

;; A stream is a cons of a value and a thunk that computes the next value when applied
(struct stream (first rest))

;;--------------------------------------------------------------------------------------------------

(define/contract (make-stream hd thunk)
  (configurable-ctc
   [types (-> any/c (-> stream?) stream?)]
   [max (-> any/c (-> stream?) stream?)])
  (stream hd thunk))

;; `stream-unfold st` Destruct a stream `st` into its first value and the new stream produced by de-thunking the tail
(define/contract (stream-unfold st)
  (configurable-ctc
   [types (-> stream? (values any/c stream?))]
   [max (-> stream? (values any/c stream?))])
  (values (stream-first st) ((stream-rest st))))

;; `stream-get st i` Get the `i`-th element from the stream `st`
(define/contract (stream-get st i)
  (configurable-ctc
   [types (-> stream? exact-nonnegative-integer? any/c)]
   [max (-> stream? exact-nonnegative-integer? any/c)])
  (define-values (hd tl) (stream-unfold st))
  (cond [(= i 0) hd]
        [else    (stream-get tl (sub1 i))]))

;; `stream-take st n` Collect the first `n` elements of the stream `st`.
(define/contract (stream-take st n)
  (configurable-ctc
   [types (-> stream? exact-nonnegative-integer? (listof any/c))]
   [max (-> stream? exact-nonnegative-integer? (listof any/c))])
  (cond [(= n 0) '()]
        [else (define-values (hd tl) (stream-unfold st))
              (cons hd (stream-take tl (sub1 n)))]))
