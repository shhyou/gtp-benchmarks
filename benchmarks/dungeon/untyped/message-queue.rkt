#lang racket/base

(require racket/contract)

(provide
  enqueue-message!
  reset-message-queue!
)


;; list of strings (messages) which were produced since the previous
;; previous display, and need to be displayed now
(define/contract message-queue
  (listof string?)
  '())

(define/contract (enqueue-message! m)
  (string? . -> . void?)

  (set! message-queue (cons m message-queue)))

(define/contract (reset-message-queue!)
  (-> void?)

  (set! message-queue '()))
