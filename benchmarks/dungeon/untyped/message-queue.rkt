#lang racket/base

(require racket/contract
         (only-in racket/list first empty?))

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
  (let ([pre/queue-len #f])
    (->i ([m string?])
         #:pre () (set! pre/queue-len (length message-queue))
         [result void?]
         #:post (m) (and (= (length message-queue)
                            (add1 pre/queue-len))
                         (string=? m (first message-queue)))))

  (set! message-queue (cons m message-queue)))

(define/contract (reset-message-queue!)
  (->* () ()
       void?
       #:post (empty? message-queue))

  (set! message-queue '()))
