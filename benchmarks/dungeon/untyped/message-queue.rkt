#lang racket/base

(require racket/contract
         (only-in racket/list first empty? rest)
          "../../../ctcs/precision-config.rkt")

(provide
  enqueue-message!
  reset-message-queue!
)


;; list of strings (messages) which were produced since the previous
;; previous display, and need to be displayed now
(define/contract message-queue
  (configurable-ctc
   [max (listof string?)]
   [types (listof string?)])
  '())

(define/contract (enqueue-message! m)
  (configurable-ctc
   [max (let ([pre/queue-len #f]
              [pre/queue message-queue])
          (->i ([m string?])
               #:pre () (begin (set! pre/queue-len (length message-queue))
                               (set! pre/queue-len message-queue))
               [result void?]
               #:post (m) (and (equal? pre/queue (rest message-queue))
                               (= (length message-queue)
                                  (add1 pre/queue-len))
                               (string=? m (first message-queue)))))]
   [types (string? . -> . void?)])

  (set! message-queue (cons m message-queue)))

(define/contract (reset-message-queue!)
  (configurable-ctc
   [max (->* () ()
             void?
             #:post (empty? message-queue))]
   [types (-> void?)])

  (set! message-queue '()))
