#lang racket/base

(require (only-in "eval.rkt"
  forth-eval*
))
(require (only-in racket/file file->lines)
         (only-in "../../../ctcs/precision-config.rkt"
                  configurable-ctc)
         racket/contract
         (only-in racket/math natural?))

;; =============================================================================

(define/contract LOOPS
  (configurable-ctc
   [max 10]
   [types natural?])
  10)

(define/contract (main lines)
  (configurable-ctc
   [max (-> (listof string?) void?)]
   [types (-> (listof string?) void?)])

  (for ((i (in-range LOOPS)))
    (define-values [_e _s] (forth-eval* lines))
    (void)))

(define/contract lines
  (configurable-ctc
   [max (listof string?)]
   [types (listof string?)])

  (file->lines "../base/history-100.txt"))

(time (main lines))
