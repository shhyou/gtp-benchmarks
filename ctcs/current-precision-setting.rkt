#lang racket

(provide current-precision-config
         precision-configs)

(define precision-configs
  '(none types max))

;; Must be a member of ^; modify to change all configurable-ctc precision levels
(define current-precision-config 'max)
