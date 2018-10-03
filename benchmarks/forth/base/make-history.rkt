#lang racket/base

(require (only-in racket/string string-join)
         (only-in "../../../ctcs/common.rkt"
           memberof/c)
         racket/contract)

(define binop* '(+ - *))
(define other* '(dup drop over swap))
(define new* (box '()))

;; lltodo: parametric?
(define/contract (random-ref xs)
  (configurable-ctc
   [max (->i ([xs (listof any/c)])
             [result (xs) (memberof/c xs)])]
   [types ((listof any/c) . -> . any/c)])

  (list-ref xs (random (length xs))))

(define/contract (random-def)
  (configurable-ctc
   [max (let ([new*-len/before #f])
          (->* #:pre (set! new*-len/before (length (unbox new*)))
               #rx"define \\(([a-zA-Z0-9] )+\\) "
               #:post (let* ([new** (unbox new*)]
                             [new**/first (first new**)])
                        ;; Every call adds a symbol to the front of new*
                        (and ;; Only one thing added
                         (= (length new**)
                            (add1 new*-len/before))
                         ;; Thing was a symbol
                         (symbol? new**/first)
                         ;; Added symbol is unique
                         (not (member new**/first (rest new**)))))))]
   [types (-> string?)])

  (define cmd* (for/list ([_i (in-range (add1 (random 10)))])
                 (random-ref other*)))
  (define name (gensym (apply string-append (map symbol->string cmd*))))
  (set-box! new* (cons name (unbox new*)))
  (string-join (list* "define" (map symbol->string (cons name cmd*))) " "))

;; ll: temporal: this should print things
(define/contract (print-random-command n)
  (configurable-ctc
   [max (->i ([n number?])
             [result (n) (if (< n 2)
                             (+ 1 n)
                             (memberof/c (list (sub1 n) n (add1 n))))])]
   [types (number? . -> . number?)])

  (if (< n 2)
    (begin (printf "push ~a\n" (random 9001))
           (+ 1 n))
    (case (random 5)
     [(0) (displayln (random-ref binop*))
          (- n 1)]
     [(1 2) (printf "push ~a\n" (random 9001))
            (+ 1 n)]
     [(2) (displayln (random-ref other*))
          n]
     [(3) (displayln (if (null? (unbox new*))
              (random-def)
              (random-ref (unbox new*))))
          n]
     [(4) (displayln (random-def))
          n])))

(module+ main
  (require racket/cmdline)
  (command-line
   #:args (N-str out-file)
   (define N (string->number N-str))
   (with-output-to-file out-file #:exists 'replace
     (lambda ()
       (for/fold ([size 0])
                 ([i (in-range N)])
         (print-random-command size))))
   (void)))
