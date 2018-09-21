#lang racket/base
;; TODO use open?

(provide
 empty-cell%
 void-cell%
 door%
 vertical-door%
 horizontal-door%
 char->cell%
 wall%
 void-cell%
 cell%?
 cell%/c
 class-equal?
 cell%
 )

;; -----------------------------------------------------------------------------

(require
 racket/class
 "../base/un-types.rkt"
 racket/contract
 )
(require (only-in racket/function
                  curry)
         (only-in racket syntax-case syntax))
(require (only-in "message-queue.rkt"
                  enqueue-message!
                  ))
(require (only-in racket/dict
                  dict-ref
                  dict-set!
                  dict-has-key?
                  ))

;; =============================================================================

(define-syntax (class/c* stx)
  (syntax-case stx (field/all all inherit+super)
    [(_ (field/all fspec ...)
        (all all-spec ...)
        (inherit+super i+s-spec ...)
        other-specs ...)
     #'(class/c (field fspec ...)
                (inherit-field fspec ...)
                ;; all
                (inherit all-spec ...)
                (super all-spec ...)
                (override all-spec ...)
                ;; i+s
                (inherit i+s-spec ...)
                (super i+s-spec ...)
                ;; rest
                other-specs ...)]))

(define ((equal-to/c thing) other)
  (equal? thing other))

(define-syntax-rule (make-cell%/c-with self-id show-char
                                       free?/occupant-comparer)
  (class/c* (field/all [items list?]     ;; ll: never seems to
                       [occupant any/c]) ;; actually be used

            (all
             [open (->m void?)]
             [close (->m void?)]
             [free? (->m boolean?)])
            (inherit+super
             [show (->i ([self-id any/c])
                        [result (self-id) show-char])])
            (override
             [show (->m char?)])

            [free? (->i ([self-id any/c])
                        [result (self-id)
                                (curry free?/occupant-comparer
                                       (get-field occupant
                                                  self-id))])]))

(define cell%/c (make-cell%/c-with self any/c (λ x #t)))

(define/contract cell% ; some kind of obstacle by default
  (make-cell%/c-with self #\* equal?)
  (class object%
    (inspect #f)
    (init-field [items    '()]
                [occupant #f]) ; player, monster, etc.
    (define/public (free?)
      #f)
    (define/public (show)
      #\*) ; for debugging
    (define/public (open)
      (enqueue-message! "Can't open that."))
    (define/public (close)
      (enqueue-message! "Can't close that."))
    (super-new)))

(define cell%? (instanceof/c cell%/c))

;; maps printed representations to cell classes
;; for map parsing
(define/contract chars->cell%s
  (hash/c char? cell%/c)
  (make-hash))

;; Workaround for bug(?) in class comparison:
;; (subclass? cell% (dict-ref chars->cell%s #\*)) => #f
(define (class-equal? a% b%)
  (and (subclass? a% b%)
       (subclass? b% a%)))

(define/contract (register-cell-type! c% char)
  (->i ([c% cell%/c]
        [char char?])
       [result void?]
       #:post (c% char) (class-equal? (dict-ref chars->cell%s char (λ x (void)))
                                      c%))

  (dict-set! chars->cell%s char c%))

(define/contract (char->cell% char)
  (->i ([char (and/c char? (curry dict-has-key? chars->cell%s))])
       [result (char)
               (and/c cell%/c
                      (curry class-equal? (dict-ref chars->cell%s char)))])

  (dict-ref chars->cell%s char))

(register-cell-type! cell% #\*)

(define/contract empty-cell%
  (make-cell%/c-with self
                     (or/c #\space
                           (send (get-field occupant self)
                                 show))
                     (not/c equal?))
  (class cell%
    (inspect #f)
    (inherit-field occupant)
    (define/override (free?)
      (not occupant))
    (define/override (show)
      (if occupant
          (send (or occupant (raise-user-error 'show)) show)
          #\space))
    (super-new)))
(register-cell-type! empty-cell% #\space)

(define/contract void-cell%
  (make-cell%/c-with self #\. equal?)
  (class cell%
    (inspect #f)
    (define/override (show) #\.) ; for testing only
    (super-new)))
(register-cell-type! void-cell% #\.)

(define/contract wall%
  (make-cell%/c-with self #\X equal?)
  (class cell%
    (inspect #f)
    (define/override (show) #\X) ; for testing only
    (super-new)))
(register-cell-type! wall% #\X)

(define double-bar? #t)
(define-syntax-rule (define-wall name single-bar double-bar)
  (begin (define/contract name
           (make-cell%/c-with self
                              (if double-bar? double-bar single-bar)
                              equal?)
           (class wall%
             (inspect #f)
             (define/override (show) (if double-bar? double-bar single-bar))
             (super-new)))
         ;; parse either kind
         (register-cell-type! name single-bar)
         (register-cell-type! name double-bar)
         (provide name)))
(define-wall pillar%           #\+     #\#)
(define-wall vertical-wall%    #\u2502 #\u2551)
(define-wall horizontal-wall%  #\u2500 #\u2550)
(define-wall four-corner-wall% #\u253c #\u256c)
(define-wall north-east-wall%  #\u2510 #\u2557)
(define-wall north-west-wall%  #\u250c #\u2554)
(define-wall south-east-wall%  #\u2518 #\u255d)
(define-wall south-west-wall%  #\u2514 #\u255a)
(define-wall north-tee-wall%   #\u252c #\u2566)
(define-wall south-tee-wall%   #\u2534 #\u2569)
(define-wall east-tee-wall%    #\u2524 #\u2563)
(define-wall west-tee-wall%    #\u251c #\u2560)

(define/contract door%
  (make-cell%/c-with self #\* (not/c equal?))
  (class cell%
    (inspect #f)
    ;(init-field [open? #f])
    (inherit-field occupant)
    (define/override (free?)
      (and #;open? (not occupant)))
    (define/override (open)
      (if #t ;open?
          (enqueue-message! "The door is already open.")
          (void) #;(set! open? #t)))
    (define/override (close)
      (if #t ;open?
          (void) #;(set! open? #f)
          (enqueue-message! "The door is already closed.")))
    (super-new)))

(define/contract vertical-door%
  (make-cell%/c-with self
                     (or/c #\_
                           (send (get-field occupant self)
                                 show))
                     (not/c equal?))
  (class door%
    (inspect #f)
    (inherit-field #;open? occupant)
    (define/override (show)
      (if #t ;open?
          (if occupant (send (or occupant (raise-user-error 'vdoor)) show) #\_)
          #\|))
    (super-new)))
(register-cell-type! vertical-door% #\|)

(define/contract other-vertical-door%
  (make-cell%/c-with self
                     (or/c #\_
                           (send (get-field occupant self)
                                 show))
                     (not/c equal?))
  (class vertical-door%
    (inspect #f)
    (super-new #;[open? #t])))
(register-cell-type! other-vertical-door% #\_)

(define/contract horizontal-door%
  (make-cell%/c-with self
                     (or/c #\'
                           (send (get-field occupant self)
                                 show))
                     (not/c equal?))
  (class door%
    (inspect #f)
    (inherit-field #;open? occupant)
    (define/override (show)
      (if #t ;open?
          (if occupant (send (or occupant (raise-user-error 'hdoor)) show) #\')
          #\-))
    (super-new)))
(register-cell-type! horizontal-door% #\-)

(define/contract other-horizontal-door%
  (make-cell%/c-with self
                     (or/c #\'
                           (send (get-field occupant self)
                                 show))
                     (not/c equal?))

  (class horizontal-door%
    (inspect #f)
    (super-new #;[open? #t])))
(register-cell-type! other-horizontal-door% #\')

;; TODO chests, entry/exit
