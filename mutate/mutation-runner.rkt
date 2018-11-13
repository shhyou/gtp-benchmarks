#lang racket

(require custom-load
         syntax/modread
         (only-in syntax/modresolve [resolve-module-path module-path->path])
         syntax/parse
         syntax/strip-context
         "mutate.rkt")

(define (module-path-resolve mod-path [load? #f])
  ((current-module-name-resolver) mod-path #f #f load?))

(define (read-module path)
  (check-module-form
   (with-module-reading-parameterization
     (λ () (with-input-from-file path
             (λ () (port-count-lines! (current-input-port)) (read-syntax)))))
   'ignored path))


;; Produce the mutated syntax for the module at the given path
(define (mutate-module module-file-path mutation-index)
  (syntax-parse (read-module module-file-path)
    #:datum-literals [module #%module-begin]
    [(module name lang (#%module-begin body ...))
     (define program-stx #'{body ...})
     (match-define (mutated-program program-stx/mutated mutated-id)
       (mutate-program/with-id program-stx mutation-index))
     (values
      (strip-context
       #`(module name lang
           (#%module-begin
            #,@program-stx/mutated)))
      mutated-id)]))

(define (make-precision-config-module precision-config)
  #`(module current-precision-setting racket
      (#%module-begin
       (provide (for-syntax current-precision-config))
       (define-for-syntax current-precision-config '#,precision-config))))

(define (make-mutated-module-runner main-module
                                    module-to-mutate
                                    mutation-index
                                    ctc-precision-config)
  (define main-module-path `(file ,main-module))
  (define module-to-mutate/path `(file ,module-to-mutate))
  (define module-to-mutate/file-path (module-path->path module-to-mutate/path))
  (define-values (module-containing-directory _1 _2)
    (split-path module-to-mutate/file-path))
  (define-values (mutant-module-stx mutated-id)
    (mutate-module module-to-mutate/file-path mutation-index))

  (define precision-config/path
    '(file "../ctcs/current-precision-setting.rkt"))
  (define precision-config/file-path
    (module-path->path precision-config/path))
  (define-values (precision-config-containing-directory __1 __2)
    (split-path precision-config/file-path))
  (define precision-config/stx
    (make-precision-config-module ctc-precision-config))

  ;; Make racket/contract come from the same namespace so that
  ;; we can inspect contract violations thrown inside eval
  (define ns (make-base-namespace))
  (namespace-attach-module (current-namespace)
                           'racket/contract
                           ns)

  (define (run)
    (parameterize ([current-load/use-compiled
                    ;; Prevent loading from bytecode to ensure mutant is loaded
                    (make-custom-load/use-compiled
                     #:blacklist (curryr member
                                         (list module-to-mutate/file-path
                                               precision-config/file-path)))]
                   [current-namespace ns])
      (eval '(require "mutate.rkt"))

      ;; Load the precision config make it impersonate the original
      ;; one, so that loading the original module loads the above
      ;; constructed config instead
      (parameterize
          ([current-load-relative-directory
            precision-config-containing-directory]
           ;; Note that this needs to be resolved *before*
           ;; parameterizing current-directory otherwise relative module
           ;; paths get messed up.
           [current-module-declare-name
            (module-path-resolve precision-config/path)]
           ;; Ensure relative load paths work
           [current-directory precision-config-containing-directory])
        (eval precision-config/stx))

      ;; Load the mutated module and make it impersonate the original
      ;; one, so that loading the original module loads the mutant
      ;; instead
      (parameterize
          ([current-load-relative-directory module-containing-directory]
           ;; Note that this needs to be resolved *before*
           ;; parameterizing current-directory otherwise relative module
           ;; paths get messed up.
           [current-module-declare-name
            (module-path-resolve module-to-mutate/path)]
           ;; Ensure relative load paths work
           [current-directory module-containing-directory])
        (eval mutant-module-stx))

      ;; Ensure relative load paths work
      (parameterize
          ([current-directory module-containing-directory])
        ;; Eval the main module
        (eval `(require ,main-module-path)))))

  (values run mutated-id))

(struct run-status (outcome blamed mutated-id) #:transparent)
(define (run-with-mutated-module main-module
                                 module-to-mutate
                                 mutation-index
                                 ctc-precision-config)
  (with-handlers ([mutation-index-exception?
                   (λ (e) (run-status 'index-exceeded
                                      #f #f))])
    (define-values (run mutated-id)
      (make-mutated-module-runner main-module
                                  module-to-mutate
                                  mutation-index
                                  ctc-precision-config))
    (with-handlers ([exn:fail:contract:blame?
                     (λ (be)
                       (run-status 'blamed
                                   (blame-positive
                                    (exn:fail:contract:blame-object be))
                                   mutated-id))]
                    [exn? (λ (e)
                            (run-status 'crashed
                                        e
                                        mutated-id))])
      (begin
        (run)
        (run-status 'completed
                    #f
                    mutated-id)))))

;; note: stack grows down ⇓
;;
;; lltodo: wiw: write a function that will run all mutants possible to
;; generate for a set of modules.

;; Note that this function needs to also tweak the ctc strength


;; for debugging
(define (print-mutation module-to-mutate mutation-index)
  (define-values (mutated-program-stx mutated-id)
    (mutate-module module-to-mutate mutation-index))
  (printf "--------------------\nMutated: ~a\n\n"
          mutated-id)
  (pretty-print (syntax->datum mutated-program-stx)))


