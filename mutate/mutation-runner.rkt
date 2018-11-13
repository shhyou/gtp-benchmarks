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

(define (run-with-mutated-module main-module module-to-mutate mutation-index)
  (define main-module-path `(file ,main-module))
  (define module-to-mutate/path `(file ,module-to-mutate))
  (define module-to-mutate/file-path (module-path->path module-to-mutate/path))
  (define-values (module-containing-directory _1 _2)
    (split-path module-to-mutate/file-path))
  (define-values (mutant-module-stx mutated-id)
    (mutate-module module-to-mutate/file-path mutation-index))

  (parameterize ([current-load/use-compiled
                  ;; Prevent loading from bytecode to ensure mutant is loaded
                  (make-custom-load/use-compiled
                   #:blacklist (λ (path)
                                 (equal? path module-to-mutate/file-path)))]
                 [current-namespace (make-base-namespace)])
    (eval '(require "mutate.rkt"))

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
      (eval `(require ,main-module-path))))
  mutated-id)

(define/contract (mutant-status run-mutant-thunk)
  ((-> any/c) . -> . (or/c 'completes 'crashes 'index-exceeded))

  (with-handlers ([mutation-index-exception?
                   (λ (e) 'index-exceeded)]
                  [exn? (λ (e) 'crashes)])
    (begin
      (run-mutant-thunk)
      'completes)))

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


