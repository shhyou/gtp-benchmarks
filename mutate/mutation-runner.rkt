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
     (strip-context
      #`(module name lang
          (#%module-begin
           #,@(mutate-program #'(body ...) mutation-index))))]))

(define (run-with-mutated-module main-module module-to-mutate mutation-index)
  (define main-module-path `(file ,main-module))
  (define module-to-mutate/path `(file ,module-to-mutate))
  (define module-to-mutate/file-path (module-path->path module-to-mutate/path))
  (define-values (module-containing-directory _1 _2)
    (split-path module-to-mutate/file-path))
  (define mutant-module-stx (mutate-module module-to-mutate/file-path
                                           mutation-index))

  (parameterize ([current-load/use-compiled
                  ;; Prevent loading from bytecode to ensure mutant is loaded
                  (make-custom-load/use-compiled
                   #:blacklist (λ (path)
                                 (equal? path module-to-mutate/file-path)))]
                 [current-namespace (make-base-namespace)])
    ;; Ensure relative load paths work
    (parameterize
        ([current-directory module-containing-directory]
         [current-load-relative-directory module-containing-directory])
      (eval '(require "mutate.rkt"))
      ;; Load the mutated module and make it impersonate the original one,
      ;; so that loading the original module loads the mutant instead
      (parameterize ([current-module-declare-name
                      (module-path-resolve module-to-mutate/path)])
        (eval mutant-module-stx)))

    ;; Eval the main module
    (eval `(require ,main-module-path))))

(define/contract (mutant-status run-mutant-thunk)
  ((-> any/c) . -> . (or/c 'completes 'crashes 'index-exceeded))

  (with-handlers ([mutation-index-exception?
                   (λ (e) 'index-exceeded)]
                  [exn? (λ (e) 'crashes)])
    (begin
      (run-mutant-thunk)
      'completes)))

;; lltodo: write a function that will run all mutants possible to
;; generate for a set of modules.
