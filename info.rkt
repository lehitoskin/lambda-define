#lang setup/infotab

(define name "lambda-define")
(define scribblings '(("doc/lambda-define.scrbl" ())))

(define blurb '("Racket definition macros to keep track of procedure definitions."))
(define primary-file "lambda-define.rkt")
(define homepage "https://github.com/lehitoskin/lambda-define/")

(define version "0.1")
(define release-notes '("Initial package assembly."))

(define required-core-version "6.3")

(define deps '("base"
               "scribble-lib"))
(define build-deps '("racket-doc"
                     "sandbox-lib"))
