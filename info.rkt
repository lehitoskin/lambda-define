#lang setup/infotab

(define name "lambda-define")
(define scribblings '(("doc/lambda-define.scrbl" ())))

(define blurb '("Racket definition macros to keep track of procedure definitions."))
(define primary-file "lambda-define.rkt")
(define homepage "https://github.com/lehitoskin/lambda-define/")

(define version "0.2")
(define release-notes '("Currying accounted for."))

(define required-core-version "6.3")

(define deps '("base"
               "rackunit-lib"
               "scribble-lib"))
(define build-deps '("racket-doc"
                     "sandbox-lib"))
