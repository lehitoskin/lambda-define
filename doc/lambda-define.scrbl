#lang scribble/manual
@; lambda-define.scrbl
@(require racket/sandbox
          scribble/example
          (for-label racket/base
                     "../lambda-define.rkt"))

@title{@bold{lambda-define}: Racket Definition Macros.}
@author{Lehi Toskin}

@defmodule[lambda-define]{
  @racketmodname[lambda-define] is a library for programmers to keep track of
  procedure definitions in larger programs. Say you've forgotten the arity of
  a procedure or perhaps what exactly its definition is. Instead of opening the
  file and scrolling all the way to where it's located, simply call
  @racket[(λ-access-list procedure-name)] and you'll be greeted with a list
  version of the procedure.
}

@defstruct[λ-access ([proc procedure?] [list list?]) #:omit-constructor]{
  The procedure struct. This is what will store the procedure's list
  representation for later viewing.
}

@defform*[((λ-define id expr)
           (λ-define (head args) body ...)
           (λ-define (head . args) body ...)
           (λ-define (head arg0 . rest-args) body ...))]{
  Define a procedure and store its list representation in the procedure struct.
}

@defform[(λ-set! id lmb)
         #:contracts ([id identifier?]
                      [lmb procedure?])]{
  Mutate the identifier @racket[id] so it becomes a procedure specified by
  @racket[lmb]. This will ensure the new procedure is named after @racket[id]
  and has its list representation stored.
}

@examples[
#:eval (parameterize ([sandbox-output 'string]
                      [sandbox-error-output 'string])
         (make-module-evaluator (build-path "doc/../lambda-define.rkt")
                                #:language 'racket/base))
#:once

(λ-define (foo . bar)
  (for ([b (in-list bar)])
    (displayln b)))
(foo 1 2 3 4 5)
(λ-access-list foo)
]
