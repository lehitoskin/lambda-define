# lambda-define

Racket Definition Macros that may be used to help debug larger programs.
When defining a procedure, use `lambda-define` to keep a copy of the
procedure's definition as a list.

For example:
```racket
#lang racket/base
(require lambda-define)

(λ-define (foo x)
  (+ x 5))
(λ-access-list foo)
;=> '(λ (x) (+ x 5))
```
