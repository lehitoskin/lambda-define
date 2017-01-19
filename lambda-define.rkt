#lang racket/base
; lambda-define.rkt
; GPL-3+
(require rackunit)
(provide (struct-out λ-access) λ-define λ-set!)

(struct λ-access (proc list)
  #:property prop:procedure
  (struct-field-index proc))

(define-syntax λ-define
  (syntax-rules ()
    ; no parameters
    [(_ (id) body0 ...)
     (begin
       (define id
         (λ-access (procedure-rename (λ () body0 ...) (quote id))
                   '(λ () body0 ...))))]
    ; one or more parameters
    [(_ (id arg0 ...) body0 ...)
     (begin
       (define id
         (λ-access (procedure-rename (λ (arg0 ...) body0 ...) (quote id))
                   `(λ ,(quote (arg0 ...)) body0 ...))))]
    ; args with rest args
    [(_ (id arg0 ... . rest-arg) body0 ...)
     (begin
       (define id
         (λ-access (procedure-rename (λ (arg0 ... . rest-arg) body0 ...) (quote id))
                   `(λ ,(quote (arg0 ... . rest-arg)) body0 ...))))]
    ; rest args
    [(_ (id . rest-arg) body0 ...)
     (begin
       (define id
         (λ-access (procedure-rename (λ rest-arg body0 ...) (quote id))
                   `(λ ,(quote rest-arg) body0 ...))))]))

(define-syntax-rule (λ-set! id lmb)
  (set! id (λ-access (procedure-rename lmb (quote id)) (quote lmb))))

; (λ-define (((foo bar) baz) bin) ...) ; => '(λ (bar) (λ (baz) (λ (bin) ...)))
#|(define arg-lst
  (let loop ([nested '(((((id a) b)) d) e)]
             [accum '()])
    (cond [(and (list? nested) (list? (car nested)))
           (loop (car nested) (cons (cdr nested) accum))]
          [(and (list? nested) (not (list? (car nested))))
           (cons nested accum)])))

(define lmbs
  (for/fold ([accum '(body0 ...)])
            ([arg (in-list (reverse arg-lst))])
    (append `(λ ,arg) (list accum))))
(define-values (before after) (split-at lmbs 2))
(values
 (caadr before)
 (append `(λ ,(cdadr before)) after))|#

(module+ test
  (λ-define (identity1) (void))
  (check-true (void? (identity1)))
  (check-equal? (λ-access-list identity1) '(λ () (void)))
  (λ-set! identity1 newline)
  (check-equal? (λ-access-list identity1) 'newline)
  
  (λ-define (identity2 x y z) (values x y z))
  (check-equal? (λ-access-list identity2) '(λ (x y z) (values x y z)))
  (λ-set! identity2 (λ (a b c d) (+ a b c d)))
  (check-equal? (λ-access-list identity2) '(λ (a b c d) (+ a b c d)))
  
  (λ-define (foo) (list 'bar 'baz 'bin))
  (check-equal? (λ-access-list foo) '(λ () (list 'bar 'baz 'bin)))

  (λ-define (bar x . yz) (append (list x) yz))
  (check-equal? (bar 1 2 3 4 5 6) '(1 2 3 4 5 6))

  (λ-define (baz [x 0]) (+ x 1))
  (check-= (baz) (baz 0) 0.0)

  (λ-define (bin . args) (println args))
  (check-equal? (λ-access-list bin) '(λ args (println args)))

  (λ-define (qub #:kw kw) (+ kw 5))
  (check-equal? (λ-access-list qub) '(λ (#:kw kw) (+ kw 5))))
