#lang racket/base
; lambda-define.rkt
; GPL-3+
(require rackunit (for-syntax racket/base syntax/define))
(provide (struct-out λ-access) λ-define λ-set!)

(struct λ-access (proc list)
  #:property prop:procedure
  (struct-field-index proc))

; thanks to mbutterick
(define-syntax (λ-define stx)
  (with-syntax ([(id lambda-exp)
                 (let-values ([(id-stx body-exp-stx) (normalize-definition stx #'λ #t #t)])
                   (list id-stx body-exp-stx))])
    #'(define id (if (procedure? lambda-exp)
                     (λ-access (procedure-rename lambda-exp 'id)
                               'lambda-exp)
                     lambda-exp))))

(define-syntax (λ-set! stx)
  (with-syntax ([(id lambda-exp)
                 (let-values ([(id-stx body-exp-stx) (normalize-definition stx #'λ #t #t)])
                   (list id-stx body-exp-stx))])
    #'(set! id (if (procedure? lambda-exp)
                   (λ-access (procedure-rename lambda-exp 'id)
                             'lambda-exp)
                   lambda-exp))))

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
  (check-equal? (λ-access-list qub) '(λ (#:kw kw) (+ kw 5)))

  ; value
  (λ-define qud (λ () 'qud))
  (check-equal? (λ-access-list qud) '(λ () 'qud))

  ; not a procedure
  (λ-define qug (+ 1 2))
  (check-false (λ-access? qug))

  ; currying
  (λ-define ((add n) m) (+ n m))
  (check-equal? (λ-access-list add) '(λ (n) (λ (m) (+ n m)))))
