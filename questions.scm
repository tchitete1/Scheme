(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (x) (cons first x)) rests)
)

(define (zip pairs)
  (list (map car pairs) (map cadr pairs))
)

;; Problem 16
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 16
  (define (enumerate-inner s i new-s-so-far)
    ; (cond
    ;   [(null? s) new-s-so-far]
    ;   [(define curr-pair (cons i (list (car s)))) 
    ;    (enumerate-inner (cdr s) (+ i 1) (append new-s-so-far (list curr-pair)))
    ;   ]
    ; )

    (if (null? s)
      new-s-so-far
      (begin
        (define curr-pair (cons i (list (car s)))) 
        (enumerate-inner (cdr s) (+ i 1) (append new-s-so-far (list curr-pair)))
      )
    )
  )
  (enumerate-inner s 0 '())
)
  ; END PROBLEM 16

;; Problem 17
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 17
  ; Code obtained from ocseal
  ; https://github.com/ocseal/sp2019-cs61a/blob/master/scheme/questions.scm
  ; Accessed 12/02/2021
  (cond 
    ((or (null? denoms) (< total 0)) nil) 
    ((= total 0) (list ()))
    (else
      (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
  )
  ; End of code obtained from ocseal
  ; END PROBLEM 17
)

;; Problem 18
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 18
         expr 
         ; END PROBLEM 18
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           ; Code obtained from ocseal
           ; https://github.com/ocseal/sp2019-cs61a/blob/master/scheme/questions.scm
           ; Accessed 12/02/2021
           (cons form (cons (map let-to-lambda params) (map let-to-lambda body)))
           ; End of code obtained from above source
           ; END PROBLEM 18
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           ; Code obtained from ocseal
           ; https://github.com/ocseal/sp2019-cs61a/blob/master/scheme/questions.scm
           ; Accessed 12/02/2021
           (cons (cons 'lambda (cons (car (zip (let-to-lambda values))) (let-to-lambda body))) (cadr (zip (let-to-lambda values))))
           ; End of code obtained from above source
           ; END PROBLEM 18
           ))
        (else
         ; BEGIN PROBLEM 18
         ; Code obtained from ocseal
         ; https://github.com/ocseal/sp2019-cs61a/blob/master/scheme/questions.scm
         ; Accessed 12/02/2021
         (map let-to-lambda expr)
         ; End of code obtained from above source
         ; END PROBLEM 18
         )))