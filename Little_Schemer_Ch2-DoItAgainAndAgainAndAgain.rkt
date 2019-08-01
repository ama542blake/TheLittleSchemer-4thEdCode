#lang scheme

; required for the chapter (given in chapter 1, pp. 10)
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; pp. 15
; is the list a list of atom?
; list -> boolean
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; pp. 22
; check if a is a member of lat
; any, lat -> boolean
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or
             (eq? (car lat) a)
             (member? a (cdr lat)))))))

; First commandment (preliminary) Always ask null? as the first question in expressing any function.