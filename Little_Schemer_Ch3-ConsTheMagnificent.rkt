#lang scheme

; The second commandment: use cons to build lists

; pp. 37 and 41
; Remove atom a from the list of atom lat
; atom, lat -> lat
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

; The third commandment: When building a list, describe the first typical element, and then cons it onto the natural recursion

; pp. 46
; takes a list of lists, and returns a new list of the first element of each list in the list
; list(list) -> list
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

; pp. 50
; takes two atoms, one the atom to add and one the atom to add after, and a list, and adds the new atom after the old atom (only the first occurrence of old)
; if old is not in the list, new will not be inserted
; atom, atom, lat -> lat
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat)))) ; could also be (cons (car lat) (cons new (cdr lat) (cdr lat)))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

; pp. 51
; like insertR, but insert to left of old
; atom, atom, lat -> lat
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

; pp. 51
; construct a new list where we substite the new atom for the old atom (only the first occurrence)
; atom, atom, lat -> lat
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

; pp. 52
; like subst, but there are two old variables for replacement now (only the first occurrence of ONE OF THE TWO is replaced)
; atom, atom, atom -> lat
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

; pp. 53
; like rember, but remove all occurrences of the atom from the lat
; atom, lat -> lat
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat))) ; this is where multirember differs from rember
      (else (cons (car lat) (multirember a (cdr lat)))))))

; pp. 56
; like insertR but insert to the right of every occurrence of old
; atom, atom, lat -> lat
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat))))) ; this is where multiinsertR differs from insertR
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

; pp. 57
; like insertL but insert to the right of every occurrence of old
; atom, atom, lat -> lat
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat))))) ; this is where multiinsertL differs from insertL
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

; The fourth commandment (preliminary): Always change at least one argument while recurring. It must be changed to be closer to termination.
;                                       The changing argument must be tested in the termination condition:
;                                           When using cdr, test termination with null?.

; pp. 57
; construct a new list where we substite the new atom for the old atom at all occurrecnes
; atom, atom, lat -> lat
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

; not included in the book, but I don't know why
; like subst, but there are two old variables to be replaced now
; atom, atom, atom -> lat
(define multisubst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (multisubst2 new o1 o2 (cdr lat))))
      (else (cons (car lat) (multisubst2 new o1 o2 (cdr lat)))))))