#lang scheme

; in this chapter, we only consider nonnegative integers, for reasons related to recursion; see the notes above some of the functions

; CONTRACT NOTE: integers are numbers; floats are numbers; numbers are not necessarily floats; numbers are not necessarily ints

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; pp. 59
(define add1
  (lambda (n)
    (+ n 1)))

; pp. 59
(define sub1
  (lambda (n)
    (- n 1)))

; pp. 60
; add two nonzero numbers (technically, n can be negative, but m can not, as m would never recur as 0 and this would cause an infinite loop)
; n may be a decimal number, but m must be an integer (because m can never recur as value 0 and we would therefore have an infinite loop)
; number, int -> number
(define add ; in the book, this the function of the hollow plus sign, but obviously I can't type that
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add (add1 n) (sub1 m))))))

; pp. 60
; perform subtraction between two nonzero numbers (technically, n can be negative, but m can not, as m would never recur as 0 and this would cause an infinite loop)
; n may be a decimal number, but m must be an integer (because m can never recur as value 0 and we would therefore have an infinite loop)
; number, int -> number
(define sub ; in the book, this is the function of the hollow minus sign, but obviously I can't type that
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub (sub1 n) (sub1 m))))))

; 1st Commandment update: When recurring on a number always ask (zero? n) and else

; pp. 64
; sums all numbers in a list of numbers (tuple)
; tuple -> number
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (add (car tup) (addtup (cdr tup)))))))

; 4th Commandment update: When using sub1, test termination with zero?

; pp. 65
; multiply two numbers: again m must be a positive integer, n may be any number
; number, int -> number
(define mult
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (add n (mult n (sub1 m)))))))

; 5th Commandment: When building a value with plus, always use 0 for the value of the terminating line, for adding 0 does not change the value of an addition.
;                  When building a value with mult, always use 1 for the value of the terminating line, for multiplying by 1 does not change the value of a multiplication.
;                  When building a value with cons, always consider () for the value of the terminating line.

; pp. 69
; adds two lists (which must be the same size n) element by element and returns a new list of length n with the results of each addition
; tuple, tuple -> tuple
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) '())
      (else (cons
             (add (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

; pp. 71
; like tup+, but allows tuples to be of different lengths
; tuple, tuple -> tuple
(define tup+dl ; dl meaning "different length": the name of the alternate version of tup+ listed in the book on pp. 71
  (lambda (tup1 tup2)
    (cond                 ; don't need to check if both are null, since if both are, we appended the empty list to the previous iterations which doesn't change anything
      ((null? tup1) tup2) ; if tup1 is empty, we just cons tup1 to the end (sort of like appending 0s to the end of tup1 for the addition)
      ((null? tup2) tup1) ; if tup2 is empty, we just cons tup2 to the end (sort of like appending 0s to the end of tup1 for the addition)
      (else (cons (add (car tup1) (car tup2)) (tup+dl (cdr tup1) (cdr tup2)))))))

; pp. 72
; see if n is greater than m;
; keep in mind, we are only considering nonnegative integers in this chapter, for reasons related to recursion; using negative will result in infinite loop or undefined behavior
; int, int -> boolean
(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

; pp. 73
; see if n is less than m;
; keep in mind, we are only considering nonnegative integers in this chapter, for reasons related to recursion; using negative will result in infinite loop or undefined behavior
; int, int -> boolean
(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

; pp. 74
; see if n and m are equal, but using < and >; more concise than the definition given in the book with use of not
; int, int -> boolean
(define =?
  (lambda (n m)
      (not (or (> n m) (< n m)))))

; pp. 74
; calculate n to the mth power; again assume nonnegative integers
; int, int -> int
(define ↑
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (mult n (↑ n (sub1 m)))))))
; pp. 75
; n / m (integer division)
; int, int -> int
(define ÷
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (÷ (sub n m) m))))))

; pp. 76
; length of a list of atoms
; lat -> int
(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

; pp. 76
; get the nth element from the list (1 indexed)
; int, list -> list element
(define pick
  (lambda (n l)
    (cond
      ;((null? l) '()) ; undefined behavior when n < 1 or n > length of the list: not included in the book, so that's why this is commented out
      ((zero? (sub1 n)) (car l)) ; if we didn't (sub1 n), we would retrieve from the list as if it were 0 indexed
      (else (pick (sub1 n) (cdr l))))))

; pp. 77
; remove nth item from the list
; int, list -> list
(define rempick
  (lambda (n l)
    (cond
      ((null? l) '())
      ((zero? (sub1 n)) (cdr l)) ; same as pick with (sub1 n)
      (else (cons (car l) (rempick (sub1 n) (cdr l)))))))


; pp. 77
; remove all numbers from a list of atoms
; lat -> lat
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

; pp 78
; create a tuple from a list of atoms by extracting all numbers in the lat
; lat -> tuple
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

; pp. 78
; check if 2 atoms (numeric or not) are the same
; atom, atom -> boolean
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((and (atom? a1) (atom? a2)) (eq? a1 a2))
      (else #f))))

; pp. 78
; counts the number of times an atom occurs in a list
; atom, lat -> int
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

; pp. 79
; check if a number is 1, return 0 otherwise
; int -> boolean
(define one?
  (lambda (n)
    (eq? n 1)))

; pp. 79
; rempick, but redefine using one?
; atom, list -> list
(define one?rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (one?rempick (sub1 n) (cdr lat)))))))

(one?rempick 4 '(14 52 532 234))