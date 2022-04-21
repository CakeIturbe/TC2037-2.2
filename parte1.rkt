;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bar) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define fahrenheit-to-celsius
  (lambda (f)
    (/(*(- f 32) 5) 9)))

(define sign
  (lambda (n)
    (cond
      [( = n 0) 0]
      [( < n 0) -1]
      [else 1])))

(define roots
  (lambda (a b c)
    (/(+ (- b)(sqrt (- (* b b)(* 4 a c))))(* 2 a))))

(define bmi-range
  (lambda (bmi)
    (cond
      [(< bmi 20) "underweight"]
      [(and (>= bmi 20) (< bmi 25)) "normal"]
      [(and (>= bmi 25) (< bmi 30)) "obese1"]
      [(and (>= bmi 30) (< bmi 40)) "obese2"]
      [else "obese3"])))


(define bmi
  (lambda (w h)
    (bmi-range (/ w (* h h)))))

(define factorial
  (lambda (n)
    (cond
      [(= n 0) 1]
      [else (* n (factorial(- n 1)))])))

(define duplicate-aux
  (lambda (n r)
    (cond
      [( = r 0) '()]
      [else (cons n (duplicate-aux n (- r 1)))])))

(define duplicate
  {lambda (lst)
    (cond
      [(null? lst) '()]
    [else(append (duplicate-aux (car lst) 2) (duplicate (cdr lst)))])})

  
(define pow
  (lambda (a b)
    (cond
      [(= b 0) 1]
      [else (* a (pow a (- b 1)))])))

(define fib
  (lambda (f)
    (cond
      [(<= f 1) f]
      [else (+ (fib(- f 1)) (fib(- f 2)))])))

(define enlist
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [else (cons (cons(car lst) '()) (enlist(cdr lst)))])))

(define positives
  (lambda (lst)
    (cond
      [(null? lst) '()]
     [(negative? (car lst))(positives(cdr lst))]
      [else (cons (car lst) (positives(cdr lst)))])))

(define add-list
  (lambda (lst)
    (cond
      [(null? lst) 0]
      [else(+ (car lst) (add-list(cdr lst)))])))

(define invert-pairs
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [else(cons(cdr(car lst)) (car(car lst)))])))

(define list-of-symbols?
  (lambda (lst)
    (cond
    [(null? lst) #t]
    [(number? (car lst)) #f]
    [else(list-of-symbols?(cdr lst))])))

(define average
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [else(/ (add-list lst) (length lst))])))

(define replic
  {lambda (lst n)
    (cond
      [(null? lst) '()]
    [else(append (duplicate-aux (car lst) n) (replic (cdr lst) n))])})






