#lang racket/base

(require racket/match)
(provide (except-out (all-defined-out)
                     string-ref* skip-whitespace char-non-numeric?
                     string-natural-compare make-path-comparator))

(define (string-ref* str i)
  (if (< i (string-length str))
      (string-ref str i)
      #\nul))

(define (skip-whitespace str i)
  (if (char-whitespace? (string-ref* str i))
      (skip-whitespace str (add1 i))
      i))

(define (char-non-numeric? ch)
  (not (char-numeric? ch)))

(define (string-natural-compare a b [case-insensitive? #f])
  (let loop ([ai 0] [bi 0])
    (let ([ai (skip-whitespace a ai)]
          [bi (skip-whitespace b bi)])
      (match (cons (string-ref* a ai) (string-ref* b bi))
        [(cons #\nul #\nul) '=]
        [(cons #\nul _) '<]
        [(cons _ #\nul) '>]
        [(cons #\0 #\0)
         (let compare-left ([ai ai] [bi bi])
           (match (cons (string-ref* a ai) (string-ref* b bi))
             [(cons (? char-non-numeric?) (? char-non-numeric?))
              (loop ai bi)]
             [(cons (? char-non-numeric?) _) '<]
             [(cons _ (? char-non-numeric?)) '>]
             [(cons ca cb)
              (cond
                [(char<? ca cb) '<]
                [(char>? ca cb) '>]
                [else (compare-left (add1 ai) (add1 bi))])]))]
        [(cons (? char-numeric?) (? char-numeric?))
         (let compare-right ([ai ai] [bi bi] [bias #f])
           (match (cons (string-ref* a ai) (string-ref* b bi))
             [(cons (? char-non-numeric?) (? char-non-numeric?))
              (or bias (loop ai bi))]
             [(cons (? char-non-numeric?) _) '<]
             [(cons _ (? char-non-numeric?)) '>]
             [(cons ca cb)
              (compare-right
               (add1 ai)
               (add1 bi)
               (or bias (and (char<? ca cb) '<) (and (char>? ca cb) '>)))]))]
        [(cons ca cb)
         (cond
           [(and case-insensitive? (char-ci<? ca cb)) '<]
           [(and case-insensitive? (char-ci>? ca cb)) '>]
           [(and (not case-insensitive?) (char<? ca cb)) '<]
           [(and (not case-insensitive?) (char>? ca cb)) '>]
           [else (loop (add1 ai) (add1 bi))])]))))

(define (string-natural<? s1 s2)
  (eq? (string-natural-compare s1 s2 #f) '<))

(define (string-natural<=? s1 s2)
  (case (string-natural-compare s1 s2 #f)
    [(< =) #t]
    [else #f]))

(define (string-natural>? s1 s2)
  (eq? (string-natural-compare s1 s2 #f) '>))

(define (string-natural>=? s1 s2)
  (case (string-natural-compare s1 s2 #f)
    [(> =) #t]
    [else #f]))

(define (string-natural-ci<? s1 s2)
  (eq? (string-natural-compare s1 s2 #t) '<))

(define (string-natural-ci<=? s1 s2)
  (case (string-natural-compare s1 s2 #t)
    [(< =) #t]
    [else #f]))

(define (string-natural-ci>? s1 s2)
  (eq? (string-natural-compare s1 s2 #t) '>))

(define (string-natural-ci>=? s1 s2)
  (case (string-natural-compare s1 s2 #t)
    [(> =) #t]
    [else #f]))

(define (make-path-comparator string-comparator)
  (lambda (x1 x2)
    (string-comparator (if (path? x1) (path->string x1) x1)
                       (if (path? x2) (path->string x2) x2))))

(define path-natural<? (make-path-comparator string-natural<?))
(define path-natural<=? (make-path-comparator string-natural<=?))
(define path-natural>? (make-path-comparator string-natural>?))
(define path-natural>=? (make-path-comparator string-natural>=?))
(define path-natural-ci<? (make-path-comparator string-natural-ci<?))
(define path-natural-ci<=? (make-path-comparator string-natural-ci<=?))
(define path-natural-ci>? (make-path-comparator string-natural-ci>?))
(define path-natural-ci>=? (make-path-comparator string-natural-ci>=?))
