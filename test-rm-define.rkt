#lang racket

(require rackunit
         "rm-define.rkt")

(test-case
    "Toplevel defines are scanned out"
  (check-equal?
   (rm-define '(begin
                 (define even?
                   (lambda (n)
                     (begin
                       (if (zero? n)
                           #t
                           (odd? (pred n))))))
                 (define odd?
                   (lambda (n)
                     (begin
                       (if (zero? n)
                           #f
                           (even? (pred n))))))
                 (define n 42)
                 (even? n)))
   '(let ([even? __undefined]
          [odd? __undefined]
          [n __undefined])
      (begin
        (set! even?
              (lambda (n)
                (begin
                  (if (zero? n)
                      #t
                      (odd? (pred n))))))
        (set! odd?
              (lambda (n)
                (begin
                  (if (zero? n)
                      #f
                      (even? (pred n))))))
        (set! n 42)
        (even? n)))))

(test-case
    "Internal defines are scanned out"
  (check-equal?
   (rm-define '(begin
                 (define fancy
                   (lambda (x y)
                     (begin
                       (define bar
                         (lambda (n)
                           (begin
                             (* n x))))
                       (define quux 42)
                       (define zaz
                         (lambda (q y)
                           (begin
                             (+ q q (* x y)))))
                       (bar (zaz quux 3)))))))
   '(let ([fancy __undefined])
      (begin
        (set! fancy
              (lambda (x y)
                (let ([bar __undefined]
                      [quux __undefined]
                      [zaz __undefined])
                  (begin
                    (set! bar (lambda (n)
                                (begin
                                  (* n x))))
                    (set! quux 42)
                    (set! zaz (lambda (q y)
                                (begin
                                  (+ q q (* x y)))))
                    (bar (zaz quux 3))))))))))
