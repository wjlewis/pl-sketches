#lang racket

;; We replace all `define` forms with a combination of `let` and `set!`.
;; Along with removing a syntactic form to have to deal with later, this
;; simplifies the treatment of internal definitions. The input language
;; must have
;; 1. Only simple `define`s
;; That is, the following must be desugared in advance:
;;
;; ```
;; (define (fn x) (* x x))
;; ```
;;
;; to something like
;;
;; ```
;; (define fn (lambda (x) (* x x)))
;; ```
;;
;; 2. All "blocks" must be enclosed in a `begin` expression.
;; So
;;
;; ```
;; (define fn (lambda (x) (* x x)))
;; ```
;;
;; must be further desugared/transformed to
;;
;; ```
;; (define fn (lambda (x) (begin (* x x))))
;; ```
;;
;; This includes an entire "program" at the toplevel.
;;
;; (These are all very straightforward transformations.)
;;
;; All `define`s within a `begin` expression are collected and
;; introduced by a single `let` expression (putting the names in scope).
;; However, the names are bound to some "unusable" value until they are
;; `set!` at the site of their definition. If one of these values is
;; encountered at runtime, an error of some kind should be produced.

(provide rm-define)

(define (rm-define e)
  (match e
    [(? literal?) e]
    [(? symbol?) e]
    [`(define ,n ,e) `(set! ,n ,(rm-define e))]
    [`(if ,e1 ,e2 ,e3) `(if ,(rm-define e1)
                            ,(rm-define e2)
                            ,(rm-define e3))]
    [`(let ([,ns ,es] ...) ,b)
     (let* ([es (map rm-define es)]
            [bindings (map list ns es)])
       `(let (,@bindings)
          ,(rm-define b)))]
    [`(set! ,n ,e) `(set! ,n ,(rm-define e))]
    [`(lambda (,ns ...) ,b)
     `(lambda (,@ns)
        ,(rm-define b))]
    [`(begin ,es ..1)
     (let ([names (defined-names es)])
       (if (empty? names)
           ;; If there are no definitions, there is no need for an
           ;; enclosing `let`
           `(begin ,@(map rm-define es))
           (let ([bindings (map (lambda (n) `(,n __undefined))
                                names)]
                 [body (map replace-define es)])
                 `(let (,@bindings)
                    (begin ,@body)))))]
    [`(,rator ,rands ...)
     `(,(rm-define rator)
       ,@(map rm-define rands))]
    [else
     (error (format "Unrecognized expression: ~a" e))]))

(define (defined-names block)
  (let* ([defs (filter (lambda (e)
                         (eq? (first e) 'define))
                       block)]
         [names (map second defs)])
    (remove-duplicates names)))

(define (replace-define e)
  (match e
    [`(define ,n ,v) `(set! ,n ,(rm-define v))]
    [e e]))

(define (literal? e)
  (or (number? e)
      (boolean? e)))
