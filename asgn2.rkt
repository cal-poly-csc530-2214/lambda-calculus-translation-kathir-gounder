#lang typed/racket

(require typed/rackunit)

;;  LC	 	=	 	num
;; 	 	|	 	id
;; 	 	|	 	(/ id => LC)
;; 	 	|	 	(LC LC)
;; 	 	|	 	(+ LC LC)
;; 	 	|	 	(* LC LC)
;; 	 	|	 	(ifleq0 LC LC LC)
;; 	 	|	 	(println LC)

(define-type LC (U AppC NumC IdC MultC PlusC PrintC LambdaC Ifleq0C))
(struct AppC ([f : LC] [arg : LC]) #:transparent)
(struct LambdaC ([a : Symbol] [body : LC]) #:transparent)
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([name : Symbol]) #:transparent)
(struct MultC ([l : LC] [r : LC]) #:transparent)
(struct PlusC ([l : LC] [r : LC]) #:transparent)
(struct PrintC ([p : LC]) #:transparent)
(struct Ifleq0C ([test : LC] [then : LC] [otherwise : LC]) #:transparent)

;; (lemons) ? alert("please give me a lemonade") : alert("then give me a beer");

(: translate (-> LC String))
(define (translate lcalc)
  (match lcalc
    [(NumC n) (~a n)]
    [(IdC name) (~a name)]
    [(MultC l r) (string-append (translate l) " " "*" " " (translate r))]
    [(PlusC l r) (string-append (translate l) " " "+" " " (translate r))]
    [(Ifleq0C t then otherwise) (string-append "(" "(" (translate t) " < 0) ? " (translate then) " : " (translate otherwise) ")")]
    [(LambdaC a body) (string-append "(" (~a a) ")" " => " (translate body))]
    [(AppC f arg) (string-append "(" (translate f) ")" "(" (translate arg) ")")]
    [(PrintC p) (string-append "console.log(" (translate p) ")")]
))


(: parse (-> Sexp LC))
(define (parse s)
  (cond [(real? s) (NumC s)]
        [(symbol? s) (IdC s)]
        [(list? s) (match s
                     [(list '+ l r) (PlusC (parse l) (parse r))]
                     [(list '* l r) (MultC (parse l) (parse r))]
                     [(list 'ifleq0 a b c) (Ifleq0C (parse a) (parse b) (parse c))]
                     [(list 'println p) (PrintC (parse p))]
                     [(list 'lambda arg '=> b) (LambdaC (cast arg Symbol) (parse b))]
                     [(list a b) (AppC (parse a) (parse b))])]
        [else (error "bad LC")]))

(check-equal?
 (translate (parse '{println {ifleq0 {ifleq0 {{lambda a => {+ 10 a}} -11} 69 420}
                                     {* {+ 1 1} 2}
                                     {{lambda b => {* 10 b}} 10}}}))
 "console.log(((((((a) => 10 + a)(-11) < 0) ? 69 : 420) < 0) ? 1 + 1 * 2 : ((b) => 10 * b)(10)))")