#lang racket

;; Require and Provide other language details 
(require Schml/language/shared
         Schml/framework/pprint)

(provide (all-from-out Schml/language/shared))

(struct Prog (name exp)
        #:methods gen:custom-write
        [(define write-proc (lambda (o p d)
                              (with-printed-labels
                               (pretty-print
                                (hang prog-indent-size
                                      (doc-list
                                       (v-append
                                        (hs-append (text "Prog:")
                                                   (text (Prog-name o)))
                                        (expr->doc (Prog-exp o)))))
                                p)
                               p)))]
        #:transparent)


(provide
 (contract-out
  [struct Prog ((name string?) (exp Expr?))]))

;; The super type of core forms that are considered expressions
(struct Expr (ty) #:transparent)
(struct Lambda Expr (fmls exp) #:transparent)
(struct Var Expr (id) #:transparent)
(struct App Expr (exp exp*) #:transparent)
(struct Cast Expr (exp ty-exp lbl) #:transparent)
(struct If Expr (tst csq alt) #:transparent)
(struct Let Expr (bnds exp) #:transparent)
(struct Const Expr (const) #:transparent)
(struct Prim Expr (pexp))

(provide
 (contract-out
  [struct Expr ((ty Type?))]
  [struct (Lambda Expr) ((ty Type?)
                         (fmls (listof (or/c Fml? Fml:Ty?))) 
                         (exp Expr?))]
  [struct (Var Expr) ((ty Type?) (id uvar?))] 
  [struct (App Expr) ((ty Type?) (exp Expr?) (exp* (listof Expr?)))]
  [struct (Cast Expr) ((ty Type?) (exp Expr?) (ty-exp Type?) (lbl label?))]
  [struct (If Expr) ((ty Type?) (tst Expr?) (csq Expr?) (alt Expr?))]
  [struct (Let Expr) ((ty Type?)
                      (bnds (listof (or/c Bnd? Bnd:Ty?)))
                      (exp Expr?))]
  [struct (Const Expr) ((ty Type?) (const constant?))]
  ;; There is curren
  [struct (Prim Expr) ((ty Type?) (pexp PExpr?))]))


(define (expr->doc o)
  (define bnd->doc (mk-bnd->doc expr->doc))
  (define prim->doc (mk-prim->doc expr->doc))
  (match o
    [(App (app type->doc t) (app expr->doc exp) (list (app expr->doc exp*) ...))
     (doc-list (align (vs-append exp (vs-concat exp*) (hs-append  colon t))))]
    [(Lambda (app type->doc t) (list (app bnd->doc fmls) ...) (app expr->doc body)) 
     (hang lambda-indent-size
           (doc-list (v-append (vs-append (text "lambda") (doc-list (vs-concat fmls)))
                               (h-append colon t)
                               body)))]
    [(Let (app type->doc t) (list (app bnd->doc bnds) ...) (app expr->doc body))
     (hang let-indent-size
           (doc-list (v-append
                      (hs-append (text "let") (hang 1 (doc-list (v-concat bnds))))
                      (hs-append colon t)
                      body)))]
    [(Cast (app type->doc t2) (app expr->doc e) (app type->doc t1) l)
     (doc-list
      (align (vs-append (hs-append colon (label->doc! l)) (align e) t1 t2)))]
    [(If (app type->doc t) (app expr->doc tst) (app expr->doc csq) (app expr->doc alt))
     (doc-list
      (hs-append (text "if") (align (v-append tst csq alt (hs-append colon t)))))]
    [(or (Var _ (app format->doc k)) (Const _ (app format->doc k))) k]
    [(Prim t pe) (prim->doc pe)]
    [o (text "Non printable expr")]))
