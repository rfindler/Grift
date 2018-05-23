#lang typed/racket/base

(require "./forms.rkt")
(provide (all-defined-out))
#|-----------------------------------------------------------------------------+
| Language/Grift-Syntax 
+-----------------------------------------------------------------------------|#
#| Maybe type |#

;; The language created by grift/read
(define-type Syntax-Lang (Prog String (Listof Stx)))

(define-type Stx (Syntaxof Any));; This might not be what I want considier Just Syntax
(define-type Stx* (Listof Stx))


#|-----------------------------------------------------------------------------+
| Language/Grift0
+-----------------------------------------------------------------------------|#
(define-type Grift0-Lang (Prog (List String Natural) S0-Expr))

(define-type (G0-Form E)
  (U (Lambda Grift-Fml* (Ann E (Option Grift-Type)))
     (Letrec G0-Bnd* E)
     (Let G0-Bnd* E)
     (App E (Listof E))
     (Op Grift-Primitive (Listof E))
     (If E E E)
     (Ascribe E Grift-Type (Option Blame-Label))
     (Var Uid)
     (Quote Grift-Literal)
     (Begin (Listof E) E)
     (Repeat Uid E E E)
     ;; Monotonic effects
     (Mbox E)
     (Munbox E)
     (Mbox-set! E E)
     (Mvector E E)
     (Mvector-set! E E E)
     (Mvector-ref E E)
     ;; Guarded effects
     (Gbox E)
     (Gunbox E)
     (Gbox-set! E E)
     (Gvector E E)
     (Gvector-set! E E E)
     (Gvector-ref E E)))

(define-type G0-Expr
  (Rec E (Ann (G0-Form E) Src)))

(define-type G0-Expr* (Listof G0-Expr))
(define-type G0-Bnd (Bnd Uid Grift-Type? G0-Expr))
(define-type G0-Bnd* (Listof G0-Bnd))

