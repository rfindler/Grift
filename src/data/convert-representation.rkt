#lang typed/racket
(require "../language.rkt"
         "./normalize-context.rkt"
         "./remove-let.rkt"
         "./remove-complex-opera.rkt"
         "./flatten-values.rkt"
         "./simplify-predicates.rkt")

(provide convert-representation)

(: convert-representation (Data0-Lang Config . -> . Data5-Lang))
(define (convert-representation d0 config)
  (let* ([d1 (normalize-context d0 config)]
         [d2 (remove-let d1 config)]
         [d3 (remove-complex-opera d2 config)]
         [d4 (flatten-values d3 config)]
         [d5 (simplify-predicates d4 config)])
    d5))