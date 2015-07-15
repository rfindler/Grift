#lang typed/racket
#|
This is a micro compiler that takes a path, reads the contents
of that file, parses the syntax into an ast, type-checks the
ast, and finally converts that ast into an equivalent ast
of the cast calculus.
|#
(provide (all-defined-out))
(require "../language.rkt"
         "../helpers.rkt"
         "./read.rkt"
         "./parse.rkt"
         "./type-check.rkt"
         "./insert-implicit-casts.rkt")

(: reduce-to-cast-calculus (Path Config . -> . Cast0-Lang))
(trace-define (reduce-to-cast-calculus path config)
  (let* ((stx-lang (read path config))
	 (s0 (parse stx-lang config))
	 (s1 (type-check s0 config)))
    (insert-implicit-casts s1 config)))