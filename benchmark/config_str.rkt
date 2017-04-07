#lang racket

(module+ main
  (define configs-file "benchmark/configs.dat")
  (define (read-configs)
    (call-with-input-file configs-file
      (lambda (in) (read in))))
  (define (write-configs)
    (call-with-output-file configs-file
      (lambda (in)
        (write
         (hash
          1 (list (string->symbol "Coercions") (string->symbol "Proxied"))
          2 (list (string->symbol "Type-Based Casts") (string->symbol "Proxied"))
          3 (list (string->symbol "Coercions") (string->symbol "Monotonic"))
          4 (list (string->symbol "Type-Based Casts") (string->symbol "Monotonic")))
         in))))
  
  (command-line
   #:once-any
   [("--generate-configs-file" "-g")
    "Generate the configurations file"
    (if (file-exists? configs-file)
        (begin
          (delete-file configs-file)
          (write-configs))
        (write-configs))]
   [("--all" "-a")
    "Generate configuration strings for all configurations supported by Schml"
    (let ([cs (read-configs)])
      (printf "~a" (string-join
                    (for/list ([i (in-range 1 (+ 1 (hash-count cs)))])
                      (string-join (map symbol->string (hash-ref cs i))
                                   ".")) ",")))]
   [("--indices" "-i")
    "Generate configuration indices"
    (let ([cs (read-configs)])
      (printf "~a" (string-join
                    (map number->string
                         (range 1 (+ 1 (hash-count cs)))) " ")))]
   [("--contrast" "-c") c1 c2
    "Generate configuration strings for the different parameter and a configuration string for shared parameters"
    (let ([n1 (string->number c1)]
          [n2 (string->number c2)])
      (if (and n1 n2)
          (let* ([cs (read-configs)]
                 [s1 (list->set (hash-ref cs n1))]
                 [s2 (list->set (hash-ref cs n2))]
                 [f (lambda (s1 s2)
                      (let ([s (set-subtract s1 s2)])
                        (if (= (set-count s) 1)
                            (symbol->string (set-first s))
                            (error "you should compare configurations that differ in exactly one parameter"))))])
            (printf "~a,~a,~a"
                    (f s1 s2)
                    (f s2 s1)
                    (string-join
                     (map symbol->string
                          (set->list (set-intersect s1 s2))) "_")))
          (error 'config_str: "could not parse ~a and ~a as numbers" c1 c2)))]))