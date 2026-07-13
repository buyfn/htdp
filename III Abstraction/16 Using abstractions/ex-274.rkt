#lang htdp/isl+

; A Prefix is a [List-of 1String]
; A Suffix is a [List-of 1String]

; [List-of 1String] -> [List-of Prefix]
; produces the list of all prefixes of list los
(check-expect (prefixes '()) '())
(check-expect (prefixes (list "a" "b" "c"))
              (list (list "a" "b" "c")
                    (list "a" "b")
                    (list "a")))
(define (prefixes los)
  (map reverse (suffixes (reverse los))))

; [List-of 1String] -> [List-of Suffix]
; produces the list of all suffixes of list los
(check-expect (suffixes '()) '())
(check-expect (suffixes (list "a" "b" "c"))
              (list (list "a" "b" "c")
                    (list "b" "c")
                    (list "c")))
(define (suffixes los)
  (local (; 1String [List-of Suffix] -> [List-of Suffix]
          ; produces new acc with added next suffix from los starting from current s
          (define (add-next-suf s acc)
            (cond
              [(empty? acc) (cons (cons s '()) acc)]
              [else (cons (cons s (first acc))
                          acc)])))
    (foldr add-next-suf '() los)))
