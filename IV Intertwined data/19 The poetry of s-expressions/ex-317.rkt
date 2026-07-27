#lang htdp/isl+

; Any -> Boolean
; determines whether passed value is an atom
(check-expect (atom? 1) #true)
(check-expect (atom? "hi") #true)
(check-expect (atom? 'hi) #true)
(check-expect (atom? (list 1 2)) #false)
(check-expect (atom? identity) #false)
(define (atom? v)
  (or (number? v)
      (string? v)
      (symbol? v)))

; S-expr Symbol -> N
; counts all occurrences of sy in sexp
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)
(define (count sexp sy)
  (local (; Atom -> N
          ; counts all occurences of sy in at
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)]))
          ; SL -> N
          ; counts all occurrences of sy in sl
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else (+ (count (first sl) sy)
                       (count-sl (rest sl)))])))
    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)])))
