#lang htdp/isl
(require 2htdp/image)

(define triangle-p
  (list (make-posn 20 10)
        (make-posn 20 20)
        (make-posn 30 20)))

(define square-p
  (list (make-posn 10 10)
        (make-posn 20 10)
        (make-posn 20 20)
        (make-posn 10 20)))

; a plain background image
(define MT (empty-scene 50 50))

; Image Polygon -> Image 
; adds a corner of p to img
(define (render-poly img poly)
  (local (; Polygon -> Posn
          ; extracts the last item from p
          (define (last p)
            (cond
              [(empty? (rest (rest (rest p)))) (third p)]
              [else (last (rest p))]))
          ; NELoP -> Image
          ; connects the Posns in p in an image
          (define (connect-dots p)
            (cond
              [(empty? (rest p)) img]
              [else (render-line (connect-dots (rest p))
                                 (first p)
                                 (second p))])))
    (render-line (connect-dots poly) (first poly) (last poly))))

; Image Posn Posn -> Image
; renders a line
(define (render-line im p q)
  (scene+line
   im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))


