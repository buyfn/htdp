;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-194) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define triangle-p
  (list (make-posn 20 10)
        (make-posn 20 20)
        (make-posn 30 20)))

(define square-p
  (list (make-posn 10 10)
        (make-posn 20 10)
        (make-posn 20 20)
        (make-posn 10 20)))

; A Polygon is one of:
; - (list Posn Posn Posn)
; - (cons Posn Polygon)

; An NELoP is one of:
; - (cons Posn '())
; - (cons Posn NELoP)

; a plain background image
(define MT (empty-scene 50 50))

; Image NELoP Posn -> Image
; connects the dots in p and additional pos by rendering lines in img
(check-expect (connect-dots MT triangle-p (first triangle-p))
              (scene+line
               (scene+line
                (scene+line MT 20 20 30 20 "red")
                20 10 20 20 "red")
               30 20 20 10 "red"))
(check-expect (connect-dots MT square-p (first square-p))
              (scene+line
               (scene+line
                (scene+line
                 (scene+line MT 20 20 10 20 "red")
                 20 10 20 20 "red")
                10 10 20 10 "red")
               10 20 10 10 "red"))
(define (connect-dots img p pos)
  (cond
    [(empty? (rest p)) (render-line img (first p) pos)]
    [else (render-line (connect-dots img (rest p) pos)
                       (first p)
                       (second p))]))

; Image Polygon -> Image
; renders the given polygon p into img
(check-expect (render-poly MT triangle-p)
              (scene+line (scene+line (scene+line MT 20 10 20 20 "red")
                                      20 20 30 20 "red")
                          30 20 20 10 "red"))
(check-expect (render-poly MT square-p)
              (scene+line
               (scene+line
                (scene+line
                 (scene+line MT 10 10 20 10 "red")
                 20 10 20 20 "red")
                20 20 10 20 "red")
               10 20 10 10 "red"))
(define (render-poly img p)
  (connect-dots img p (first p)))

; Image Posn Posn -> Image
; renders a line from p to q into img
(define (render-line img p q)
  (scene+line
   img
   (posn-x p) (posn-y p) (posn-x q) (posn-y q)
   "red"))
