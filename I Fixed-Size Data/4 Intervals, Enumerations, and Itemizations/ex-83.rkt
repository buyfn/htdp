;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-83) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define CURSOR (rectangle 1 20 "solid" "red"))
(define TEXT-SIZE 11)
(define TEXT-COLOR "black")

(define-struct editor [pre post])
; An Editor is a structure:
; (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with
; the cursor displayed between s and t

; Editor -> Image
; renders Editor text on an empty scene
(check-expect (render (make-editor "hello " "world"))
              (overlay/align "left" "center"
                             (beside/align "center"
                                           (text "hello " 11 "black")
                                           CURSOR
                                           (text "world" 11 "black"))
                             (empty-scene 200 20)))
(define (render state)
  (overlay/align "left" "center"
                 (beside/align "center"
                               (text (editor-pre state) TEXT-SIZE TEXT-COLOR)
                               CURSOR
                               (text (editor-post state) TEXT-SIZE TEXT-COLOR))
                 (empty-scene 200 20)))
  