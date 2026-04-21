;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-84) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
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

; Editor KeyEvent -> Editor
; Produces updated editor based on key pressed;
; - ignores Tab "\t" and Return "\r"
; - adds single-character KeyEvent to the end of (editor-pre ed)
; - for ke = "\b", deletes the last character of (editor-pre ed)
; - for ke = "left" moves cursor one character to the left
; - for ke = "right" moves cursor one character to the right
; - ignores all other KeyEvents
(check-expect (edit (make-editor "a" "b") "c")
              (make-editor "ac" "b"))
(check-expect (edit (make-editor "a" "b") "\b")
              (make-editor "" "b"))
; deleting a character at cursor position 0
(check-expect (edit (make-editor "a" "b") "\b")
              (make-editor "" "b"))
; moving cursor left at cursor position 0
(check-expect (edit (make-editor "a" "b") "left")
              (make-editor "" "ab"))
(check-expect (edit (make-editor "a" "b") "right")
              (make-editor "ab" ""))
(define (edit ed ke)
  (cond
    [(string=? "\t" ke) ed]
    [(string=? "\r" ke) ed]
    [(string=? "\b" ke)
     (cond
       [(> (string-length (editor-pre ed)) 0)
        (make-editor (string-remove-last (editor-pre ed))
                     (editor-post ed))]
       [else ed])]
    [(string=? "left" ke)
     (cond
       [(> (string-length (editor-pre ed)) 0)
        (make-editor (string-remove-last (editor-pre ed))
                     (string-append (string-last (editor-pre ed))
                                    (editor-post ed)))]
       [else ed])]
    [(string=? "right" ke)
     (cond
       [(> (string-length (editor-post ed)) 0)
        (make-editor (string-append (editor-pre ed)
                                    (string-first (editor-post ed)))
                     (string-remove-first (editor-post ed)))]
       [else ed])]
    [(= (string-length ke) 1)
     (make-editor (string-append (editor-pre ed) ke)
                  (editor-post ed))]
    [else ed]))

; Editor -> Editor
(define (run state)
  (big-bang state
      [on-key edit]
      [to-draw render]))

; String -> String
; removes the last character from a string
(check-expect (string-remove-last "abc") "ab")
(define (string-remove-last str)
  (substring str 0 (- (string-length str) 1)))

; String -> String
; removes the first character from a string
(check-expect (string-remove-first "abc") "bc")
(define (string-remove-first str)
  (substring str 1))

; String -> 1String
; returns the last character of a string
(check-expect (string-last "abc") "c")
(define (string-last str)
  (substring str (- (string-length str) 1)))

; String -> 1String
; returns the first character of a string
(check-expect (string-first "abc") "a")
(define (string-first str)
  (substring str 0 1))

(run (make-editor "Edit me" ""))