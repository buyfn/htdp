;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-163) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define-struct work [employee rate hours])
; A (piece of) Work is a structure:
; (make-work String Number Number)
; interpretation (make-work n r h) combines the name
; with the pay rate r and the number of hours h

(define-struct work.v2 [employee rate hours])
; A (piece of) Work.v2 is a structure:
; (make-work Employee Number Number)
; interpretation (make-work e r h) combines the employee record e
; with the pay rate r and the number of hours h

(define-struct paycheck [employee amount])
; A Paycheck is a structure:
; (make-paycheck String Number)
; interpretation (make-paycheck name amount) combines
; the employee name and his wage

(define-struct paycheck.v2 [employee amount])
; A Paycheck.v2 is a structure:
; (make-paycheck Employee Number)
; interpretation (make-paycheck employee amount) combines
; the employee record and his wage

(define-struct employee [name number])
; An Employee is a structure:
; (make-employee String Number)
; interpretation (make-employee name number)
; represents employee record with his name and employee number

; Lop (short for list of paychecks) is one of:
; - '()
; (cons Paycheck Lop)

; Low (short for list of works) is one of:
; - '()
; - (cons Work Low)
; interpretation an instance of Low represents the
; hours worked for a number of employees

; Low -> Lop
; computes the paychecks for all weekly work records
(check-expect
    (wage*.v3 (cons (make-work "Robby" 11.95 39) '()))
    (cons (make-paycheck "Robby" (* 11.95 39)) '()))
(define (wage*.v3 an-low)
    (cond
        [(empty? an-low) '()]
        [else (cons (wage.v3 (first an-low))
                    (wage*.v3 (rest an-low)))]))

; Work -> Paycheck
; computes the paycheck for the given work record w
(check-expect (wage.v3 (make-work "Robby" 11.95 39)) (make-paycheck "Robby" (* 11.95 39)))
(check-expect (wage.v3 (make-work "Alexis" 10 20)) (make-paycheck "Alexis" 200))
(define (wage.v3 w)
    (make-paycheck (work-employee w)
                   (* (work-rate w) (work-hours w))))

; Low.v2 -> Lop.v2
; computes the paychecks for all weekly work records
(check-expect
    (wage*.v4 (cons (make-work.v2 (make-employee "Robby" 1) 10 40) '()))
    (cons (make-paycheck.v2 (make-employee "Robby" 1) 400) '()))
(check-expect (wage*.v4 '()) '())
(define (wage*.v4 an-low)
    (cond
        [(empty? an-low) '()]
        [else (cons (wage.v4 (first an-low))
                    (wage*.v4 (rest an-low)))]))

; Work.v2 -> Paycheck.v2
; computes the paycheck for the given work record w
(check-expect
 (wage.v4 (make-work.v2 (make-employee "Robby" 1) 10 40))
 (make-paycheck.v2 (make-employee "Robby" 1) 400))
(check-expect
 (wage.v4 (make-work.v2 (make-employee "Alexis" 2) 15.5 20))
 (make-paycheck.v2 (make-employee "Alexis" 2) 310))
(define (wage.v4 w)
    (make-paycheck.v2 (work.v2-employee w)
                      (* (work.v2-rate w) (work.v2-hours w))))
