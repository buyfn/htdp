#lang htdp/isl+

(define-struct employee [name ssn pay-rate])
; An Employee is a structure:
; (make-employee String Number Number)
; interpretation: (make-employee n ssn pr) combines
; the name n with social security number ssn and per-hour wage pr

(define-struct work [name hours])
; A (piece of) Work is a structure:
; (make-work String Number)
; interpretation: (make-work n h) combines
; the name of an employee with the amount of hours worked in a week

(define-struct wage [name wage])
; A Wage is a structure:
; (make-wage String Number)
; interpretation: (make-wage n w) combines
; the name of emplyee with his wage earned in a week

; [List-of Employee] [List-of Work] -> [List-of Wage]
; produces a list of wages by multiplying corresponding hours and pay-rates
; from the lists of works and employess
; assume the two lists are of equal lengths
(check-expect (wages* '() '()) '())
(check-expect (wages* (list (make-employee "john" 123 8))
                      (list (make-work "john" 40)))
              (list (make-wage "john" 320)))
(define (wages* employees works)
  (cond
    [(empty? employees) '()]
    [else
     (cons (weekly-wage (first employees) (first works))
           (wages* (rest employees) (rest works)))]))

; Employee Work -> Wage
; computes weekly wage of an employee for given work week
(define (weekly-wage e w)
  (make-wage (employee-name e)
             (* (employee-pay-rate e) (work-hours w))))
