#lang htdp/isl+
(require 2htdp/abstraction)

(define TIME_ENTRY_NOT_FOUND_ERROR "Could not find time entry for employee")
(define EMPLOYEE_NOT_FOUND_ERROR "Could not find employee for time card")

(define-struct employee [name number pay-rate])
; An Employee is a structure
; (make-employee String Number Number)
; interpretation: (make-employee name number pay-rate)
; combines employee name, number and their pay rate
(define john (make-employee "John" 1 8))
(define kate (make-employee "Kate" 2 15))

(define-struct time-card [employee-number hours])
; A TimeCard is a structure
; (make-time-card Number Number)
; interpretation: (make-time-card employee-number hours)
; represents a time card which records how many hours employee
; with employee-number has worked in given week
(define time-1 (make-time-card 1 40))
(define time-2 (make-time-card 2 30))

(define-struct wage [employee-name wage])
; A Wage is a structure
; (make-wage String Number)
; interpretation: (make-wage name wage)
; combines an employee name with their weekly wage

; [List-of Employee] [List-of TimeCard] -> [List-of Wage]
; produces a list of Wage records which contain the names and
; weekly wage of an employee
(check-expect (wages* (list john) (list time-1))
              (list (make-wage "John" 320)))
(check-error (wages* (list john kate) (list time-2))
             TIME_ENTRY_NOT_FOUND_ERROR)
(check-error (wages* (list john) (list time-1 time-2))
             EMPLOYEE_NOT_FOUND_ERROR)
(check-expect (wages* '() '()) '())
(define (wages* employees times)
  (if (andmap (lambda (t)
                (ormap (lambda (emp) (= (employee-number emp)
                                        (time-card-employee-number t)))
                       employees))
              times)
      (map (lambda (emp) (get-wage emp times)) employees)
      (error EMPLOYEE_NOT_FOUND_ERROR)))

; Employee [List-of TimeCard] -> Wage
(define (get-wage empl times)
  (local ((define empl-time
            (filter (lambda (t)
                      (= (time-card-employee-number t)
                         (employee-number empl)))
                    times)))
    (if (= (length empl-time) 0)
        (error TIME_ENTRY_NOT_FOUND_ERROR)
        (make-wage (employee-name empl)
                   (* (time-card-hours (first empl-time))
                      (employee-pay-rate empl))))))
