(load "interpreter.wb.scm")

(define test
  (lambda ()
    (cond
      ((test_list '("parser_level1\\p1_test01.txt") '(150) '1) (begin
                                                  (display "All Tests passed\n")
                                                  #t))
      (else (begin (display " <<< failed test(s)\n") #f)))))

(define test_list
  (lambda (files vals test_number)
    (cond
      ((or (null? files) (null? vals)) #t)
      ((test_one (car files) (car vals)) (test_list (cdr files) (cdr vals) (+ 1 test_number)))
      (else (begin (display test_number) #f)))))

(define test_one
  (lambda (file expected_value)
    (eq? (_i file #f) expected_value)))