(load "interpreter.wb.scm")

(define _test
  (lambda ()
    (cond
      ((test_list '("parser_level1\\p1_test01.txt" "parser_level1\\p1_test02.txt" "parser_level1\\p1_test03.txt" "parser_level1\\p1_test04.txt"
                                                   "parser_level1\\p1_test05.txt" "parser_level1\\p1_test06.txt" "parser_level1\\p1_test07.txt"
                                                   "parser_level1\\p1_test08.txt" "parser_level1\\p1_test09.txt" "parser_level1\\p1_test10.txt") 
                  '(150 -4 10 16 220 5 6 10 5 -39)
                  '1) (begin
                        (display " passed\nAll Tests passed\n")
                        #t))
      (else (begin (display " <<< failed test(s)\n") #f)))))

(define test_list
  (lambda (files vals test_number)
    (begin (display "test ") (display test_number)
    (cond
      ((or (null? files) (null? vals)) #t)
      ((test_one (car files) (car vals)) (begin (display " passed\n") (test_list (cdr files) (cdr vals) (+ 1 test_number))))
      (else (begin (display " failed\n") (test_list (cdr files) (cdr vals) (+ 1 test_number))))))))

(define test_one
  (lambda (file expected_value)
    (eq? (_i file #f) expected_value)))