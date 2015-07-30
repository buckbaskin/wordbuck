(load "interpreter.wb.scm")

(define _test
  (lambda ()
    (cond
      ((test_list '("parser_level1\\p1_test01.txt" "parser_level1\\p1_test02.txt" "parser_level1\\p1_test03.txt" "parser_level1\\p1_test04.txt"
                                                   "parser_level1\\p1_test05.txt" "parser_level1\\p1_test06.txt" "parser_level1\\p1_test07.txt"
                                                   "parser_level1\\p1_test08.txt" "parser_level1\\p1_test09.txt" "parser_level1\\p1_test10.txt"
                                                   
                                                   "parser_level1\\p1_test15.txt" "parser_level1\\p1_test16.txt" "parser_level1\\p1_test17.txt"
                                                   "parser_level1\\p1_test18.txt")
                  '(150 -4 10 16 220 5 6 10 5 -39 "Truth" 100 "Naht Truth" "True" 30 11 1106 12 16 72)
                  '1) (begin
                        (display "All Tests passed\n")
                        #t))
      (else (begin (display " <<< failed test(s)\n") #f)))))

(define test_list
  (lambda (files vals test_number)
    (cond
      ((or (null? files) (null? vals)) #t)
      (else
       (begin (display "test ") (display test_number)
              (cond
                ((or (null? files) (null? vals)) #t)
                ((test_one (car files) (car vals)) (begin (display " passed\n") (test_list (cdr files) (cdr vals) (+ 1 test_number))))
                (else (begin (display " failed\n") (test_list (cdr files) (cdr vals) (+ 1 test_number))))))))))

(define test_one
  (lambda (file expected_value)
    (eq? (_i file #f) expected_value)))