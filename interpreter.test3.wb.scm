(load "interpreter.wb.scm")
(load "parser_level3/functionParser.scm")

(define _test
  (lambda ()
    (cond
      ((test_list '("parser_level3\\p3_test01.txt" "parser_level3\\p3_test02.txt" "parser_level3\\p3_test03.txt" "parser_level3\\p3_test04.txt" ; 4
                                                   "parser_level3\\p3_test05.txt" "parser_level3\\p3_test06.txt" "parser_level3\\p3_test07.txt" ; 7
                                                   "parser_level3\\p3_test08.txt" "parser_level3\\p3_test09.txt" "parser_level3\\p3_test10.txt" ; 10
                                                   "parser_level3\\p3_test11.txt" #|test 12 returns param err |# "parser_level3\\p3_test13.txt" ; 12
                                                   "parser_level3\\p3_test14.txt" "parser_level3\\p3_test15.txt" "parser_level3\\p3_test16.txt" ; 15
                                                   #|test 17 returns scope err |# "parser_level3\\p3_test18.txt" "parser_level3\\p3_test19.txt" ; 17
                                                   "parser_level3\\p3_test20.txt") ; 18
                  '(10 14 45 55 1 ; 5
                       115 "Truth" 20 24 2 ; 10
                       35 #|12:err|# 90 69 87 ; 14
                       64 #|17:err|# 3421 20332 21; 15
                       )
                  '1) (begin
                        (display "All Tests passed\n")
                        #t))
      (else (begin (display " ^ Test failed.\n") #f)))))

(define test_list
  (lambda (files vals test_number)
    (cond
      ((or (null? files) (null? vals)) #t)
      (else
       (begin (display "test ") (display test_number)
              (cond
                ((or (null? files) (null? vals)) #t)
                ((test_one (car files) (car vals)) (begin (display " passed\n") (test_list (cdr files) (cdr vals) (+ 1 test_number))))
                (else (begin (display " failed\n") #f))))))))
                ;(else (begin (display " failed\n") (test_list (cdr files) (cdr vals) (+ 1 test_number))))))))))

(define test_one
  (lambda (file expected_value)
    (eq? (_i file #f) expected_value)))