(load "interpreter.wb.scm")
(load "parser_level2/simpleParser.scm")

(define _test
  (lambda ()
    (cond
      ((test_list '("parser_level1\\p1_test01.txt" "parser_level1\\p1_test02.txt" "parser_level1\\p1_test03.txt" "parser_level1\\p1_test04.txt" ; 4
                                                   "parser_level1\\p1_test05.txt" "parser_level1\\p1_test06.txt" "parser_level1\\p1_test07.txt" ; 7
                                                   "parser_level1\\p1_test08.txt" "parser_level1\\p1_test09.txt" "parser_level1\\p1_test10.txt" ; 10
                                                   
                                                   "parser_level1\\p1_test15.txt" "parser_level1\\p1_test16.txt" "parser_level1\\p1_test17.txt" ; 13
                                                   "parser_level1\\p1_test18.txt" ; 14
                                                   "parser_level1\\p1_test19.txt" "parser_level1\\p1_test20.txt" "parser_level1\\p1_test21.txt" ; 17
                                                   "parser_level1\\p1_test22.txt" "parser_level1\\p1_test23.txt" "parser_level1\\p1_test24.txt" ; 20
                                                   
                                                   "parser_level2\\p2_test01.txt" "parser_level2\\p2_test02.txt" "parser_level2\\p2_test03.txt" ; 23
                                                   "parser_level2\\p2_test04.txt" "parser_level2\\p2_test05.txt" "parser_level2\\p2_test06.txt" ; 26
                                                   "parser_level2\\p2_test07.txt" "parser_level2\\p2_test11.txt" "parser_level2\\p2_test12.txt" ; 29
                                                   "parser_level2\\p2_test13.txt") ; 30
                  '(150 -4 10 16 220 ; 5
                        5 6 10 5 -39 ; 10
                        "Truth" 100 "Naht Truth" "Truth" 30 ; 15
                        11 1106 12 16 72 ; 20
                        100 20 6 -1 789 ; 25
                        2 164 12 32 21 ; 30
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
    (eq? (_i2 file #f) expected_value)))