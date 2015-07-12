(load "parser_level1/simpleParser.scm")

; INTERPRETER
; Interpreter takes in a parsed file, and evaluates the
;  main method. 
; NOTE: intermediary steps before final version may execute
;  diferent portions of code than just a main method
; in -> parse tree with main method
; out-> program output

; run tests with (_i "parser_level1/p1_test01.txt")

(define interpret
  (lambda (parsed-file)
    (begin
      (display "parsed file \n")
      (display parsed-file)
      (display "\ninterpreter out:\n")
      (process_arg_list parsed-file (new_state)))))

(define _run-interpreter
  (lambda (raw-file)
    (interpret (parser raw-file))))

(define _i
  (lambda (file)
    (_run-interpreter file)))

(define process_arg_list
  (lambda (l state)
    (error "process_arg_list not yet implemented")))

(define new_state
  (lambda ()
    ; ((vars) (vals))
    '(() ())))
