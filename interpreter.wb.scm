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
    (cond
      ((null? l) (error "Null input to interpreter."))
      (else (M_state (car l) (cdr l) state (lambda (v) v))))))


; ====== START INTERPRETER BODY ======

; ====== UTILTIES ======

(define new_state
  (lambda ()
    ; ((vars) (vals))
    '(() ())))

; ====== M Func ======
; return the state, value of the function execution

; ====== M State ======
; returns the state modified by the given code
; This is for use within functions, etc

; ====== M Value ======
; evaluate 

; ====== M boolean ======
; evaluate conditional (while, for, if that kind of thing)