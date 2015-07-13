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
      (else (M_state (car l) (cdr l) state (lambda (v) v) (lambda (v) v))))))


; ====== START INTERPRETER BODY ======

; ====== UTILTIES ======

(define new_state
  (lambda ()
    ; ((vars) (vals))
    '(() ())))

; ====== M Class ======
; return the state given a class definition

; ====== M Func ======
; return the state, value of the function execution

; ====== M State ======
; returns the state modified by the given code
; This is for use within functions, etc
; Continuations: 
; term - called at the end of the evaluation if nothing else occurs (term state)
; return - called if return is called (return value state)
; excep - called if exception is thrown (excep value state)
; cont - called in loop to return to top (cont state)
; break - called in loop to exit loop (break state)

(define M_state
  (lambda (arg arg_list state term return excep cont break)
    (error "M_state not yet implemented")))

; ====== M Value ======
; evaluate 

; ====== M boolean ======
; evaluate conditional (while, for, if that kind of thing)