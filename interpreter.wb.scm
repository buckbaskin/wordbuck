(load "parser_level1/simpleParser.scm")

; INTERPRETER
; Interpreter takes in a parsed file, and evaluates the
;  main method. 
; NOTE: intermediary steps before final version may execute
;  diferent portions of code than just a main method
; in -> parse tree with main method
; out-> program output

; run tests with (_i "parser_level1/p1_test01.txt")

; _ leading underscore denotes a function called by other scripts

(define _help
  (lambda ()
    "help! Use (_i file_path [#t|#f]) to run"))

(define interpret_debug
  (lambda (parsed-file)
    (begin
      (display "parsed file \n")
      (display parsed-file)
      (display "\ninterpreter out:\n")
      (interpret parsed-file))))

(define interpret
  (lambda (parsed-file)
    (process_arg_list parsed-file (new_state))))

(define _run-interpreter
  (lambda (raw-file)
    (interpret (parser raw-file))))

(define _i
  (lambda (file debug)
    (cond
      (debug (interpret_debug (parser file)))
      (else (interpret (parser file))))))

(define process_arg_list
  (lambda (l state)
    (cond
      ((null? l) (error "Null input to interpreter."))
      (else (M_state (car l) (cdr l) state 
                     (lambda (state) (error "code did not return")) 
                     (lambda (val state) (pretify val)) 
                     (lambda (excep) (error "Code throws exception")) 
                     (lambda (cont) (error "Code attempts to continue outside of loop"))
                     (lambda (break) (error "Code attempts to break outside of loop")))))))


; ====== START INTERPRETER BODY ======

; ====== UTILTIES ======

(define new_state
  (lambda ()
    ; ((names) (objs))
    '(() ())))

(define is_valid_state?
  (lambda (state)
    (cond
      ((null? state) #f)
      ((not (list? state)) #f)
      ((eq? (length state) '2) #t)
      (else #f))))

(define pretify
  (lambda (value)
    (cond
      ((eq? value #t) "Truth")
      ((eq? value #f) "Naht Truth")
      (else value))))

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
    (cond
      ((null? arg) (error "M_state: argument is null"))
      ((not (is_valid_state? state)) (error "M_state: invalid state"))
      ((is_return? arg) (M_s_return arg arg_list state term return excep cont break))
      (else (error "That part of M_s is not defined")))))

; === M state pieces ===

(define is_return?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (eq? (length arg) '2)) #f)
      (else (eq? 'return (car arg))))))

(define M_s_return
  (lambda (arg arg_list state term return excep cont break)
    (M_value (cadr arg) state (lambda (val state) (return val state)))))

; === M_state utils ===
      
(define create_obj
  (lambda (name state)
    (list (cons name (car state)) (cons 'notDefined (cadr state)))))

(define create ; helper to create_obj when the state is more complicated
  (lambda (name var_list val_list return)
    (error "create: not yet implemented")))

(define assign_obj ; method is the programmer facing. Splits state, and then rebuilds state
  (lambda (name obj state)
    (assign name obj (car state) (cadr state) (lambda (names objs) (list names objs)))))
     
(define assign
  (lambda (name obj name_list obj_list return)
    (cond
      ((or (null? name_list) (null? obj_list)) (error "assign_var: variable not yet initialized"))
      ((eq? name (car name_list)) (return name_list (cons obj (cdr obj_list))))
      (else (assign name obj (cdr name_list) (cdr obj_list) (lambda (names objs) (return (cons (car name_list) names) (cons (car obj_list) objs))))))))



; ====== M Value ======
; evaluate 
(define M_value
  (lambda (arg state return)
    (cond
      ((null? arg) (error "M_value: null arg"))
      ((integer? arg) (return arg state))
      ((is_math? arg) (M_v_math arg state return))
      (else (display arg) (error "M_value for this isn't implemented")))))
       
; === M value pieces ===

(define is_math?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((integer? arg) #f)
      (else #t))))

(define M_v_math
  (lambda (arg state return)
    (cond
      ((op? '% '2 arg) (op_2 remainder (cadr arg) (caddr arg) state return))
      ((op? '/ '2 arg) (op_2 quotient (cadr arg) (caddr arg) state return))
      ((eq? '3 (length arg)) (op_2 (eval (car arg)) (cadr arg) (caddr arg) state return))
      ((eq? '2 (length arg)) (op_1 (eval (car arg)) (cadr arg) state return))
      (else (display arg) (error "M_v_math: this operator not yet implemented")))))

; === M value utlities ===

(define op?
  (lambda (operator num_args input)
    (cond
      ((null? input) #f)
      ((not (eq? (+ num_args 1) (length input))) #f)
      (else (eq? operator (car input))))))

(define op_2
  (lambda (operation left_arg right_arg state0 return)
    (M_value left_arg state0 (lambda (val_left state1) 
                                                       (M_value right_arg state1 (lambda (val_right state2)
                                                                                     (return (operation val_left val_right) state2)))))))

; ====== M boolean ======
; evaluate conditional (while, for, if that kind of thing)