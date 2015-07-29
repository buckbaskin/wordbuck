(load "parser_level1/simpleParser.scm")

; INTERPRETER
; Interpreter takes in a parsed file, and evaluates the
;  main method.
; NOTE: intermediary steps before final version may execute
;  diferent portions of code than just a main method
; in -> parse tree with main method
; out-> program output

; run tests with (_i "parser_level1\\p1_test01.txt" #t)

; _ leading underscore denotes a function called by other scripts

(define _help
  (lambda ()
    "help! Use (_i file_path [#t|#f]) to run"))

(define interpret-debug
  (lambda (parsed-file)
    (begin
      (display "parsed file \n")
      (display parsed-file)
      (display "\ninterpreter out:\n")
      (process_arg_list parsed-file (new_state) #t))))

(define interpret
  (lambda (parsed-file)
    (process_arg_list parsed-file (new_state) #f)))

(define _run-interpreter
  (lambda (raw-file)
    (interpret (parser raw-file))))

(define _i
  (lambda (file debug)
    (cond
      (debug (interpret-debug (parser file)))
      (else (interpret (parser file))))))

(define process_arg_list
  (lambda (l state debug)
    (cond
      ((null? l) (error "Null input to interpreter."))
      (else (M_state (car l) (cdr l) state 
                     (lambda (state) (error "code did not return")) 
                     (lambda (val state)
                       (cond
                         (debug (begin (display state)) (display "\n") (pretify val))
                         (else (pretify val)))) 
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
      ((is_declare? arg) (M_s_declare arg arg_list state term return excep cont break))
      ((is_assign? arg) (M_s_assign arg arg_list state term return excep cont break))
      ((is_if? arg) (M_s_if arg arg_list state term return excep cont break))
      (else (display "\nstate\n") 
            (display state) 
            (display "\narg\n") 
            (display arg)
            (display "\n") 
            (error "^ The above part of M_state is not defined")))))

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

(define is_declare?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((> (length arg) 3) #f)
      ((< (length arg) 2) #f)
      (else (eq? 'var (car arg))))))

(define M_s_declare
  (lambda (arg arg_list state term return excep cont break)
    (cond
      ((eq? (length arg) 2) (M_state (car arg_list) (cdr arg_list) (create_obj (cadr arg) state) term return excep cont break))
      (else (M_s_assign (list '= (cadr arg) (caddr arg)) arg_list (create_obj (cadr arg) state) term return excep cont break)))))

(define is_assign?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      (else (eq? '= (car arg))))))

(define M_s_assign
  (lambda (arg arg_list state term return excep cont break)
    (M_value (caddr arg) state (lambda (val state1) (M_state (car arg_list) (cdr arg_list) (assign_obj (cadr arg) val state1) term return excep cont break)))))
;(if <condition> <then> <else>)
;(car cadr caddr cadddr)
(define is_if?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((> (length arg) 4) #f)
      ((< (length arg) 3) #f)
      (else (eq? 'if (car arg))))))

(define M_s_if
  (lambda (arg arg_list state term return excep cont break)
    (M_bool (cadr arg) state (lambda (condition state)
                               (cond
                                 (condition (M_state (caddr arg) arg_list state term return excep cont break))
                                 ((eq? (length arg) '4) (M_state (cadddr arg) arg_list state term return excep cont break))
                                 (else (M_state (car arg_list) (cdr arg_list) state term return excep cont break)))))))
                                  

; === M_state utils ===
      
(define create_obj
  (lambda (name state)
    (list (cons name (car state)) (cons 'notDefined (cadr state)))))

(define create ; helper to create_obj when the state is more complicated
  (lambda (name var_list val_list return)
    (error "create: not yet implemented")))

(define assign_obj ; method is the programmer facing. Splits state, and then rebuilds state
  (lambda (name val state)
    (assign name val (car state) (cadr state) (lambda (names vals) (list names vals)))))
     
(define assign
  (lambda (name obj name_list obj_list return)
    (cond
      ((or (null? name_list) (null? obj_list)) (error "Variable assignment before declaration\nassign_var: variable not yet initialized"))
      ((eq? name (car name_list)) (return name_list (cons obj (cdr obj_list))))
      (else (assign name obj (cdr name_list) (cdr obj_list) (lambda (names objs) (return (cons (car name_list) names) (cons (car obj_list) objs))))))))

(define find_var
  (lambda (name state)
    (find name (car state) (cadr state) (lambda (val) val))))

(define find
  (lambda (name var_list val_list return)
    (cond
      ((or (null? var_list) (null? val_list)) (error "Variable access before declaration\nfind_var: variable not yet declared"))
      ((eq? name (car var_list))
       (cond
         ((eq? (car val_list) 'notDefined) (error "Variable access before assignment\nfind_var: variable not initialized"))
         (else (return (car val_list)))))
      (else (find name (cdr var_list) (cdr val_list) return)))))



; ====== M Value ======
; evaluate 
(define M_value
  (lambda (arg state return) ; Note: return takes (value state)
    (cond
      ((null? arg) (error "M_value: null arg"))
      ((integer? arg) (return arg state))
      ((is_math? arg) (M_v_math arg state return))
      ((is_comparison? arg) (M_v_comparison arg state return))
      ((symbol? arg) (return (find_var arg state) state))
      (else (display arg) (error "M_value for this isn't implemented")))))
       
; === M value pieces ===

(define is_math?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      ((> (length arg) 3) #f)
      ((< (length arg) 2) #f)
      ((op? '- 1 arg) #t)
      (else (match_list arg '(- + / % *))))))

(define M_v_math
  (lambda (arg state return)
    (cond
      ((op? '% '2 arg) (op_2 remainder (cadr arg) (caddr arg) state return))
      ((op? '/ '2 arg) (op_2 quotient (cadr arg) (caddr arg) state return))
      ((eq? '3 (length arg)) (op_2 (eval (car arg)) (cadr arg) (caddr arg) state return))
      ((eq? '2 (length arg)) (op_1 (eval (car arg)) (cadr arg) state return))
      (else (display arg) (error "M_v_math: this operator not yet implemented")))))

(define is_bool?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((> (length arg) 3) #f)
      ((< (length arg) 2) #f)
      (else (match_list arg '(&& || !))))))

(define is_comparison?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      ((> (length arg) 3) #f)
      ((< (length arg) 2) #f)
      ((op? '! 1 arg) #t)
      (else (match_list arg '(< <= == != >= >))))))

(define M_v_comparison
  (lambda (arg state return)
    (cond
      ;((op? '!= '2 arg) (op_2 (lambda (a b) (not (eq? a b))) (cadr arg) (caddr arg) state return))
      ;((op? '== '2 arg) (op_2 (lambda (a b) (eq? a b)) (cadr arg) (caddr arg) state return))
      ((eq? '3 (length arg)) (op_2 (eval (car arg)) (cadr arg) (caddr arg) state return))
      (else (display arg) (error "M_v_comparison: this operator not yet implemented")))))

(define ==
  (lambda (a b)
    (eq? a b)))

(define !=
  (lambda (a b)
    (not (eq? a b))))

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

(define op_1
  (lambda (operation arg state0 return)
    (M_value arg state0 (lambda (val_arg state1) (return (operation val_arg) state1)))))

(define match_list
  (lambda (arg list)
    (foldl (lambda (a b) (or a (eval b))) #f (map (lambda (operator) (op? operator 2 arg)) list))))

; ====== M boolean ======
; evaluate conditional (while, for, if that kind of thing)

(define M_bool
  (lambda (arg state return)
    (cond
      ((null? arg) (error "M_bool: null arg"))
      ((or (eq? #t arg) (eq? 'true arg)) (return #t))
      ((or (eq? #f arg) (eq? 'false arg)) (return #f))
      (else (M_value arg state (lambda (v state) 
                                 (cond
                                   ((integer? v) (error "M_bool: condition evaluated to integer"))
                                   (else (return v state)))))))))


; ====== TESTING ======
(define test
  (lambda ()
    (begin
      (load "test.interpreter.wb.scm")
      (_test))))