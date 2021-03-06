(load "parser_level3/functionParser.scm")

; INTERPRETER
; Interpreter takes in a parsed file, and evaluates the
;  main method.
; NOTE: intermediary steps before final version may execute
;  diferent portions of code than just a main method
; in -> parse tree with main method
; out-> program output

; run tests with (_i "parser_level3\\p3_test01.txt" #t)

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
      (m_func_read parsed-file (new_state) #t (lambda (state) state)))))

(define interpret
  (lambda (parsed-file)
    (m_func_read parsed-file (new_state) #f (lambda (state) 
                                              ; (arg arg_list state term return excep cont break)
                                              (M_f_call '(funcall main) '() state
                                                        (lambda (state) (error "main() code did not return")) 
                                                        (lambda (val state)
                                                          (pretify val))
                                                        (lambda (excep state) (error "main() code throws exception")) 
                                                        (lambda (cont_state) (error "main() code attempts to continue outside of loop"))
                                                        (lambda (break_state) (error "main() code attempts to break outside of loop")))))))

(define interpret2
  (lambda (parsed-file)
    (m_expr_test parsed-file (new_state) #f)))

(define _run-interpreter
  (lambda (raw-file)
    (interpret (parser raw-file))))

(define _i
  (lambda (file debug)
    (cond
      (debug (interpret-debug (parser file)))
      (else (interpret (parser file))))))

(define _i2
  (lambda (file debug)
    (cond
      (debug (interpret2 (parser file)))
      (else (interpret2 (parser file))))))

(define m_func_read
  (lambda (l state debug return)
    (cond
      ((null? l) (return state))
      ;             (arg arg_list state term return excep cont break)
      (else (M_func (car l) (cdr l) state
                    (lambda (state) (return state)) 
                    (lambda (val state) (error "m_func_read: Code attempted to return a value"))
                    (lambda (excep state) (error "m_func read: Code throws exception")) 
                    (lambda (cont_state) (error "m_func read: Code attempts to continue outside of loop"))
                    (lambda (break_state) (error "m_func read: Code attempts to break outside of loop")))))))

(define m_expr_test
  (lambda (l state debug)
    (cond
      ((null? l) (error "Null input to m_expr_test interpreter."))
      (else (M_expr (car l) (cdr l) state 
                     (lambda (state) (error "code did not return")) 
                     (lambda (val state)
                       (cond
                         (debug (begin (display state)) (display "\n") (pretify val))
                         (else (pretify val)))) 
                     (lambda (excep state) (error "Code throws exception")) 
                     (lambda (cont_state) (error "Code attempts to continue outside of loop"))
                     (lambda (break_state) (error "Code attempts to break outside of loop")))))))


; ====== START INTERPRETER BODY ======

; ====== UTILTIES ======

(define new_state
  (lambda ()
    ; ((names) (objs))
    (list (new_layer))))

(define new_layer
  (lambda ()
    '(() ())))

(define add_layer
  (lambda (layer state)
    (cons layer state)))

(define remove_layer
  (lambda (state)
    (cond
      ((null? state) (error "remove_layer: cannot remove layer from empty state"))
      (else (cdr state)))))

(define is_valid_state?
  (lambda (state)
    (cond
      ((null? state) #f)
      (else (lol state (lambda (v) v))))))

(define lol ; list of layers
  (lambda (l return)
    (cond
      ((null? l) (return #t))
      ((not (list? l)) (return #f))
      ((not (list? (car l))) (return #f))
      ((not (eq? (length (car l)) '2)) (return #f))
      (else (lol (cdr l) return)))))

(define split_state
  (lambda (state return)
    (cond
      ((null? state) (return '() '()))
      ((not (is_valid_state? state)) (error "split_state: invalid state"))
      (else (split_state (cdr state) (lambda (vars vals)
                                       (return (cons (car (car state)) vars) (cons (cadr (car state)) vals))))))))

(define merge_state
  (lambda (vars vals return)
    (cond
      ((not (and (list? vars) (list? vals))) (error "merge_state: improperly formatted input"))
      ((and (null? vars) (null? vals)) (return '()))
      ((eq? (length vars) (length vals)) (merge_state (cdr vars) (cdr vals) (lambda (state) (return (cons (list (car vars) (car vals)) state)))))
      (else (error "merge_state: vars and vals not equal")))))
    
(define pretify
  (lambda (value)
    (cond
      ((eq? value #t) "Truth")
      ((eq? value #f) "Naht Truth")
      (else value))))

; ====== M state ======
; defines the state transition for the code

; ====== M Class ======
; return the state given a class definition

; ====== M Func ======
; return the state, value of the function execution
; M_func processes the outer layer of variable declarations, assignments and function defs

(define M_func
  (lambda (arg arg_list state term return excep cont break)
    (cond
      ((null? arg) (error "M_func: argument is null"))
      ((not (is_valid_state? state)) (error "M_func: invalid state"))
      ((is_declare? arg) (M_e_declare arg arg_list state term return excep cont break))
      ((is_assign? arg) (M_e_assign arg arg_list state term return excep cont break))
      ((is_func_def? arg) (M_f_def arg arg_list state term return excep cont break))
      (else (display "\nstate\n") 
            (display state) 
            (display "\narg\n") 
            (display arg)
            (display "\n") 
            (error "^ The above part of M_func is not defined")))))

; === M func pieces ===

(define is_func_def?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      (else (and (eq? (length arg) '4) (eq? (car arg) 'function))))))

(define M_f_def
  (lambda (arg arg_list state term return excep cont break)
    (func_to_def arg state (lambda (f_def)
                               (cond
                                 ((pair? arg_list) (M_expr (car arg_list) (cdr arg_list) (assign_var (cadr arg) f_def (create_var (cadr arg) state)) term return excep cont break))
                                 (else (term (assign_var (cadr arg) f_def (create_var (cadr arg) state)))))))))

(define is_func_call?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      (else (and (>= (length arg) '2) (eq? (car arg) 'funcall))))))

(define M_f_call
  (lambda (arg arg_list state term return excep cont break)
    (funcall (cadr arg) (cddr arg) (add_layer (new_layer) state) 
             (lambda (term_state) (term (remove_layer term_state)))
             (lambda (val state) (return val (remove_layer state)))
             (lambda (exception state) (excep exception (remove_layer state)))
             (lambda (cont_state) (error "Can't call continue inside a function"))
             (lambda (break_state) (error "Can't call break inside a function")))))

; === M func utilities ===
(define func_to_def
  (lambda (arg state return)
    (make_closure (cadr arg) state (lambda (closure)
                                     (return (list 'function (caddr arg) (cadddr arg) closure))))))

(define funcall
  (lambda (name arguments state term return excep cont break)
    (fc_h (cadr (find_var name state)) arguments (cadddr (find_var name state)) (caddr (find_var name state)) state 
          (lambda (state1)
            (term state1))
          (lambda (val state2)
            (return val state2))
          excep cont break)))

(define fc_h
  (lambda (arg_vars arg_vals closure arg_list state term return excep cont break)
    (bind_args arg_vars arg_vals state (operate_closure closure state)
               (lambda (state1) ; once the args are bound
                 (M_expr (car arg_list) (cdr arg_list) state1 term return excep cont break)) excep)))
    
(define make_closure
  (lambda (f_name state return)
    (return (lambda (current_state)
              (assign_var f_name (find_var f_name current_state) (create_var f_name state))))))

(define operate_closure
  (lambda (closure state)
    (closure state)))

(define bind_args
  (lambda (arg_vars arg_vals val_state f_state return excep)
    (cond
      ((not (and (list? arg_vars) (list? arg_vals))) (error "bind_args: improperly formatted lists"))
      ((not (eq? (length arg_vars) (length arg_vals))) (error "bind_args: length arg_vars does not equal arg_vals"))
      ((and (null? arg_vars) (null? arg_vals)) (return f_state))
      ; note: pass by reference here is just passing in the variable name instead of the M_value below
      (else (M_value (car arg_vals) val_state (lambda (val state1) 
                                                (return (bind_args (cdr arg_vars) 
                                                                   (cdr arg_vals) 
                                                                   state1 
                                                                   f_state
                                                                   (lambda (state2)
                                                                     (assign_var (car arg_vars) val (create_var (car arg_vars) state2))) 
                                                                   excep)))
                     excep)))))

(define caddddr
  (lambda (arg)
    (begin
      (display arg)
      (car (cdr (cdddr arg))))))
    
; ====== M Expression ======
; returns the state modified by the given expression (line[ish] of code)
; This is for use within functions, etc
; Continuations: 
; term - called at the end of the evaluation if nothing else occurs (term state)
; return - called if return is called (return value state)
; excep - called if exception is thrown (excep value state)
; cont - called in loop to return to top (cont state)
; break - called in loop to exit loop (break state)

(define M_expr
  (lambda (arg arg_list state term return excep cont break)
    (cond
      ((null? arg) (error "M_expr: argument is null"))
      ((not (is_valid_state? state)) (error "M_expr: invalid state"))
      ((is_return? arg) (M_e_return arg arg_list state term return excep cont break))
      ((is_declare? arg) (M_e_declare arg arg_list state term return excep cont break))
      ((is_assign? arg) (M_e_assign arg arg_list state term return excep cont break))
      ((is_if? arg) (M_e_if arg arg_list state term return excep cont break))
      ((is_while? arg) (M_e_while arg arg_list state term return excep cont break))
      ((is_block? arg) (M_e_block arg arg_list state term return excep cont break))
      ((is_break? arg) (M_e_break arg arg_list state term return excep cont break))
      ((is_continue? arg) (M_e_continue arg arg_list state term return excep cont break))
      ((is_func_def? arg) (M_f_def arg arg_list state term return excep cont break))
      ((is_func_call? arg) (M_f_call arg arg_list state term return excep cont break))
      (else (display "\nstate\n") 
            (display state) 
            (display "\narg\n") 
            (display arg)
            (display "\n") 
            (error "^ The above part of M_expr is not defined")))))

; === M state pieces ===

(define is_return?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (eq? (length arg) '2)) #f)
      (else (eq? 'return (car arg))))))

(define M_e_return
  (lambda (arg arg_list state term return excep cont break)
    (M_value (cadr arg) state (lambda (val state) (return val state)) excep)))

(define is_declare?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((> (length arg) 3) #f)
      ((< (length arg) 2) #f)
      (else (eq? 'var (car arg))))))

(define M_e_declare
  (lambda (arg arg_list state term return excep cont break)
    (cond
      ((eq? (length arg) 2) (M_expr (car arg_list) (cdr arg_list) (create_var (cadr arg) state) term return excep cont break))
      (else (M_e_assign (list '= (cadr arg) (caddr arg)) arg_list (create_var (cadr arg) state) term return excep cont break)))))

(define is_assign?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      (else (eq? '= (car arg))))))

(define M_e_assign
  (lambda (arg arg_list state term return excep cont break)
    (M_value (caddr arg) state (lambda (val state1) 
                                 (cond
                                   ((pair? arg_list) (M_expr (car arg_list) (cdr arg_list) (assign_var (cadr arg) val state1) term return excep cont break))
                                   (else (term (assign_var (cadr arg) val state1)))))
             excep)))

(define is_continue?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((pair? arg) (eq? (car arg) 'continue))
      (else #f))))

(define M_e_continue
  (lambda (arg arg_list state term return excep cont break)
    (cont state)))

(define is_break?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((pair? arg) (eq? (car arg) 'break))
      (else #f))))

(define M_e_break
  (lambda (arg arg_list state term return excep cont break)
    (break state)))

;(if <condition> <then> <else>)
;(car cadr caddr cadddr)
(define is_if?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((> (length arg) 4) #f)
      ((< (length arg) 3) #f)
      (else (eq? 'if (car arg))))))

(define M_e_if
  (lambda (arg arg_list state term return excep cont break)
    (M_bool (cadr arg) state (lambda (condition state)
                               (cond
                                 (condition (M_expr (caddr arg) arg_list state term return excep cont break))
                                 ((eq? (length arg) '4) (M_expr (cadddr arg) arg_list state term return excep cont break))
                                 ((pair? arg_list) (M_expr (car arg_list) (cdr arg_list) state term return excep cont break))
                                 (else (term state)))) 
            excep)))

(define is_while?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      ((not (eq? (length arg) '3)) #f)
      (else (eq? 'while (car arg))))))

(define M_e_while
  (lambda (arg arg_list state term return excep cont break)
    (M_bool (cadr arg) state (lambda (condition state)
                               (cond
                                 (condition (M_expr (caddr arg) (cons arg arg_list) state term return excep 
                                                     (lambda (cont_state)
                                                       (M_e_while arg arg_list cont_state term return excep cont break))
                                                     (lambda (break_state)
                                                       (M_expr (car arg_list) (cdr arg_list) break_state term return excep cont break))))
                                 (else (M_expr (car arg_list) (cdr arg_list) state term return excep cont break))))
            excep)))

(define is_block?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      ((<= (length arg) 1) #f)
      (else (eq? 'begin (car arg))))))

(define M_e_block
  (lambda (arg arg_list state term return excep cont break)
    (M_expr (cadr arg) (cddr arg) (add_layer (new_layer) state) 
             (lambda (state1)
               (cond
                 ((pair? arg_list) (M_expr (car arg_list) (cdr arg_list) (remove_layer state1) term return excep cont break))
                 (else (term (remove_layer state1)))))
             (lambda (ret_val state)
               (return ret_val (remove_layer state)))
             (lambda (exception state)
               (excep exception (remove_layer state)))
             (lambda (cont_state)
               (cont (remove_layer cont_state)))
             (lambda (break_state)
               (break (remove_layer break_state))))))
                                  

; === M_expr utils ===
      
(define create_var
  (lambda (name state)
    (cond
      ((unique? name state) (split_state state (lambda (vars vals) 
                                                 (create name vars vals (lambda (state_with_var) state_with_var)))))
      (else (error "Variable already declared. Shouldn't redefine.\ncreate_var: variable name already defined")))))

(define create ; helper to create_var
  (lambda (name var_list val_list return)
    (merge_state (cons (cons name (car var_list)) (cdr var_list))
                 (cons (cons 'notDefined (car val_list)) (cdr val_list))
                 (lambda (state) (return state)))))

(define unique?
  (lambda (name state)
    (cond
      ((not (is_valid_state? state)) (error "unique?: invalid state"))
      (else (uni_in_layer? name (car state))))))

(define uni_in_layer?
  (lambda (name layer)
    (not (foldl (lambda (a b) (or a b)) #f (map (lambda (e) (eq? e name)) (car layer))))))
    

(define assign_var ; method is the programmer facing. Splits state, and then rebuilds state
  (lambda (name val state)
    (split_state state (lambda (vars vals) 
                         (assign name val vars vals (lambda (vars vals)
                                                      (merge_state vars vals (lambda (state) state))))))))
     
(define assign
  (lambda (name val var_list val_list return)
    (cond
      ((or (null? var_list) (null? val_list)) (display name) (error "Variable assignment before declaration\nassign: variable not yet initialized"))
      ((or (not (list? var_list)) (not (list? val_list))) (error "assign: Malformed var or val list"))
      ((not (and (list? (car var_list)) (list? (car val_list)))) (error "assign: Malformed var or val list (element not a list)"))
      (else (try_assign_layer name val (car var_list) (car val_list) (lambda (set vars vals) ; returned from setting in layer
                                                                       (cond
                                                                         (set (return (cons vars (cdr var_list)) (cons vals (cdr val_list))))
                                                                         (else (assign name val (cdr var_list) (cdr val_list) (lambda (var_list2 val_list2) ; returned from assign rest
                                                                                                                                (return (cons (car var_list) var_list2) (cons (car val_list) val_list2))))))))))))

(define try_assign_layer
  (lambda (name val var_layer val_layer return)
    (cond
      ((or (null? var_layer) (null? val_layer)) (return #f '() '()))
      ((eq? (car var_layer) name) (return #t var_layer (cons val (cdr val_layer))))
      (else (try_assign_layer name val (cdr var_layer) (cdr val_layer) (lambda (set vars vals)
                                                                         (return set (cons (car var_layer) vars) (cons (car val_layer) vals))))))))

(define find_var
  (lambda (name state)
    (split_state state (lambda (vars vals)
                         (find name vars vals (lambda (val)
                                                (cond
                                                  ((eq? val 'notDefined) (error "Variable access before assignment\nfind_var: variable not yet assigned"))
                                                  (else val))))))))

(define find ; helper to find_var
  (lambda (name var_list val_list return)
    (cond
      ((or (null? var_list) (null? val_list)) (display name) (error "Variable access before declaration\nfind_var: variable not yet declared"))
      (else (try_find_layer name (car var_list) (car val_list) (lambda (found value)
                                                                 (cond
                                                                   (found (return value))
                                                                   (else (find name (cdr var_list) (cdr val_list) return)))))))))

(define try_find_layer
  (lambda (name var_layer val_layer return)
    (cond
      ((or (null? var_layer) (null? val_layer)) (return #f '()))
      ((eq? (car var_layer) name) (return #t (car val_layer)))
      (else (try_find_layer name (cdr var_layer) (cdr val_layer) return)))))

; ====== M Value ======
; evaluate 
(define M_value
  (lambda (arg state return excep) ; Note: return takes (value state)
    (cond
      ((null? arg) (error "M_value: null arg"))
      ((integer? arg) (return arg state))
      ((is_math? arg) (M_v_math arg state return excep))
      ((is_comparison? arg) (M_v_comparison arg state return excep))
      ((is_bool_op? arg) (M_v_bool_op arg state return excep))
      ((or (eq? 'true arg) (eq? #t arg)) (return #t state))
      ((or (eq? 'false arg) (eq? #f arg)) (return #f state))
      ((is_assign? arg) (M_v_assign arg state return excep))
      ((is_func_call? arg) (M_v_funcall arg state return excep))
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
  (lambda (arg state return excep)
    (cond
      ((op? '% '2 arg) (op_2 remainder (cadr arg) (caddr arg) state return excep))
      ((op? '/ '2 arg) (op_2 quotient (cadr arg) (caddr arg) state return excep))
      ((eq? '3 (length arg)) (op_2 (eval (car arg)) (cadr arg) (caddr arg) state return excep))
      ((eq? '2 (length arg)) (op_1 (eval (car arg)) (cadr arg) state return excep))
      (else (display arg) (error "M_v_math: this operator not yet implemented")))))

(define is_bool_op?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      ((> (length arg) 3) #f)
      ((< (length arg) 2) #f)
      ((op? '! 1 arg) #t)
      (else (match_list arg '(&& ||))))))

(define M_v_bool_op
  (lambda (arg state return excep)
    (cond
      ((op? '! '1 arg) (op_1 (lambda (a) (not a)) (cadr arg) state return excep))
      ((op? '&& '2 arg) (op_2 (lambda (a b) (and a b)) (cadr arg) (caddr arg) state return excep))
      ((op? '|| '2 arg) (op_2 (lambda (a b) (or a b)) (cadr arg) (caddr arg) state return excep))
      (else (display arg) (error "M_v_bool_op?: this operator not yet implemented")))))

(define is_comparison?
  (lambda (arg)
    (cond
      ((null? arg) #f)
      ((not (list? arg)) #f)
      ((> (length arg) 3) #f)
      ((< (length arg) 2) #f)
      (else (match_list arg '(< <= == != >= >))))))

(define M_v_comparison
  (lambda (arg state return excep)
    (cond
      ;((op? '!= '2 arg) (op_2 (lambda (a b) (not (eq? a b))) (cadr arg) (caddr arg) state return))
      ;((op? '== '2 arg) (op_2 (lambda (a b) (eq? a b)) (cadr arg) (caddr arg) state return))
      ((eq? '3 (length arg)) (op_2 (eval (car arg)) (cadr arg) (caddr arg) state return excep))
      (else (display arg) (error "M_v_comparison: this operator not yet implemented")))))

(define ==
  (lambda (a b)
    (eq? a b)))

(define !=
  (lambda (a b)
    (not (eq? a b))))

(define M_v_assign
  (lambda (arg state return excep)
    (M_value (caddr arg) state (lambda (val state1)
                                 (return val (assign_var (cadr arg) val state1))) excep)))

(define M_v_funcall
  (lambda (arg state return excep)
    (funcall (cadr arg) (cddr arg) (add_layer (new_layer) state) 
             (lambda (term_state) (error "function as expression did not return a value"))
             (lambda (val state) (return val (remove_layer state)))
             (lambda (exception state) (excep exception (remove_layer state)))
             (lambda (cont_state) (error "Can't call continue inside a function"))
             (lambda (break_state) (error "Can't call break inside a function")))))

; === M value utlities ===

(define op?
  (lambda (operator num_args input)
    (cond
      ((null? input) #f)
      ((not (eq? (+ num_args 1) (length input))) #f)
      (else (eq? operator (car input))))))

(define op_2
  (lambda (operation left_arg right_arg state0 return excep)
    (M_value left_arg state0 (lambda (val_left state1) 
                               (M_value right_arg state1 (lambda (val_right state2)
                                                           (return (operation val_left val_right) state2)) excep)) excep)))

(define op_1
  (lambda (operation arg state0 return excep)
    (M_value arg state0 (lambda (val_arg state1) (return (operation val_arg) state1)) excep)))

(define match_list
  (lambda (arg list)
    (foldl (lambda (a b) (or a (eval b))) #f (map (lambda (operator) (op? operator 2 arg)) list))))

; ====== M boolean ======
; evaluate conditional (while, for, if that kind of thing)

(define M_bool
  (lambda (arg state return excep)
    (cond
      ((null? arg) (error "M_bool: null arg"))
      ((or (eq? #t arg) (eq? 'true arg)) (return #t state))
      ((or (eq? #f arg) (eq? 'false arg)) (return #f state))
      (else (M_value arg state (lambda (v state) 
                                 (cond
                                   ((integer? v) (error "M_bool: condition evaluated to integer"))
                                   (else (return v state)))) excep)))))


; ====== TESTING ======
(define test
  (lambda ()
    (begin
      (load "interpreter.test3.wb.scm")
      (_test))))

(define test2
  (lambda ()
    (begin
      (load "interpreter.test2.wb.scm")
      (_test))))