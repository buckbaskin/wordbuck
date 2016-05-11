; up next? variables! and cps!

(define interpret
  (lambda (code)
    (evolve code (collect_rules))))

(define evolve
  (lambda (code rules)
    (apply_rule code rules (lambda (new_code)
                             (cond
                               ((null? new_code) new_code)
                               ((deep_equals code new_code) new_code)
                               (else (interpret new_code)))))))

(define deep_equals
  (lambda (v1 v2)
    (cond
      ((and (null? v1) (null? v2)) #t)
      ((or (null? v1) (null? v2)) #f)
      ((and (list? v1) (list? v2)) (and (deep_equals (car v1) (car v2)) (deep_equals (cdr v1) (cdr v2))))
      ((or (null? v1) (null? v2)) #f)
      (else (eq? v1 v2)))))

;===============
; COLLECT RULES
;===============
(define rule_nullcode
  (list (lambda (code)
          (null? code))
        (lambda (code cont)
          (cont code))))

(define rule_notlist
  (list (lambda (code)
          (not (list? code)))
        (lambda (code cont)
          (cont code))))

(define rule_listoflist
  (list (lambda (code)
          (list? (car code)))
        (lambda (code cont)
          (cont (cons (evolve (car code) (collect_rules)) (rule_listoflist (cdr code)))))))

(define rule_add2
  (list (lambda (code)
          (cond
            ((not (eq? (length code) '3)) #f)
            (else (eq? (car code) '+))))
        (lambda (code cont)
          (cont ((lambda (a b)
                   (cond
                     ((and (number? a) (number? b)) (+ a b))
                     (else (raise "can't add non-numbers 1"))))
                 (interpret (cadr code)) (interpret (caddr code)))))))

(define rule_add2cps
  (list (lambda (code)
          (cond
            ((not (eq? (length code) '3)) #f)
            (else (eq? (car code) '+))))
        (lambda (code cont)
          (apply_rule (cadr code) (collect_rules)
                      (lambda (left)
                        (apply_rule (caddr code) (collect_rules)
                                    (lambda (right)
                                      (cond
                                        ((and (number? left) (number? right)) (cont (+ left right)))
                                        (else (raise "can't add non-numbers 2"))))))))))

(define rule_addn
  (list (lambda (code)
          (eq? (car code) '+))
        (lambda (code cont)
          (cont (cons '+ (cons (interpret (list '+ (cadr code) (caddr code))) (cdddr code)))))))

(define rule_addncps
  (list (lambda (code)
          (eq? (car code) '+))
        (lambda (code cont)
          (apply_rule (list '+ (cadr code) (caddr code)) (collect_rules)
                      (lambda (first)
                        (cont (cons '+ (cons first (cdddr code)))))))))

(define rule_multiply2
  (list (lambda (code)
          (cond
            ((not (eq? (length code) '3)) #f)
            (else (eq? (car code) '*))))
        (lambda (code cont)
          (cont ((lambda (a b)
             (cond
               ((and (number? a) (number? b)) (* a b))
               (else (raise "can't multiply non-numbers")))) (interpret (cadr code)) (interpret (caddr code)))))))

(define rule_multiplyn
  (list (lambda (code)
          (eq? (car code) '*))
        (lambda (code cont)
          (code (cons '* (cons (interpret (list '* (cadr code) (caddr code))) (cdddr code)))))))

(define rule_divide2
  (list (lambda (code)
          (cond
            ((not (eq? (length code) '3)) #f)
            (else (eq? (car code) '/))))
        (lambda (code)
          ((lambda (a b)
             (cond
               ((and (number? a) (number? b)) (/ a b))
               (else (raise "can't multiply non-numbers")))) (interpret (cadr code)) (interpret (caddr code))))))

(define rule_dividen
  (list (lambda (code)
          (eq? (car code) '/))
        (lambda (code)
          (cons '/ (cons (interpret (list '/ (cadr code) (caddr code))) (cdddr code))))))

(define rule_subtract1
  (list (lambda (code)
          (cond
            ((not (eq? (length code) 2)) #f)
            (else (eq? (car code) '-))))
        (lambda (code)
          (cons '- (cons '0 (cdr code))))))

(define rule_subtract2
  (list (lambda (code)
          (cond
            ((not (eq? (length code) 3)) #f)
            (else (eq? (car code) '-))))
        (lambda (code)
          (interpret (list '+ (cadr code) (- (caddr code)))))))

(define rule_subtractn
  (list (lambda (code)
          (eq? (car code) '-))
        (lambda (code)
          (cons '- (cons (interpret (list '+ (cadr code) (- (caddr code)))) (cdddr code))))))

(define rule_modulo
  (list (lambda (code)
          (eq? (length code) 3)
          (eq? (car code) '%))
        (lambda (code)
          (remainder (cadr code) (caddr code)))))

(define rule_not
  (list (lambda (code)
          (cond
            ((not (eq? (length code) 2)) #f)
            (else (eq? (car code) 'not))))
        (lambda (code)
          (cond
            ((interpret (cadr code)) #f)
            (else #t)))))

(define rule_or2
  (list (lambda (code)
          (cond
            ((not (eq? (length code) 3)) #f)
            (else (eq? (car code) 'or))))
        (lambda (code)
          (cond
            ((interpret (cadr code)) #t)
            (else (interpret (caddr code)))))))

(define rule_orn
  (list (lambda (code)
          (eq? (car code) 'or))
        (lambda (code)
          (cond
            ((interpret (cadr code)) #t)
            (else (cons 'or (cddr code)))))))

(define rule_and2
  (list (lambda (code)
          (cond
            ((not (eq? (length code) 3)) #f)
            (else (eq? (car code) 'and))))
        (lambda (code)
          (cond
            ((interpret (cadr code)) (interpret (caddr code)))
            (else #f)))))

(define rule_andn
  (list (lambda (code)
          (eq? (car code) 'and))
        (lambda (code)
          (cond
            ((interpret (cadr code)) (cons 'and (cddr code)))
            (else #f)))))

(define rule_cond
  (list (lambda (code)
          (eq? (car code) 'cond))
        (lambda (code)
          (cond
            ((eq? (car (cadr code)) 'else) (cadr (cadr code)))
            ((interpret (car (cadr code))) (cadr (cadr code)))
            (else (cons 'cond (cddr code)))))))

(define rule_if
  (list (lambda (code)
          (cond
            ((not (eq? (length code) 4)) #f)
            (else (eq? (car code) 'if))))
        (lambda (code)
          (cond
            ((interpret (cadr code)) (caddr code))
            (else (cadddr code))))))

(define collect_rules
  (lambda ()
    (list rule_nullcode
          rule_notlist
          rule_add2cps
          rule_addncps)))

(define first_condition
  (lambda (rules)
    (car (car rules))))

(define first_operation
  (lambda (rules)
    (car (cdr (car rules)))))

(define apply_rule
  (lambda (code rules cont)
    (cond
      ((null? rules) (cont code))
      (((first_condition rules) code) ((first_operation rules) code cont))
      (else (apply_rule code (cdr rules) cont)))))

;=============
; Run example
;=============

(interpret '(+ 1 2 3))