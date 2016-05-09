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
        (lambda (code)
          code)))

;(define rule_notlist
;  (list (lambda (code)
;          (not (list? code)))
;        (lambda (code)
;          code))

(define rule_add2
  (list (lambda (code)
          (cond
            ((null? code) #f) 
            ((not (list? code)) #f)
            ((not (eq? (length code) '3)) #f)
            (else (eq? (car code) '+))))
        (lambda (code)
          ((lambda (a b)
             (cond
               ((and (number? a) (number? b)) (+ a b))
               (else (raise "can't add non-numbers")))) (interpret (cadr code)) (interpret (caddr code))))))

(define rule_addn
  (list (lambda (code)
          (cond
            ((null? code) #f)
            ((not (list? code)) #f)
            (else (eq? (car code) '+))))
        (lambda (code)
          (cons '+ (cons (interpret (list '+ (cadr code) (caddr code))) (cdddr code))))))

(define rule_multiply2
  (list (lambda (code)
          (cond
            ((null? code) #f) 
            ((not (list? code)) #f)
            ((not (eq? (length code) '3)) #f)
            (else (eq? (car code) '*))))
        (lambda (code)
          ((lambda (a b)
             (cond
               ((and (number? a) (number? b)) (* a b))
               (else (raise "can't multiply non-numbers")))) (interpret (cadr code)) (interpret (caddr code))))))

(define rule_multiplyn
  (list (lambda (code)
          (cond
            ((null? code) #f)
            ((not (list? code)) #f)
            (else (eq? (car code) '*))))
        (lambda (code)
          (cons '* (cons (interpret (list '* (cadr code) (caddr code))) (cdddr code))))))

(define rule_divide2
  (list (lambda (code)
          (cond
            ((null? code) #f) 
            ((not (list? code)) #f)
            ((not (eq? (length code) '3)) #f)
            (else (eq? (car code) '/))))
        (lambda (code)
          ((lambda (a b)
             (cond
               ((and (number? a) (number? b)) (/ a b))
               (else (raise "can't multiply non-numbers")))) (interpret (cadr code)) (interpret (caddr code))))))

(define rule_dividen
  (list (lambda (code)
          (cond
            ((null? code) #f)
            ((not (list? code)) #f)
            (else (eq? (car code) '/))))
        (lambda (code)
          (cons '/ (cons (interpret (list '/ (cadr code) (caddr code))) (cdddr code))))))

(define rule_subtract1
  (list (lambda (code)
          (cond
            ((null? code) #f)
            ((not (list? code)) #f)
            ((not (eq? (length code) 2)) #f)
            (else (eq? (car code) '-))))
        (lambda (code)
          (cons '- (cons '0 (cdr code))))))

(define rule_subtract2
  (list (lambda (code)
          (cond
            ((null? code) #f)
            ((not (list? code)) #f)
            ((not (eq? (length code) 3)) #f)
            (else (eq? (car code) '-))))
        (lambda (code)
          (interpret (list '+ (cadr code) (- (caddr code)))))))

(define rule_subtractn
  (list (lambda (code)
          (cond
            ((null? code) #f)
            ((not (list? code)) #f)
            (else (eq? (car code) '-))))
        (lambda (code)
          (cons '- (cons (interpret (list '+ (cadr code) (- (caddr code)))) (cdddr code))))))

(define rule_not
  (list (lambda (code)
          (cond
            ((not (list? code)) #f)
            ((not (eq? (length code) 2)) #f)
            (else (eq? (car code) 'not))))
        (lambda (code)
          (cond
            ((interpret (cadr code)) #f)
            (else #t)))))

(define rule_or2
  (list (lambda (code)
          (cond
            ((not (list? code)) #f)
            ((not (eq? (length code) 3)) #f)
            (else (eq? (car code) 'or))))
        (lambda (code)
          (cond
            ((interpret (cadr code)) #t)
            (else (interpret (caddr code)))))))

(define rule_orn
  (list (lambda (code)
          (cond
            ((not (list? code)) #f)
            (else (eq? (car code) 'or))))
        (lambda (code)
          (cond
            ((interpret (cadr code)) #t)
            (else (cons 'or (cddr code)))))))

(define collect_rules
  (lambda ()
    (list rule_nullcode
          rule_not
          rule_or2
          rule_orn
          rule_multiply2
          rule_multiplyn
          rule_divide2
          rule_dividen
          rule_add2
          rule_addn
          rule_subtract2
          rule_subtract1
          rule_subtractn)))

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
      (((first_condition rules) code) (cont ((first_operation rules) code)))
      (else (apply_rule code (cdr rules) cont)))))


(interpret '(or #t #f #f #f))