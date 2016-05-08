(define interpret
  (lambda (code)
    (evolve code (collect_rules))))

(define evolve
  (lambda (code rules)
    (apply_rule code rules (lambda (v) v))))

(define add_rule2
  (cons (lambda (args)
          (cond
            ((null? args) #f) 
            ((not (list? args)) #f)
            ((not (eq? (length args) '3)) #f)
            (else (eq? (car args) '+))))
        (cons (lambda (code)
                (+ (interpret (cadr code)) (interpret (caddr code))))
              '())))
  
(define collect_rules
  (lambda ()
    (list add_rule2)))

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


(interpret '(+ 1 (+ 2 3)))