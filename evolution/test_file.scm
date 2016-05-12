(define rule_gen2 
  (lambda (operation)
    (list (lambda (code)
            (cond
              ((not (eq? (length code) 3)) #f)
              (else (eq? operation (car code)))))
          (lambda (code cont)
            (apply_rule (cadr code) (collect_rules)
                        (lambda (left)
                          (apply_rule (caddr code) (collect_rules)
                                      (lambda (right)
                                        (cond
                                          ((and (number? left) (number? right)) (cont (operation left right)))
                                          (else (raise "can't add non-numbers x")))))))))))

(rule_gen2 +)
((car (rule_gen2 +)) '(+ 1 2))
((cadr (rule_gen2 +)) '(+ 1 2) (lambda (v) v))