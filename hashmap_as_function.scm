(define base_dict
  (lambda (key)
    '()))

(define new_dict
  (lambda ()
    base_dict))

(define len
  (lambda (a)
    (cond
      ((number? a) a)
      ((string? a) (string-length a))
      ((list? a) (length a))
      (else '0))))

(define hash_function
  (lambda (key)
    (len key)))

(define add_element
  (lambda (key value dict)
    (lambda (k)
      (cond
        ((eq? (hash_function k) (hash_function key)) 
         (cond
           ((eq? k key) (cons (list value) (dict key)))
           (else
            (dict k))))
        (else (dict k))))))

(define remove_element
  (lambda (key dict)
    (lambda (k)
      (cond
        ((not (eq? (hash_function k) (hash_function key))) (dict k))
        (else '())))))

(define contains
  (lambda (key dict)
    (> (length (key dict)) 0)))