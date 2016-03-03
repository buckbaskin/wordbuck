(define insertion_sort
  (lambda (l)
    (insertion_c1 '() l (lambda (sorted) sorted))))

(define insertion_c1
  (lambda (front rest cont)
    (cond
      ((null? rest) (cont front))
      (else (find_min rest (lambda (min1 rest)
                             (my_append_intc front min1 (lambda (v) (insertion_c1 v rest cont)))))))))

; for a front rest pair of lists
; get the min in the rest list, cons it onto the front
(define find_min
  (lambda (rest cont)
    (cond
      ((not (pair? (cdr rest))) (cont (car rest) '()))
      (else (find_min (cdr rest) (lambda (old_min old_rest)
                                   (cond
                                     ((> old_min (car rest)) (cont (car rest) (cons old_min old_rest)))
                                     (else (cont old_min (cons (car rest) old_rest))))))))))

(define my_append_intc
  (lambda (l int c)
    (cond
      ((null? l) (c (cons int '())))
      (else (my_append_intc (cdr l) int (lambda (earlier) (c (cons (car l) earlier))))))))

(define my_append_int
  (lambda (l int) ; (l0 .. lN int)
    (cond
      ((null? l) (cons int '()))
      (else (cons (car l) (my_append_int (cdr l) int))))))