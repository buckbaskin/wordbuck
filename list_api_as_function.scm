(define new_list
  (lambda ()
    '()))
; add(E e)
(define add
  (lambda (list element return)
    (return (cons element list) 't)))

; add(int index, E e)
(define add_index
  (lambda (list index element return)
    (cond
      ((<= index 0) (return (cons element list)))
      ((null? list) (return '()))
      (else (add_index (cdr list) (- index 1) element (lambda (list2) (return (cons (car list) list2))))))))
  
; addAll(Collection<E> c)
(define add_all
  (lambda (list collection return)
    (cond
      ((null? collection) (return list))
      (else (add_all list (cdr collection) (lambda (l2) (return (cons (car collection) l2))))))))

; addAll(int index, Collection<E> c)
(define add_all_index
  (lambda (list index collection return)
    (cond
      ((<= index 0) (add_all list collection (lambda (l2) (return l2))))
      ((null? list) (return '()))
      (else (add_all_index (cdr list) (- index 1) collection (lambda (list2) (return (cons (car list) list2))))))))

; clear()
(define clear
  (lambda (list)
    '()))

; contains(Object o)
(define contains
  (lambda (list object)
    (foldl (lambda (a b) (or a b)) 'f (map (lambda (n) (eq? object n)) list))))