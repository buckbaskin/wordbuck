(load "fileio.wb.scm")

; PARSER
; Parser takes in a raw-file, reads it in by line
;  then it goes through and checks the syntax/grammar
; in -> raw file
; out-> valid parse tree OR error for invalid syntax

(define parse
  (lambda (raw-file)
    (begin
      (cond
        (display raw-file)
        ((null? raw-file) (error "empty input file"))
        (else (_parse raw-file '(root) (lambda (v) v)))))))
    
(define _parse
  (lambda (raw-file tree return)
    (cond
      ((null? raw-file) (return '()))
      (else (_parse (cdr raw-file) (parse-one (car raw-file) tree return) (lambda (rest) (return (cons (car tree) rest))))))))
    
(define parse-one
  (lambda (line tree return)
    (cond
      ((null? line) (return '()))
      ((eq? (car line) 'return) (parse-one (cdr line) tree (lambda (rest) (list 'return (parse-expression (cdr line) (lambda (v) v)) rest)))) 
      (else (error "parse-one: not implemented")))))

(define parse-expr
  (lambda (line)
    (parse-expression line (lambda (v) v))))

(define parse-expression
  (lambda (line return)
    (cond
      ((null? line) (return '()))
      ((eq? (length line) '1) (return line))
      ((eq? (length line) '2) (return line))
      (else (parse-expression (cddr line) (lambda (rest) (append (list (operator line)) (append (list (car line)) rest))))))))

(define _run-parser
  (lambda (raw-file)
    (parse (read_in_file raw-file))))

(define _parse-expression
  (lambda (raw-file)
    (map parse-expr (read_in_file raw-file))))

(define operator
  (lambda (line)
    (cadr line)))