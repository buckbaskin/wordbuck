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
      ((eq? (car line) 'return) 

(define parse-expression
  (lambda (line return)
    (cond
      ((null? line) (return '()))
      ((eq? (length line) '1) (return line))
      (else (error "multiple element expression parse not yet implemented")))))

(define _run-parser
  (lambda (raw-file)
    (parse (read_in_file raw-file))))