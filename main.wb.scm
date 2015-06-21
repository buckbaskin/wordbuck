(load "parser.wb.scm")
(load "interpreter.wb.scm")

; MAIN file for running the wordbuck language

(define main
  (lambda (file)
    (clean (interpret (parse file)))))

(define clean
  (lambda (output)
    (cond
      ((eq? output #t) "True")
      ((eq? output #f) "False")
      (else output))))