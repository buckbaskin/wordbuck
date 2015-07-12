(load "parser_level1/simpleParser.scm")

; INTERPRETER
; Interpreter takes in a parsed file, and evaluates the
;  main method. 
; NOTE: intermediary steps before final version may execute
;  diferent portions of code than just a main method
; in -> parse tree with main method
; out-> program output

(define interpret
  (lambda (parsed-file)
    (begin
      (display ("parsed file \n"))
      (display (parsed-file))
      (error "interpret not yet implemented. \n")
      #t)))

(define _run-interpreter
  (lambda (raw-file)
    (interpret (parser raw-file))))