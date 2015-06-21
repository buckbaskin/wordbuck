(load "fileio.wb.scm")

; PARSER
; Parser takes in a raw-file, reads it in by line
;  then it goes through and checks the syntax/grammar
; in -> raw file
; out-> valid parse tree OR error for invalid syntax

(define parse
  (lambda (raw-file)
    (begin
      (display "parse raw file \n")
      (display (read_in_file raw-file))
      (display "\n")
      (error "parse not yet implmented.")
      #t)))

(define _run-parser
  (lambda (raw-file)
    (parse raw-file)))