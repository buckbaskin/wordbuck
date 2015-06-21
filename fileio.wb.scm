; READ IN FILE

(define read_in_file
  (lambda (raw-file)
    (let ((p (open-input-file raw-file))) ; p is input port
      (read-file p))))
      
(define read-file
  (lambda (port)
    (cond
      ((eof-object? (peek-char port)) (begin (close-input-port port) '()))
      (else (cons (read-by-line port) (read-file port))))))

(define read-by-line
  (lambda (port)
    (cond
      ((eof-object? (peek-char port)) '())
      ((eq? (peek-char port) #\newline) (begin (read-char port) '()))
      ((eq? (peek-char port) #\#) (begin (consume-to-newline port) '())) ; # is the comment character
      (else (cons (schemeize (combine-chars port)) (read-by-line port))))))

(define consume-to-newline
  (lambda (port)
    (cond
      ((eof-object? (peek-char port)) '())
      ((eq? (peek-char port) #\newline) (begin (read-char port) '()))
      (else (begin (read-char port) (consume-to-newline port))))))
      
(define combine-chars
  (lambda (port)
    (cond
      ((eof-object? (peek-char port)) '())
      ((eq? (peek-char port) #\newline) '())
      ((eq? (peek-char port) #\space) (begin (read-char port) '()))
      (else (cons (read-char port) (combine-chars port))))))

(define schemeize
  (lambda (chars)
    (cond
      ((not (string->number (list->string chars))) (string->symbol (list->string chars)))
      (else (string->number (list->string chars))))))