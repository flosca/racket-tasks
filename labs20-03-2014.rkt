#lang racket

; #1

; Удалить из текстового файла ненужные пробелы.
; Если пробелы стоят перед строкой, то оставить.
(define (not-space? x)
  (not (eq? x #\space)))

(define (reduce str)
  (if (eq? "" str) ""
      (let loop ((lst (string->list str))
                 (i 0))
        (if (null? (filter not-space? (string->list str)))
                      str 
                     (if (not-space? (car lst))
   (let* ([after-sp  (substring str i (string-length str))]
          [rest-str  (apply string-append (map (λ (x) (string-append x " ")) (string-split after-sp)))]
          [whole-str (string-append (substring str 0 i) rest-str)])
                            (substring whole-str 0 (- (string-length whole-str) 1))) ;cutting the space in last word
                            (loop (cdr lst) (+ i 1)))))))

;for files
(define (reduce-spaces input output)
  (define in (open-input-file input))
  
  (define (write-out str)
    (let ((out (open-output-file output #:exists 'append)))
      (displayln (reduce str) out)
      (close-output-port out)))
  
  
  (define (iter line)
    (define str (read-line in 'any))
    (if (eq? str eof) null
        (begin (write-out str)
               (iter line))))
(begin  
(define out (open-output-file output #:exists 'truncate))
(display "" out)
(iter in)
(close-input-port in)))


; #2


(define (reduce-lists lists)
  (define (letter lst res)
    (if (null? lst) res
         (if (<= (string-length (car lst)) 1) (letter (cdr lst) res)
                (letter (cdr lst) (cons (car lst) res)))))
  (letter lists null))


(define (gimme-words str)
      (sort (flatten (map reduce-lists 
                          (map (λ (a) (string-split a "*"))  (string-split str)))) string<?))


(define (transpose-cr crossword)
  (foldl string-append "" 
         (map (λ (s) (string-append s "\n"))
              (map list->string (apply map list (map string->list (string-split crossword)))))))

;main
(define (take-words-from-crossword filename)
  (define input  (file->string filename))
  (define output (open-output-file "answers.txt" #:exists 'replace))
  
  
  (begin
    (display "По горизонтали:  " output)
    (display (gimme-words input) output)
    (displayln " " output)
    (display "По вертикали:  " output)
    (display (gimme-words (transpose-cr input)) output))
  (close-output-port output))

#| example of a input file:

*******w**
***hello**
****g**r**
*frog**lie
*o*r***d**
*o*data***
|#
