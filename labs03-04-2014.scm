#lang racket

;#1
(define [polygon 
         input-filename output-filename k]
  
  (define (read-matr)
    (define input (open-input-file input-filename))
    (define vertices (read input))
    
    (build-list vertices (λ (x) (build-list 2 (λ (y) (read input))))))
    
  
  (define (scalar* k matr)
    (map (λ (a) (map (λ (x) (* k x)) a)) matr))
  
  (define answer (scalar* k [read-matr]))
  
  (define (write-out n)
    (define out (open-output-file output-filename #:exists 'replace))
    (displayln (length answer) out)
    (for-each (λ (x) (for-each (λ (y) (display y out) (display #\space out)) x) (displayln " " out)) answer) 
    (close-output-port out))
  
  (write-out output-filename))




; #2
(define [pascal->file
         k filename]
         
; Builds the Pascal's triangle {with k strings} and writes it into a text file.         
  
  (define [next line]
    (map + (cons 0 line)
         (append line (list 0))))
  
  (define (pascal-list num)
    (define (iter lst k)
      (if (= k -1) null
          (cons lst (iter (next lst) (sub1 k)))))
    (iter (list 1) num))
  
  
  (let ([triangle (pascal-list k)]
        [output (open-output-file filename #:exists 'replace)])
    
    (for-each (λ (line) 
                (for-each (λ (y) (display y   output)
                                 (display " " output)) line)
                (display "\n" output)) triangle) 
    (close-output-port output)))


; #3

(define [pref-suff 
         input-filename output-filename
         prefix suffix]
  
  (define (starts-with? substr str)
    (define (iter i)
      (if (> i (string-length substr)) #f
          (if (equal? (substring str 0 i) substr) #t
              (iter (+ i 1)))))
    (iter 1))
  
  (define (string-reverse str)
  (list->string (reverse (string->list str))))
  
  (define (filter-strings list)
    (filter (λ (x) (and [starts-with? prefix x]
                        [starts-with? (string-reverse suffix)
                                      (string-reverse x)])) list))
  
  (define (write-out smth)
    (define out (open-output-file output-filename #:exists 'append))
    [begin
      (display smth out)
      (close-output-port out)])
  
  (define (iter list)
    (if (null? list) (display "Done!")
        [begin
          (write-out (first list))
          (write-out #\return)
          (iter (rest list))]))
  [begin
    (define out (open-output-file output-filename #:exists 'truncate))
    (display "" out)
    (iter (filter-strings (file->lines input-filename)))])
