#lang racket

;#1
(define (similarize-polygon 
         input output k)
;; Input: each line contains a pair of numbers (could be rational or float) - coordinates of vertex in a plane
;; Output: same collection of verices, but multiplied in {k} times.
  
  (define (read-matr)
    (define in (open-input-file input))
    (define vertices (read input))
       (build-list vertices (λ (x) (build-list 2 (λ (y) (read in))))))
    
  
  (define (scalar* k matr)
    (map (λ (a) (map (λ (x) (* k x)) a)) matr))
  
  (define answer (scalar* k [read-matr]))
  
  (define (write-out n)
    (define out (open-output-file output #:exists 'replace))
    (displayln (length answer) out)
    (for-each (λ (x) (for-each (λ (y) (display y out) (display #\space out)) x) (displayln " " out)) answer) 
    (close-output-port out))
  
  (write-out output))




; #2
(define (pascal-triangle
         k filename)
         
; Builds the Pascal's triangle {with k strings} and writes it into a text file.         
  
  (define (next line)
    (map + (cons 0 line)
           (append line (list 0))))
  
  (define (pascal-list num)
    (define (iter lst k)
      (if (= k -1) null
          (cons lst (iter (next lst) (sub1 k)))))
  (iter (list 1) num))
  
  
  (let ([triangle (pascal-list k)]
        [out      (open-output-file filename #:exists 'replace)])
    
    (for-each (λ (line) 
                (for-each (λ (y) (display y   out)
                                 (display " " out)) line)
                (display "\n" out)) triangle) 
    (close-output-port out)))


; #3

(define (filter-pref-suff 
         input output
         prefix suffix)
;; Input: just text strings
;; Output: filtered strings (only those which start with {pref} and end with {suff})
  
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
  
  (define (write-out n)
    (define out (open-output-file output #:exists 'append))
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
    (define out (open-output-file output #:exists 'truncate))
    (display "" out)
    (iter (filter-strings (file->lines input)))])
