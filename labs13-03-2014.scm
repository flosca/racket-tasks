#lang racket

; #1
(define (words-partition filename)
 ; Takes a textfile, where each line contains one word.
 ; Returns files for all words, beginning with common letter.
  (define input (open-input-file filename))
  
  (define (write-output word)
    (let ((output (open-output-file (string-downcase (string-append (substring word 0 1) ".txt")) #:exists 'append)))
      (displayln word output)
      (close-output-port output)))
  
  (define (iter res)
    (define word (read-line input 'any))
    (if (equal? word eof) null
        (begin (write-output word)
               (iter res))))
  
  (iter input)
  (close-input-port input))


; #2

(define (rle str)
  ; Run-length encoding. 
  ; e.g. input: "RRRRREEEWWW00II", output: "R5E3W302I2"
  (define (iter i position counter result)
    (if (equal? str "") ""
        (if (negative? i) (string-append (~a position (if (= 1 counter) "" counter)) result)
            (let ((x (string-ref str i)))
              (if (char=? position x) (iter (- i 1) position (+ counter 1) result)
                  (iter (- i 1) x 1 (string-append (~a position (if (= 1 counter) "" counter)) result)))))))
  (iter (- (string-length str) 2) (string-ref str (- (string-length str) 1)) 1 ""))

; main function
(define (encode-rle filename)
  ; Encodes all strings in textfile.
  (define input  (open-input-file filename))
  (define output (open-output-file filename #:exists 'replace))
  (define (func res)
    (define string-from-input (read-line input))
    (if (eq? string-from-input eof) (displayln res output)
        (func (string-append res (rle string-from-input)))))
  (func "")
  (close-output-port output))

