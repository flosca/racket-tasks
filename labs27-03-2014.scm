#lang racket

; №1

#|
Пусть на вход подаются два файла. 
В одном файле-словаре [dictionary.txt] построчно через пробел записаны пары слов вида: <слово> <замена>.
В другом файле [input.txt] задан текст из слов с возможными разделителями (помимо пробелов
могут встречаться точки, запятые, вопросительные и восклицательные знаки и т.п.) 
Требуется записать в выходной файл [output.txt] текст, в котором произведены замены согласно словарю.
|#


(define (change-words)
  (define dict  (string-split (file->string "dictionary.txt") "\n"))
  (define input (string-append " " (file->string "input.txt")))
  
  (define (iter dict str)
    (if (null? dict) str
        (let ((changes (string-split (car dict))))
          (iter (cdr dict) (string-replace str
                                           (string-append " " (first  changes))
                                           (string-append " " (second changes)))))))
  (define (write-out)
    (define out (open-output-file "changed-text.txt" #:exists 'replace))
    
    (let* ((xs (iter dict input))
           (len (string-length xs)))
    (begin
      (display (substring xs 1 len) out))
      (close-output-port out)))
  
  (begin
    (write-out)
    (close-input-port (open-input-file "input.txt"))))



; №3

#|
Пусть некоторый объект в начальный момент времени находится на координатной плоскости в точке с координатами (x0, y0).
Объект за один ход может двигаться на единичку вдоль любой из осей координат в любом направлении. 

Направления закодированы следующим образом:

              1
              |
       2______|______0
              |
              |
              3
Во входном файле содержится строка из кодов направлений его перемещений 
(например, 1 3 0 2 1 2 2).
В выходной файл следует записать построчно пары координат точек,
в которых находился объект.
|#

(define (dot-moving x y filename)
  (define input (open-input-file filename))
  (define first-pos (cons x y))
  
  (define (step-moves num)
    (cond ([eq? num 0] (cons 1 0))
          ([eq? num 1] (cons 0 1))
          ([eq? num 2] (cons -1 0))
          ([eq? num 3] (cons 0 -1))
          (else #f)))
  
  
  (define (iter x y)
    (define way (read input))
    (define out (open-output-file "way.txt" #:exists 'append))
    
    (if (eq? way eof) null
        (if (eq? #f (step-moves way)) 
            (begin (displayln "Ошибка, путь задан неверно." out) ;then
                   (close-output-port out))
            (let ([new-x (+ (car (step-moves way)) x)]           ;else
                  [new-y (+ (cdr (step-moves way)) y)])
              (display (cons new-x new-y) out)
              (displayln " " out)
              (close-output-port out)          
              (iter new-x new-y)))))
  
  
  (begin
    (define output (open-output-file "way.txt" #:exists 'truncate))
    (display "Начальное значение объекта: " output)
    (displayln first-pos output)
    (displayln " " output)
    (displayln "Через следующие точки объект последовательно двигался:" output)
    (close-output-port output)
    
    (iter x y)
    (close-input-port input)))

