#lang racket
(require csv-reading)

(struct task (name duration dependencies))
(struct state (done pending))

(struct Left (value))
(struct Right (value))

(require csv-reading)

;; Basic reading
(define csv-content (call-with-input-file "data.csv" csv->list))

;; More controlled reading
(call-with-input-file "data.csv"
  (lambda (port)
    (let ([reader (make-csv-reader port)])
      (let loop ([rows '()])
        (let ([row (reader)])
          (if (empty? row)
              (reverse rows)
              (loop (cons row rows))))))))