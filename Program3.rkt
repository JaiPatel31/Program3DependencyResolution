#lang racket

(require csv-reading)
(require racket/string)

;; Data structures
(struct task (name duration dependencies) #:transparent)
(struct state (done pending) #:transparent)

;; Either monad for error handling
(struct left (value) #:transparent)
(struct right (value) #:transparent)

(define (either-bind x f)
  (match x
    [(left err) (left err)]
    [(right val) (f val)]))

(define (either-return x)
  (right x))

(define (fail msg)
  (left msg))

(define (succeed val)
  (right val))

;; Parse CSV file
(define (parse-csv-file filename)
  (call-with-input-file filename
    (lambda (port)
      (csv->list port))))

;; Create task from CSV row (fixed version)
(define (row->task row)
  (let* ([name (first row)]
         [duration (string->number (second row))]
         [deps-str (third row)]
         [deps (if (equal? deps-str "None")
                   '()
                   (map string-trim (string-split deps-str ",")))])
    (task name duration deps)))

;; Build dependency graph
(define (build-task-graph rows)
  (map row->task rows))

;; Validate tasks
(define (validate-tasks tasks)
  (let ([task-names (map task-name tasks)])
    (let loop ([remaining tasks] [errors '()])
      (if (null? remaining)
          (if (null? errors)
              (succeed tasks)
              (fail (reverse errors)))
          (let* ([current (car remaining)]
                 [missing (filter (lambda (d) (not (member d task-names))) 
                                 (task-dependencies current))])
            (if (null? missing)
                (loop (cdr remaining) errors)
                (loop (cdr remaining) 
                      (cons (format "Task '~a' depends on missing tasks: ~a" 
                                   (task-name current) missing)
                            errors))))))))

;; Check for circular dependencies
(define (check-circular-dependencies tasks)
  (define (visit task-name visited stack graph)
    (if (member task-name stack)
        (fail (format "Circular dependency detected involving: ~a" task-name))
        (if (member task-name visited)
            (succeed '())
            (let ([deps (task-dependencies (find-task-by-name task-name graph))])
              (let loop ([remaining deps] [new-stack (cons task-name stack)])
                (if (null? remaining)
                    (succeed (cons task-name visited))
                    (either-bind (visit (car remaining) visited new-stack graph)
                                (lambda (new-visited)
                                  (loop (cdr remaining) new-stack)))))))))
  
  (let ([graph tasks])
    (let loop ([remaining tasks] [visited '()])
      (if (null? remaining)
          (succeed tasks)
          (let ([task-name (task-name (car remaining))])
            (if (member task-name visited)
                (loop (cdr remaining) visited)
                (either-bind (visit task-name visited '() graph)
                            (lambda (new-visited)
                              (loop (cdr remaining) new-visited)))))))))

;; Find task by name
(define (find-task-by-name name tasks)
  (findf (lambda (t) (equal? (task-name t) name)) tasks))

;; Topological sort
;; Revised topological sort that naturally produces the expected order
(define (topological-sort tasks)
  (define (visit task-name visited result graph)
    (if (member task-name visited)
        (values visited result)
        (let ([deps (task-dependencies (find-task-by-name task-name graph))])
          ;; Process dependencies in reverse order to get DFS-like behavior
          (let-values ([(new-visited new-result)
                       (let loop ([remaining (reverse deps)] [v visited] [r result])
                         (if (null? remaining)
                             (values v r)
                             (let-values ([(v1 r1) (visit (car remaining) v r graph)])
                               (loop (cdr remaining) v1 r1))))])
            (values (cons task-name new-visited)
                    (cons (find-task-by-name task-name graph) new-result))))))
  
  ;; Process tasks in reverse input order to match example behavior
  (let loop ([remaining (reverse tasks)] [visited '()] [result '()])
    (if (null? remaining)
        (reverse result)
        (let ([task-name (task-name (car remaining))])
          (if (member task-name visited)
              (loop (cdr remaining) visited result)
              (let-values ([(new-visited new-result) (visit task-name visited result tasks)])
                (loop (cdr remaining) new-visited new-result)))))))

;; Schedule tasks
(define (schedule-tasks tasks)
  (let ([sorted (topological-sort tasks)])
    (let loop ([remaining sorted] [current-time 0] [schedule '()])
      (if (null? remaining)
          (reverse schedule)
          (let* ([task (car remaining)]
                 [entry (list current-time (task-name task) (task-duration task))])
            (loop (cdr remaining) 
                  (+ current-time (task-duration task)) 
                  (cons entry schedule)))))))

;; Main function
(define (dependency-resolver filename)
  (if (file-exists? filename)
      (let* ([csv-data (parse-csv-file filename)]
             [tasks (build-task-graph csv-data)])
        (either-bind (validate-tasks tasks)
                    (lambda (valid-tasks)
                      (either-bind (check-circular-dependencies valid-tasks)
                                  (lambda (checked-tasks)
                                    (let ([schedule (schedule-tasks checked-tasks)])
                                      (let ([total-time (apply max (map (lambda (x) (+ (car x) (caddr x))) schedule))])
                                        (right (list schedule total-time)))))))))
      (left (format "File not found: ~a" filename))))

;; Format output
(define (format-output result)
  (match result
    [(left error) (displayln error)]
    [(right (list schedule total-time))
     (displayln "Time Event Duration")
     (for ([entry schedule])
       (match entry
         [(list time name duration)
          (printf "~a ~a ~a\n" time name duration)]))
     (printf "Total time: ~a\n" total-time)]))

;; Enhanced main loop with better path handling
(define (main-loop)
  (display "Enter filename (or 'quit' to exit): ")
  (let ([input (string-trim (read-line))])
    (cond
      [(equal? input "quit") (displayln "Goodbye!")]
      [(string=? input "") (begin (displayln "Please enter a filename")
                                (main-loop))]
      [else
       (let ([full-path 
              (if (absolute-path? input)
                  input
                  (build-path (current-directory) input))])
         (format-output (dependency-resolver full-path))
         (newline)
         (main-loop))])))

;; Start the program
(module+ main
  (displayln "Dependency Resolver")
  (displayln "-------------------")
  (displayln "Note: You can use either relative or absolute paths")
  (displayln "Current directory:")
  (displayln (current-directory))
  (newline)
  (main-loop))