#lang racket
(require 2htdp/batch-io)
(provide make-simresult)

;; Read an parser a datfile into lists
(define (read-datfile file-name)
  (read-words-and-numbers/line  file-name))


;; A simresult is an object where
;; are storaged the numeric simulation
;; provided by a simulation software

;; it has fuctions to get in individual
;; vectors the time-series of:
;; - time (t)
;; - output(y)
;; - error (e)
;; - control signal (u)
;; - states (x)

(define (make-simresult dat-file)
  (let([datTS        (read-datfile dat-file)]
       [plot-matrix  null]
       )
    (define time-pos     first )
    (define output-pos   second)
    (define error-pos    third )
    (define ctrl-pos     fourth)
    (define states-pos   fifth )

    (define (get-vector  position-func)
      (list->vector (map (lambda (array) (position-func array)) datTS)))
   
    (define (ts2pltmatrix data)
       (vector-map list->vector (list->vector datTS)))
    
     (lambda (msg . args)
      (case msg
        [(time-series)  datTS]
        [(pltmatrix)   (ts2pltmatrix datTS)]
        [(time) (get-vector time-pos)]
        [(out)  (get-vector output-pos)]
        [(error)(get-vector error-pos)]
       
        ;; debuggin msg
        [else (error "Unknown simresult comand" msg)]))
    ))


;; Debugg Code
;(define sim (make-simresult "./test-output.dat"))
;(sim 'pltmatrix)
;(sim 'time)
;(sim 'out)
;(require "../plotter/plt-fnc.rkt")
;(plot-matrix (sim 'pltmatrix))
