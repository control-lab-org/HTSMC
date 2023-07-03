#lang racket
(require racket/system
         2htdp/batch-io
         "../filesys/file-manager.rkt"
          )
(provide simulate-ode
         create-datdir
         xppaut-simulate
         read-datfile
         make-simdata
         extract-name
         )
;; ---------------------------------------------------
;; The name of the  folder where are located the
;;  dat files and the respectively extensions
;; ---------------------------------------------------
(define datfolder  "datfiles")
(define odeextens "ode")
(define datextens  "dat" )
;(define datname    "ctrl")
(define logname    "logdata")
;; ---------------------------------------------------
;; ---------------------------------------------------
;; This functions makes the command
;; to simulate an ode file with xppaut
;; ---------------------------------------------------
(define (make-xppautcmd odename datname)
  (string-append "xppaut "  odename " -silent -quiet 1"
                 " -outfile ./datfiles/" datname "." datextens))
;; ---------------------------------------------------
;; ---------------------------------------------------
;; This function makes a directory where are going to
;; to save the dat files created by xppaut
;; ---------------------------------------------------
(define (create-datdir)
  (create-dir datfolder))
;; ---------------------------------------------------

;; ---------------------------------------------------
;; This function returns a part of list given until a
;; match is found
;; ---------------------------------------------------

(define (search-character name)
  (let ([splitter  #\/]
        [char (car name)])
    (if (equal? char splitter )
        null
        (list* char (search-character (cdr name))))))
;; ---------------------------------------------------
;; This function returns file name of a given dir
;; ---------------------------------------------------
(define (extract-name dir)
  (let* ([listf (reverse (string->list dir))]
         [list-woext (list-tail listf 4)]
         [name  (reverse (search-character list-woext))]
         )
    (list->string name)
    ))
    
;; ---------------------------------------------------
;; This function recives a string filename with 3
;; characteres of extension and the extension is
;; replaced by new one
;; ---------------------------------------------------
;; (define (change-xtension name extension)
;;   (let* ([listf (reverse (string->list name))]
;;          [sufix (list->string (reverse (list-tail listf 3)))]
;;          [newname (string-append sufix extension)]
;;         )
;;     newname))
;; ---------------------------------------------------

;; ---------------------------------------------------
;; ---------------------------------------------------
(define (simulation-error?)
  (let* ([logfile (set-route logname)]
         [ text (read-words-and-numbers/line  logfile)]
         [ lastline (last text)]
         [ sentence (string-upcase (first lastline))]
         )
    (cond [(equal? sentence "ERROR") #t] [else #f]) ))
;; ---------------------------------------------------
;; ---------------------------------------------------
;; Simulate an odefile and build *.dat file
;; ---------------------------------------------------
(define (xppaut-simulate odename datname)
  (let ([cmd (make-xppautcmd odename datname)])
    (system cmd)
    ))
;; ---------------------------------------------------
;; ---------------------------------------------------
;; Read an parser a datfile into lists
;; The order is the following:
;; t,x0,...,xn,ref,y,e,u
;(define (read-datfile file-name)
  ;(display (file-exists? file-name))
  ;(read-words-and-numbers/line  file-name))
(define (read-filedata filename)
  (let ([data (read-words-and-numbers/line  filename)])
    data))

;; Read an parser a datfile into lists
;; The order is the following:
;; t,x0,...,xn,ref,y,e,u
;(define (read-datfile file-name)
  ;(display (file-exists? file-name))
  ;(read-words-and-numbers/line  file-name))

(define (file-delete filename)
  (let ([del-cmd  (string-append (file-os-deletecommand) " " filename)])
    (system del-cmd)
    ;del-cmd
    ))

(define (read-and-erase-filedata filename)
  (let* ([data (read-filedata  filename)] )
    (file-delete filename)
    data))
    
;; ---------------------------------------------------
;; ---------------------------------------------------
(define (read-datfile name erase)
  (let ([datfile (set-route name)])
    (if (file-exists? datfile)
        (cond [(equal?  erase true) (read-and-erase-filedata datfile)]
              [(equal?  erase false) (read-filedata datfile)])
        null
        )))
;; ---------------------------------------------------
;; ---------------------------------------------------
;; This functions create the route
;; and append the name of the file
;; ---------------------------------------------------
(define (set-route datname)
  (let*  ([dir (path->string
                (current-directory))])
    (define file-name 
      (string-append dir  datfolder
                     (file-os-separator) datname
                     "." datextens))
    file-name))
;; ---------------------------------------------------
;; ---------------------------------------------------
;; This function recives an ode file and is simulate 
;; with Xppaut, generating a dat file, readed to build
;; the simulation time series array 
(define (simulate-ode odefile [erase? true])
  (let([system-name  (extract-name odefile)])
    (create-datdir)
    (xppaut-simulate odefile system-name)
    (read-datfile system-name erase?)
  ))
 

;;This function makes an object to manipulate
;; the simulation data array
;;(t , x0,..., xn,ref,y,e,u)
(define (make-simdata data-array)
  (let*([datTS        data-array]
        [auxdata (list 'r 'y 'e)]
        [sysorder (- (length (first datTS)) (add1 (length auxdata)))]
       )
    (define time-pos    0 )
    (define x1-pos 1)
    (define ref-pos     (+ sysorder 1))
    (define output-pos  (+ sysorder 2))
    (define error-pos   (+ sysorder 3))
    
    ;(define ctrl-pos    (+ sysorder 4))
 
    (define (get-vector  position-func)
      (list->vector (map (lambda (array) (list-ref array position-func )) datTS)))
   
    (define (ts2pltmatrix data)
       (vector-map list->vector (list->vector datTS)))
    
    (define (get-output)
       (get-vector output-pos))
    (define (get-error)
       (get-vector error-pos))
    (define (get-x1)
       (get-vector x1-pos))
    (define (get-time)
      (get-vector time-pos))

    (lambda (msg . args)
      (case msg
        [(time-series)  datTS]
        [(pltmatrix)   (ts2pltmatrix datTS)]
        [(t-vec) (get-time)]
        [(y-vec) (get-output)]
        [(e-vec) (get-error)]
        [(outpos) output-pos]
        [(x1-vec) (get-x1)]
        ;[(u-vec) (get-ctrl)]
        [(sysorder) sysorder]
        [(data) datTS]
        ;; debuggin msg
        [else (error "Unknown simdata object comand" msg)]))
    ))

;; --------------
;; Debug code
;; --------------
;(define odefile "./system-test.ode")
;(define test-sim-data (simulate-ode odefile #f))
;(define test-sim-data2 (simulate-ode odefile #t))




