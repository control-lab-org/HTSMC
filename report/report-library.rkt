#lang racket
(require 
         "../environment/filesys/file-manager.rkt"
         "../evaluator/evaluator-library.rkt"
         )
(provide make-datalog
         read-backup-report
         make-simdatlog
         write-last-family
;         make-agents-reports
 )
;;-------------------------------------------
;; This libraby provide the functions to make a report of evolution of
;; a family.  The first block save the fitness vale and control law
;; The second block open the fitnes-ctrl file
;;-------------------------------------------
(define report-folder "reports")
;;-------------------------------------------

;;-------------------------------------------
;; This function create the
;; folder where are going to locate odefiles
;; -------------------------------------------
(define (make-reportdir)
  (create-dir report-folder)
  )
;;-------------------------------------------

;;-------------------------------------------
;; This function remove the '-' simbols from
;; form the system-date and append the elements
;; dd-mm-yyyy ->  yyyymmdd 
;;-------------------------------------------
(define (date->filenameformat full-date)
  (let([dd (substring full-date  0 2)]
       [mm (substring full-date  3 5)]
       [yyyy (substring full-date  6 10)])
    (string-append yyyy mm dd)
    ;;aaaa
    ;;dd
    ;mm
    ))
;;-------------------------------------------
;; This function open a file with the
;; needed characteristics the reports write
;;-------------------------------------------
(define (open-reportfile name [arg 'append])
(let    ([out (open-output-file
               name
               #:mode    'binary 
               #:exists  arg  )])
  out
  ))
;;-------------------------------------------
;;-------------------------------------------
(define (close-reportfile name)
  (close-output-port name))
;;-------------------------------------------
;; (define (name-plant plant)
;;   plant)

;;-------------------------------------------
;; This function build the name of a file
;; in order to be located into the current directory
;;;-------------------------------------------
(define (build-name prefix name extension)
  (let* ([dir (path->string (current-directory))]
         [date (date->filenameformat (system-date))])
    (define file-name
      (string-append dir report-folder (file-os-separator)
                     date prefix name extension))
    file-name))
  ;;-------------------------------------------
(define (build-name-data name extension)
  (let* ([dir (path->string (current-directory))]
         ;[date (date->filenameformat (system-date))]
         )
    (define file-name
      (string-append dir report-folder (file-os-separator)
                     name
                     extension))
    file-name))
                   
;;-------------------------------------------
;; This function create a *.dat file into the
;; current directory. If it does not exist. In
;; other case, the data is append.
;; The file name is given by:
;; - prefix: "datalog-ctrl-fit-evo-"
;; - plant name
;; - date *** (not available for versions previous than V20220708 )
;;-------------------------------------------
(define (open-datlog-file plant-name)
   (let* ( [prefix "-datalog-ctrl-fit-evo-"]
           ;[plant-name name-plant)]
           [extension ".dat"]
           [file-name
            (build-name prefix plant-name extension)]   
         )
      (open-reportfile file-name)

     ))
;;-------------------------------------------
(define (open-backdatlog-file plant-name)
   (let* ( [prefix "-backup-ctrl-fit-evo-"]
           [plant-name plant-name]
           [extension ".dat"]
           [file-name
            (build-name prefix plant-name extension)]
         )
      (open-reportfile file-name 'replace)

      ))
;; --------------------------------------------------
(define (open-racket-file plant-name)
   (let* ( [prefix "backfam"]
           [plant-name (string-append prefix plant-name)]
           [extension ".rkt"]
           [file-name
            (build-name-data  plant-name extension)]
         )
      (open-reportfile file-name 'replace)

     ))
;;-------------------------------------------
;; This function create a *.dat file into the
;; current directory.
;; If the file exist, the data is appended.
;; The file name is given by:
;; - prefix: "datalog-ctrl-fit-evo-"
;; - plant name
;; - date *** (not available for versions previous than V20220708 )
;;-------------------------------------------
(define (write-datalog-header file plant-name )
  (let* ( [date (system-date)]
          [time (system-time)]
          ;[plant-name (name-plant plant)]
          )
    (fprintf file
             "# Family for ~s system ~n" plant-name)
    (fprintf file
             "# Creation started the ~s at ~s hrs ~n" date time )
    ))

(define (write-datalog-footer file)
  (let* ([date (system-date)]
         [time (system-time)]
         )
    (fprintf
     file
     "# Creation finished the ~s at ~s hrs ~n" date time )
    ))

;; An indvidual is build by a list with the fitness and ctrl-law
(define (print-indv-data file indv)
   (fprintf file "~s~n"  indv))

(define (print-family-dat file family)
  (for-each (lambda (indv) (print-indv-data file indv)) family)
  (fprintf file "~n" ))

(define (print-indv-c file indv)
   (fprintf file "(define c '~s)~n"  (second indv)))

(define (print-family-c file family)
  (for-each (lambda (indv) (print-indv-c file indv)) family)
  (fprintf file "~n" ))

(define (print-indv-backdata file indv)
   (fprintf file "~s~n"  indv))


(define (print-backfamily-dat file family)
  (for-each (lambda (indv) (print-indv-backdata file indv)) family)
  )

(define (print-backfamily-ctrl file family)
  (for-each (lambda (indv) (print-indv-backdata file (second indv))) family)
  )


(define (write-datalog plant-name family)
  (let* ([datalog-file (open-datlog-file plant-name)]   )
    (write-datalog-header datalog-file plant-name)
    (print-family-dat datalog-file family)
    ;(write-datalog-footer datalog-file)
    (close-reportfile datalog-file)
    ))

(define (write-backup-datalog plant-name family)
  (let* ([datalog-file (open-backdatlog-file plant-name)]   )
    (fprintf datalog-file "(" )
    (print-backfamily-dat datalog-file family)
    (fprintf datalog-file ")" )
    (close-reportfile datalog-file)
    ))

(define (write-last-family plant-name family)
  (let* ([datalog-file (open-racket-file plant-name)]   )
    (fprintf datalog-file "#lang racket ~n~n")
    (fprintf datalog-file "(provide family-backctrl) ~n")
    (fprintf datalog-file "(define family-backctrl '(")
    (print-backfamily-ctrl datalog-file family)
    (fprintf datalog-file "))" )
    (close-reportfile datalog-file)
    ))

;;--------------------------------------------------
(define (take-nfirst family n)
  (map (lambda (x) (list-ref family x)) (range n)))

(define (open-nbest-file plant-name n)
   (let* ([prefix "-datalog-ctrl-fit-evo-"]
          ;[plant-name (name-plant plant)]
          [sufix (string-append
                  plant-name
                  "-"
                  (number->string n)
                  "best")]      
          [extension ".dat"]
          [file-name (build-name prefix sufix extension)]        
          )
     (open-reportfile file-name)
     ))

(define (open-nbestc-file plant-name n)
   (let* ([prefix "-datalog-ctrl-"]
          ;[plant-name (name-plant plant)]
          [sufix (string-append
                  plant-name
                  "-"
                  (number->string n)
                  "best")]      
          [extension ".rkt"]
          [file-name (build-name prefix sufix extension)]        
          )
      (open-reportfile file-name)
      ))

;;--------------------------------------------------
;;--------------------------------------------------
(define (write-nbest plant-name family n)
  (let* ([datalog-file (open-nbest-file plant-name n)]
         [subfamily (take-nfirst family n)])
    (write-datalog-header datalog-file plant-name)
    (print-family-dat datalog-file subfamily)
    (close-reportfile datalog-file)
    ))

(define (write-nbestc plant-name family n)
  (let* ([datalog-file (open-nbestc-file plant-name n)]
         [subfamily (take-nfirst family n)])
    (print-family-c datalog-file subfamily)
    (close-reportfile datalog-file)
  ))


(define (open-mediafitness-filed  )
  (let* ([name "h5fitmedia"]
         [extension ".dat"]
         [file-name (build-name-data name  extension)]        
          )
     (open-reportfile file-name)
     ))

(define (open-mediafitness-file plant-name [n null])
  (let* ([prefix "-mediafit-"]
          ;[plant-name (name-plant plant)]
          [d (if (null? n) "" (number->string n))]
          [sufix (string-append
                  plant-name
                  "-"
                  d
                  "family")]      
          [extension ".dat"]
          [file-name (build-name prefix sufix extension)]        
          )
     (open-reportfile file-name)
     ))

(define (print-mediafitness file fitnessm)
  (fprintf file "~n~s\t~s\t~s"
           (first fitnessm)
           (second fitnessm)
           (third fitnessm)
           ))

(define (write-family-mediafitness plant-name family [n 1])
  (let* ([datalog-file  (open-mediafitness-file plant-name)]
         [fitness-list (map first family)]
         [media (/ (foldl + 0 fitness-list)
                   (length fitness-list))]         
         [subfamily (take-nfirst family n)]
         [fitness-sub (map first subfamily)]
         [media-sub (/ (foldl + 0 fitness-sub)
                       (length fitness-sub))]
         [best-fit (first fitness-sub)]
         )
    (print-mediafitness
     datalog-file
     (list media media-sub best-fit))
    
    (close-reportfile datalog-file)
    (list media media-sub best-fit)
    ))
    
    
(define (write-family-mediafitnessd lista)
  (let* ([datalog-file (open-mediafitness-filed)]
         )
    (print-mediafitness datalog-file
                        lista)
    (close-reportfile datalog-file)
    ;(display datalog-file)
    ))





;;--------------------------------------------------
(define (open-hdatfile plant-name)
   (let* ([prefix "-hdat-evo-"]
          ;[plant-name (name-plant plant)]
          [extension ".dat"]
          [file-name (build-name prefix plant-name extension)]        
          )
      (open-reportfile file-name)
      ))
;;--------------------------------------------------
(define (print-element file element)
  (fprintf file "~s\t"  element))
;;--------------------------------------------------
(define (print-datastep file list-data)
   (for-each  (lambda (element) (print-element file element)) list-data)
   (fprintf file "~n"))
;;--------------------------------------------------
(define (print-timeseries file simdata)
  (for-each (lambda (steptime) (print-datastep file steptime))  simdata)
  (fprintf file "~n~n" ))
;;--------------------------------------------------
(define (write-hdatfiles plant datfamily)
  (let* ([hlog-file (open-hdatfile plant)] )   
    (write-datalog-header hlog-file plant)
    (print-timeseries hlog-file datfamily)
    ;(write-datalog-footer hlog-file)
    (close-reportfile hlog-file)))
;;--------------------------------------------------




(define (make-datalog plant-name family)
  (make-reportdir)
  (write-datalog plant-name family)
  ;(write-backup-datalog plant-name family)
  (write-nbest plant-name family 5)
  ;(write-nbest plant-name family 1)
  ;(write-nbestc plant-name family 5)
  (let ([data (write-family-mediafitness plant-name family 5)])
    (write-family-mediafitnessd data))
  ;(write-n-mediafitness plant-name family 5)
  ;(display "Fit-Ctrl Report done")
  )


(define (add-element prev add ref)
  (map (lambda (prev add) (flatten (append prev  (list-ref add ref))))  prev add)
)

(define (add-elements prev adds ref)
  (if (zero? (- (length adds) 1))
      (add-element prev (first adds) ref)
      (add-elements (add-element prev (first adds) ref) (list-tail adds 1) ref)))
         
(define (build-gnuplot-dat data-family) 
  (define onestep (first (first data-family)))
  (define outpos (- (length onestep) 2))
  (define refpos (- (length onestep) 3))
  (define time (map (lambda (data-mmbr) (list (first data-mmbr))) (first data-family)))
  (define data (add-elements time data-family refpos))
  (add-elements data data-family outpos)
)
;;; ------------------------------------
;;; Simulation Datalog functions
;;; ------------------------------------

(define (datacomplete?  membersdata)
   (zero? (count null? membersdata)))

(define (build-simdatlog plant membersdata)
  (make-reportdir)
  (let* ( [gnuformat (build-gnuplot-dat membersdata)])
    (write-hdatfiles plant gnuformat)))

;;take the size-first elements  
(define (get-first-n lista n)
  (drop-right  lista (- (length lista) n)))
  
(define (make-simdatlog plant family [size 3])
  (let* ([membersdata (get-first-n family size)])
    (if (datacomplete? membersdata)
        (build-simdatlog plant membersdata)
        (display "Cannot create hdata"))))
     
(define (build-readname  name extension)
  (let* ([dir (path->string (current-directory))]
         [file-name
          (string-append dir report-folder (file-os-separator)
                         name extension)])
    file-name
    ))

(define (backup->ctrllist backup)
  (map second backup)) 

(define (read-backup-report name)
  (let* ( [file-name (build-readname name ".dat")]
          [input-0 (open-input-file file-name #:mode 'text )]
          [backup (read input-0)]
          )
    (close-input-port input-0)
    ( backup->ctrllist  backup)
    ))



;; (define (make-agents-reports agents)
;;   (


;;; ------------------------------------
;;            DEBUGG   CODE
;;; ------------------------------------

(require "../plant/plant-library.rkt")



;; ------------------------------------
;;    TEST CODE
;; ------------------------------------
;; Uncomment the following lines to library test
;; ------------------------------------

;; (define test-plant (make-plant 2realpoles))
;; (define test-family-1
;;   '( ((* (* 14.684854025243505 (tanh (* (- (+ (* (/ -25.580649059446294 e) (/ -47.24227407164709 e))(/ (- e) (+ -10.344526044014238 e)))) (- (* (/ (- e) (- e)) (/ (+ 21.906436387574956 e) (- e)))))))(* (- (- (- (* -40.44725918514419 e)))) (- (- (- (* 49.15002186857271 e))))))
;;       0.15235442031190694)
;;      ((* (* 4.6738836151007135 (tanh (* (- (+ (* (/ -25.580649059446294 e) (/ -47.24227407164709 e))(/ (- e) (+ -10.344526044014238 e))))(- (* (/ (- e) (- e)) (/ (+ 21.906436387574956 e) (- e)))))))(* (- (- (- (* -40.44725918514419 e)))) (- (- (- (* 49.15002186857271 e))))))
;;       0.23483436057304002)
;;      ((* (* 14.28742263701518  (tanh (* (/ (+ (/ (- e) (- e)) (- (* 26.148456297553835 e))) (* (- (- e)) (+ (- e) (+ 33.28501727042804 e)))) (- (+ (/ (+ -25.117977504744037 e) (- e)) (- (- e)))))))(- (* (+ (+ (/ -18.927445895802723 (* e 0.7213069981038234)) (+ -37.66304369850841 (* e 0.9117201177956966))) (- (+ -7.541581255076043 (* e 0.8031465835064857))))(/(- (* 29.84459791406425 (* e 0.8180304809823492))) (+ (- (* e 0.7027623854052685)) (- (* e 1.0199651351088537)))))))
;;       6.250868259038485)
;;      ((* (* -19.366367819766626 (tanh (/ (- (- (/ (+ 43.63945464949113 (* e 1.196225430587048)) (+ 35.08017753702678 (* e 1.1579131523254178))))) (- (- (+ (/ 34.218818414648126 (* e 0.8810290672010849)) (- (* e 1.1382469117071845)))))))) (- (* (+ (- (- (* (* e 1.2878483498172035) 0.9456158297765822))) (+ (- (* (* e 0.9413974451857312) 1.0042312149754717))(- (* (* e 1.2199084710657973) 0.8392939252160889)))) (- (- (* -29.079065623279824 (* e 0.7659337402587332)))))))
;;       10.431643056585752)
;;      ((*(* 25.629666664397973 (tanh (- (+ (- (/ (- e) (- e))) (* (/ (- e) (/ -36.621208958609834 e)) (/ (/ 17.791492818070225 e) (- e)))))))(+(-(/(* (- (* e 0.9280425093213194)) (- (* e 0.9280425093213194)))(+ (* 9.812890724530746 e) (+ -36.420897183093686 (* e 1.1649403536477112)))))(- (- (- (- (* e 0.9280425093213194)))))))
;;       60.44611597248755))
;;   )
;; (define test-family-2
;;   '( ((* (* 14.684854025243505 (tanh (* (- (+ (* (/ -25.580649059446294 e) (/ -47.24227407164709 e))(/ (- e) (+ -10.344526044014238 e)))) (- (* (/ (- e) (- e)) (/ (+ 21.906436387574956 e) (- e)))))))(* (- (- (- (* -40.44725918514419 e)))) (- (- (- (* 49.15002186857271 e))))))
;;       0.15235442031190694)
;;      ((* (* 4.6738836151007135 (tanh (* (- (+ (* (/ -25.580649059446294 e) (/ -47.24227407164709 e))(/ (- e) (+ -10.344526044014238 e))))(- (* (/ (- e) (- e)) (/ (+ 21.906436387574956 e) (- e)))))))(* (- (- (- (* -40.44725918514419 e)))) (- (- (- (* 49.15002186857271 e))))))
;;       0.23483436057304002)
;;      ((* (* 14.28742263701518  (tanh (* (/ (+ (/ (- e) (- e)) (- (* 26.148456297553835 e))) (* (- (- e)) (+ (- e) (+ 33.28501727042804 e)))) (- (+ (/ (+ -25.117977504744037 e) (- e)) (- (- e)))))))(- (* (+ (+ (/ -18.927445895802723 (* e 0.7213069981038234)) (+ -37.66304369850841 (* e 0.9117201177956966))) (- (+ -7.541581255076043 (* e 0.8031465835064857))))(/(- (* 29.84459791406425 (* e 0.8180304809823492))) (+ (- (* e 0.7027623854052685)) (- (* e 1.0199651351088537)))))))
;;       6.250868259038485)
;;      ((* (* -19.366367819766626 (tanh (/ (- (- (/ (+ 43.63945464949113 (* e 1.196225430587048)) (+ 35.08017753702678 (* e 1.1579131523254178))))) (- (- (+ (/ 34.218818414648126 (* e 0.8810290672010849)) (- (* e 1.1382469117071845)))))))) (- (* (+ (- (- (* (* e 1.2878483498172035) 0.9456158297765822))) (+ (- (* (* e 0.9413974451857312) 1.0042312149754717))(- (* (* e 1.2199084710657973) 0.8392939252160889)))) (- (- (* -29.079065623279824 (* e 0.7659337402587332)))))))
;;       10.431643056585752)
;;      ((*(* 25.629666664397973 (tanh (- (+ (- (/ (- e) (- e))) (* (/ (- e) (/ -36.621208958609834 e)) (/ (/ 17.791492818070225 e) (- e)))))))(+(-(/(* (- (* e 0.9280425093213194)) (- (* e 0.9280425093213194)))(+ (* 9.812890724530746 e) (+ -36.420897183093686 (* e 1.1649403536477112)))))(- (- (- (- (* e 0.9280425093213194)))))))
;;       60.44611597248755))
;;   )

;; (make-datalog test-plant (append test-family-1 test-family-2))
;; (define backup-name "20220823-backup-ctrl-fit-evo-2realPoles")
;; (define A (read-backup-report backup-name))
