#lang racket
(require "../../plant/rdynsys.rkt"
         "../sim/sim-params.rkt"
         "../filesys/file-manager.rkt"
         "./i2f-v2.rkt"
         "./integral-library.rkt"
         )

;;TODO: Test-odelib ... ok

;; The gen-ode function write an ode file from 4 arguments:
;; model, ctrl-law, reference and simparams
;; - the model is described by reduce-dynsys (rdynsys)
;; - Control and reference are a s-expresion.
;; The function returns the full name (rute and name) of the ode file generated.
(provide ;gen-ode
         ;gen-ode-env
         environment->odefile
         ;create-odedir
         ;make-odefile
         ;write-odefile
         ;close-output-port
         )

;; The name of the folder where are located the ode files
(define odefolder "odefiles")
;; Extension of the files created
(define odeextens "ode")

;; This function create the folder where are going to
;; locate odefiles
(define (create-odedir)
  (create-dir odefolder)
  )






;;-------------------------------------------
;; This function build the name of the file
;; in order to be located into the current directory
;;;-------------------------------------------
(define (build-name dir odename separator)
  (let([file-name
        (string-append dir odefolder separator
                       odename "." odeextens)])
    file-name))

;;-------------------------------------------
;; This function path the odefile to be in the
;; current directory
;;;-------------------------------------------
(define (set-route odename)
  (let*  ([dir (path->string (current-directory))])
    (build-name dir odename (file-os-separator))))
            

;; This function create a file from a name given
;; if another with the same name in is already exist
;; it is replaced for the new one.
(define (make-odefile file-name)
  (let  ([out (open-output-file
                file-name
                #:mode    'binary 
                #:exists  'replace )])
    out))

;;This function write the differential equations
;;into the file,  the symbols and the expresions are
;; required
(define (write-sys-deq file symbols expresion)
  (fprintf file "# System ode definition " )
  (for-each
   (lambda (state state-eq)
     (fprintf file "~n~s'=~s" state (infix-2-postfix state-eq) ))
   symbols  expresion))


;; (define (write-ctrl-deq file symbols expresion)
;;   (fprintf file "# Integral deq definition " )
;;   (for-each
;;    (lambda (state state-eq)
;;      (fprintf file "~n~s'=~s" state (infix-2-postfix state-eq) ))
;;    symbols  expresion))

;; This function write the error expresion
;; into the file. Reference and system output
;; expression are required.
(define (write-error file ref outsys)
  (fprintf file "~n# Error" )
  (fprintf file "~ne=~s-~s"
           (infix-2-postfix ref)
           (infix-2-postfix outsys)
           ))
;; This function write the control law expr
;; into the file. 
;;
;; --------------------------------------------------
;; FIREMANS FUCNTIONS
;; --------------------------------------------------
(define (write-static-ctrllaw file ctrl-law)
  (fprintf file "~n# Ctrl law" )
  (fprintf file "~nu=~s" (infx<-fix ctrl-law))
  )

(define (write-dynamic-ctrllaw file ctrl-law)
  (let* ([aux-sys (make-integral-sys ctrl-law)]
         [i-fnc (integral-fnc aux-sys)]
         [u-fnc (u-fnc aux-sys)])
    (fprintf file "~n# Aux Sys" )
    (fprintf file "~n~s'=~s" integral-symb (infx<-fix i-fnc))
    (write-static-ctrllaw file u-fnc)))

(define (write-ctrllaw file ctrl-law)
  (if (dynamical-ctrl? ctrl-law)
      (write-dynamic-ctrllaw file ctrl-law)
      (write-static-ctrllaw file ctrl-law))
  )
;; This function write the aux variables of the model
;; into the file. 
(define (write-vars file symbs values)
  (fprintf file "~n# System Parameters" )
  (for-each  (lambda (symbol value)
               (fprintf file "~nparam ~s=~s" symbol value))
             symbs values))
             
;; This function write the aux variables into the file. 
(define (write-iconds file symbs values)
  (fprintf file "~n# Initial Conds" )
  (for-each  (lambda (symbol value)
               (fprintf file "~n~s(0)=~s" symbol value))
             symbs values))


;; This function write the aux variables
;; to xppaut build the time series in
;; a fixed places
(define (write-tseries file ref outsys )
  (fprintf file "~n# Time Series Outputs:~n# (t,x0,...,xn,ref,y,e,u)" )
  ;;States
  ;; States are generated automatic by xppaut

  ;;Reference
  (fprintf file "~naux ref=~s" (infix-2-postfix ref))
  ;;output
  (fprintf file "~naux output=~s" (infix-2-postfix outsys ))
  ;;Error
  (fprintf file "~naux error=e")
  ;;ctrl
  ;(fprintf file "~naux ctrl=u")
  )

;; This function write on the file
;; the simulation parameters into xppaut format
(define (write-simparm file simparam )
  (define simp (if (null? simparam)
                       (make-sim-prmtrs)
                       simparam
                       ))
        
   ;; SIM PARAMETERS
    (fprintf file "~n@ bounds=~a" (simp 'BOUNDS))
    (fprintf file "~n@ T0= ~a"    (simp 'T0))
    (fprintf file "~n@ total=~a"  (- (simp 'TF) (simp 'T0)))
    (fprintf file "~n@ DT=~a"     (simp 'DT))
    (fprintf file "~ndone" ))
             
;; This function write in xppaut format
;; the system description.
;; The s-expr from ctrl and ref are
;; translate to a unfix sencences
(define (write-odefile file system  ctrl ref) 
  (let ([outsystem (first (system 'out-expr))])
    (write-sys-deq     file  (system 'state-symb)  (system 'state-expr))
    (write-error   file  ref outsystem)
    (write-ctrllaw file  ctrl)
    (write-vars    file (system 'prmtr-symb) (system 'prmtr-val))
    (write-iconds  file (system 'state-symb) (system 'init-cond))
    (write-tseries file  ref outsystem)
    (write-simparm file  sim-prms))
  )

(define (write-odefile-env file system  ctrl ref param) 
  (let ([outsystem (first (system 'out-expr))])
    (write-sys-deq file
                   (system 'state-symb)
                   (system 'state-expr))
    (write-error   file  ref outsystem)
    (write-ctrllaw file  ctrl)
    (write-vars    file (system 'prmtr-symb) (system 'prmtr-val))
    (write-iconds  file (system 'state-symb) (system 'init-cond))
    (write-tseries file  ref outsystem)
    (write-simparm file  param))
  )


;; gen-ode create an odefile ready
;; to simulate with xppaut, from a description
;; of a system, ctrl , reference signal.
;; This functions returns the name with route
;; of where the currente file is located.
(define (gen-ode system  ctrl ref )
  (create-odedir)
  (let* (
         [filename (set-route "system")]
         ;[filename (set-route (system 'name))]
         [file (make-odefile filename)])
    (write-odefile file system ctrl ref)
    (close-output-port file)
    filename
    ))

(define (gen-ode-env system [ctrl 0] [ref 0] [params null] )
  (create-odedir)
  (let* ([system-name (system 'name)]
         [filename (set-route system-name)]
          [file (make-odefile filename)])
    (write-odefile-env file system ctrl ref params)
    (close-output-port file)
    filename
    ))


;; (define (gen-ode-env system [ctrl 0] [ref 0] [params null] )
;;   (create-odedir)
;;   (let* ([system-name (system 'name)]
;;          ;[filename (set-route system-name)]
;;          [filename (set-route "system")]
;;          [file (make-odefile filename)])
;;     (write-odefile-env file system ctrl ref params)
;;     (close-output-port file)
;;     filename
;;     ))

(define (environment->odefile system  [ctrl 0] [ref 0] [prmtrs null])
  (gen-ode-env system ctrl ref prmtrs))




         
;; ;;------------------------
;; ;;      Debugg Code
;; ;;------------------------
;; (require "../../plant/plant-library.rkt")
;; ;; Plant Model
;; (define testmodel (SimplePendulum))
;; ;;; Ctrl Law
;; ;(define ctrllaw-test (list '* 10 'e ))
;; (define ctrllaw-test '(* (+ (- e) (* e -16.720351734625446))  (+ (- -16.647842075384954) e)))
;; ;;;; Reference functions
;; ;;;Function: (define ref-test (list '* 2.0 (list 'sin 't)))
;; ;;;Static:   (define ref-test (/ pi 2))
;; (define ref-test (/ pi 2))
;; ;
;; ;;; This functions makes the odefile to xppaut simulation
;; ;;; and returns the file name with route
;; (gen-ode testmodel ctrllaw-test ref-test)
;; (define odename  (gen-ode testmodel ctrllaw-test ref-test))
;; (define odename2 (gen-ode-env testmodel ctrllaw-test ref-test))
;; (define odename-env (environment->odefile testmodel ctrllaw-test ref-test))
