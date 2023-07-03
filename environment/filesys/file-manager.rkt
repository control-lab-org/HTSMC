#lang racket
(provide

 ;linux-platform?
 filter-files
 create-dir
 read-generation-from-data
 system-cmd
 system-date
 system-time
 file-os-separator
 file-os-deletecommand
 ;OS
 )
(define OS 'W)
;;-------------------------------------------
;; This function executes OS  instructions
;; and return the response into string
;;-------------------------------------------
(define (system-cmd cmd)
  (let* ([str  (with-output-to-string
                (lambda () (system cmd)))]
         [len (string-length str)])
    (substring str 0 (sub1 len))))
;;-------------------------------------------
;;This functions validate if the program is
;; running under linux
;;-------------------------------------------
(define (linux-platform?)
   (with-handlers ([exn:fail? (lambda (v) #f)])
     (equal? "GNU/Linux" (system-cmd "uname -o"))))
;;-------------------------------------------


(define (OS-checker)
  (cond [ (linux-platform?)  (set! OS 'L)]
        [(set! OS 'W)]
        ))



;;-------------------------------------------
;;This functions returns the current OS
;; separator character
;;-------------------------------------------
(define (file-os-separator)
  (cond [ (equal? OS 'L) "/"]
        [ (equal? OS 'W) "\\"]
        ))

(define (file-os-deletecommand)
  (cond [ (equal? OS 'L) "rm"]
        [ (equal? OS 'W) "del"]
        ))  
;;-------------------------------------------
;; This funtion returns the  system time at
;; hh:mm:ss format
;; -------------------------------------------
(define (system-time)
  ;Windows-command
  (define (win-time)
    (let* ([full-time (system-cmd "ECHO: | time")]
          [time (substring full-time  21 29)])
      time))
  ;Linux-command
  (define (linux-time)
    (let ([time (system-cmd "date +%T")])
      time))
  
  (if (linux-platform?)
      (linux-time)
      (win-time)))
;; -------------------------------------------
;;Returns the system date
;;with the format dd-mm-yyyy
;; -------------------------------------------
(define (system-date)
  ;;Win-Command
  (define (win-date)
    (let*([full-date (system-cmd "ECHO: | date")]
          [date (substring full-date  21 31)])
      (string-set! date 2 #\-)
      (string-set! date 5 #\-)
      date ))
  (define (linux-date)
    (let* ([date (system-cmd "date +%d-%m-20%y")])
      date))
  (if (linux-platform?)
      (linux-date)
      (win-date)))



(define (check-for-substring string substring)
  (check-for-substring/list (string->list string) (string->list substring)))
;;-------------------------------------------
(define (check-for-substring/list loc loc-to-find)
  (cond [(empty? loc-to-find) true]
        [(empty? loc) false]
        [(check-one loc loc-to-find) true]
        [else (check-for-substring/list (rest loc) loc-to-find)]))
;;-------------------------------------------
(define (check-one loc loc-to-find)
  (cond [(empty? loc-to-find) true]
        [(empty? loc) false]
        [(not (char=? (first loc) (first loc-to-find))) false]
        [else (check-one (rest loc) (rest loc-to-find))]))
;;-------------------------------------------
(define (filter-file file extension)
   (check-for-substring file extension))
;;-------------------------------------------
(define (filter-files file-list extension)
  (if (null? file-list)
      null
      (let ([fname (first file-list)])
        (if (filter-file fname extension)
            (append (filter-files
                     (list-tail file-list 1)
                     extension)
                    (list fname))
            (filter-files
                     (list-tail file-list 1)
                     extension))
            )))
;;-------------------------------------------
(define (create-dir name)
  (cond [(not (directory-exists? name))
         (make-directory name)
         (printf "~s <dir> -> created~n" name)]
        ;[else (printf "~s <dir> -> Already Exist~n" name)]
        ))
                      
;;-------------------------------------------
(define (read-generation-from-data file-name)
  (let* ([input-0 (open-input-file file-name)]
        [generation (read input-0)])
    (close-input-port input-0)
    generation))

;;-------------------------------------------
(OS-checker)
;;-------------------------------------------
;;-------------------------------------------
;;; Debugg code
;;-------------------------------------------

;OS
