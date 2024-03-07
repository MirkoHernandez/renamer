;; SPDX-License-Identifier: GPL-3.0-or-later
;; (renamer utils) -- Utility procedures. 
;; Copyright (C) 2024 Mirko Hernandez
;;
;;; Commentary:
;;  Procedures that manipulate datestamps and colorize text output.
;;; Code:
(define-module (renamer utils)
  #:use-module (ice-9 rdelim) 
  #:use-module (renamer regex)
  #:export (get-ignore-list-from-dir
	    add-datestamp
	    colorize-blue
	    colorize-green))

;;;; Utils datestamp
(define* (time-string stat date-format #:optional (modification-or-creation stat:mtime ))
  "Ruturn a datestamp string. STAT is the stat info of a file,
DATE-FORMAT is the format of the resulting string - YYMMDD, YY-MM-DD, etc." 
  (strftime date-format 
	    (localtime (modification-or-creation stat))))

;;;; Color
(define (colorize-blue s)
(string-append 
  "\x1b[34m" s "\x1b[0m"))

(define (colorize-green s)
  (string-append 
   "\x1b[32m" s "\x1b[0m"))

;;;; Ignore files
(define (get-ignore-list-from-dir dir)
  "Find and read the .renamer-ignore file in DIR. Return list of regexps
patterns to be ignored." 
  (let ((ignore-file  (string-append dir
				     file-name-separator-string
				     ".renamer-ignore") ))
    (if (file-exists? ignore-file)
	(let* ((port (open-input-file ignore-file))
	       (file-contents (read-string port)))
	  (close-port port)
	  (make-compiled-regexp-list 
	   (string-split file-contents #\newline)))
	'())))
