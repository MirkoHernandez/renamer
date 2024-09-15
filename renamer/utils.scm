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
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports) ;;get-string-n
  #:export (get-ignore-list-from-dir
	    get-pdf-metadata 
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

(define (which executable )
 "Returns true if EXECUTABLE is available " 
  (if (access? executable X_OK)
      executable
      (access? (or (search-path (parse-path (getenv "PATH")) executable)
		   "")
	       X_OK)))

(define (get-pdf-metadata file)
  (cond ((which "stapler")
	 (let* ((port (open-input-pipe (string-append "stapler info \""
						      file 
						      "\"")))
		;;NOTE: Reading the first 1500  characters should be enough to
		;; extract the title, author and number of pages.
		(str  (get-string-n port 1500 ))
		(s1 (string-match "^.*/Title: ([^\n]*)*\n" str))
		(s2 (string-match "^.*/Author: ([^\n]*)*\n" str)))
	   (close-pipe port)
	   (let ((title (and s1 (match:substring s1 1)))
		 (author (and s2 (match:substring s2 1))))
	     (values
	      title 
	      author 
	      #f))))
	((which "pdftk")
	 (let* ((port (open-input-pipe (string-append "pdftk  \""
						      file
						      "\" dump_data_utf8")))
		;;NOTE: Reading the first 1500  characters should be enough to
		;; extract the title, author and number of pages.
		(str  (get-string-n port 1500 ))
		(s1 (string-match "Title\nInfoValue: ([^\n]*)*\n" str))
		(s2 (string-match "Author\nInfoValue: ([^\n]*)*\n"  str))
		(s3 (string-match "NumberOfPages: ([^\n]*)*\n" str)))
	   (close-pipe port)
	   (let ((title (and s1 (match:substring s1 1)))
		 (author (and s2 (match:substring s2 1)))
		 (num-pages (and s3 (match:substring s3 1))))
	     (values
	      title 
	      author 
	      num-pages))))))
