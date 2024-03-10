;; SPDX-License-Identifier: GPL-3.0-or-later
;; (renamer regex) --   Regexp and regexp related functions.
;; Copyright (C) 2024 Mirko Hernandez
;;
;;; Commentary:
;;; Code:
(define-module (renamer regex)
  #:use-module (srfi srfi-1) ;;filter-map
  #:use-module (ice-9 regex)
  ;; shell output
  #:export (
	    make-compiled-regexp-list
	    find-regexp
	    get-extension
	    get-filename-without-extension
	    remove-invalid-characters
	    %file-extension-regexp
	    %file-and-extension-regexp
	    %whitespace-regexp
	    %default-ignore-regexp-list 
	    %local-ignore-file-regexp
	    %invalid-filename-chars-regexp 
	    %datestamp-regexp-alist 
	    %datestamp-regexp-list))
;;;; regexps
(define %whitespace-regexp
 (make-regexp " +"))

(define %invalid-filename-chars-regexp
 (make-regexp "[:,?\"|*<>/.;{}]"))

(define %file-and-extension-regexp
  (make-regexp
   "(.*)(\\.[a-zA-Z.]+)+$"))

(define %file-extension-regexp
  (make-regexp  
   "(\\.[a-zA-Z.]+)+$"))

(define %local-ignore-file-regexp
  ;; a file named renamer-ignore can be placed in a directory to ignore specified files
  (make-regexp "^\\.renamer-ignore"))
 
(define %default-ignore-regexp-list
  ;; by default dotfiles and the node_modules directory are ignored.
  `(,(make-regexp "^node_modules")
    ,(make-regexp "^\\..*")))

;; TODO: tags functionality.
(define %tags-regexp
  (make-regexp (string-append "(__[^.]+)+"
			      "(\\.[a-zA-Z.]+)+$")))

(define %datestamp-regexp-alist
  `((standard ,(make-regexp "^[0-9]{4}-([0][1-9]|[1][0-3])-[0-3][0-9]"))
    (compact ,(make-regexp "^[0-9]{4}([0][1-9]|[1][0-3])[0-3][0-9]"))
    (month ,(make-regexp "^[0-9]{4}-([0][1-9]|[1][0-3])"))))

(define %datestamp-regexp-list
  ;; A list of all datestamp related regexps."
  (map (lambda (r)
	 (cadr r) 
	 ) %datestamp-regexp-alist))

(define (find-regexp filename pattern-list)
  (find (lambda (pattern)
	  (regexp-exec pattern filename)
	  ) pattern-list))

(define (remove-invalid-characters str)
  "Remove invalid characters (for a filename) in the STR string."
  (regexp-substitute/global #f
			    %invalid-filename-chars-regexp
			    str 
			    'pre "" 'post))

(define (get-extension filename)
  "Return the extension of FILENAME."
 (let ((result (regexp-exec %file-extension-regexp filename)))
   (if result
	(match:substring result 1)
      "")))

(define (get-filename-without-extension filename)
 "Return  FILENAME without the extension." 
 (let ((result (regexp-exec %file-and-extension-regexp filename)))
   (if result
	(match:substring result 1)
      "")))

(define (make-compiled-regexp-list regexps quote-rx?)
  "Replace  a list  of  REGEXPS with  a list  of  compiled regexps.  Also
display a message indicating the invalid regexp." 
  (filter-map (lambda (r)
		(let ((regexp  
		       (false-if-exception
			(if quote-rx?
			    (make-regexp  
			     (regexp-quote r))
			    (make-regexp r)))))
		  (if regexp
		      regexp
		      (begin  
			(format #t "Invalid regexp ~s~%" r)
			#f)))
		) regexps))
