;; SPDX-License-Identifier: GPL-3.0-or-later
;; (renamer operations) --  Renaming operations.
;; Copyright (C) 2024 Mirko Hernandez
;;
;;; Commentary:
;; All the renamer operations are included  here. There are 3 kinds of
;; operations:
;; - Some take a filename string as argument.
;; - Some the filename  and options.
;; - Some take  the filename,  stat info,  and options.
;;; Code:
(define-module (renamer operations)
  #:use-module (srfi srfi-1) ;;filter-map
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 regex)
  ;; shell output
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports) ;;get-string-n
  #:use-module (ice-9 ftw)
  #:use-module (renamer utils) 
  #:use-module (renamer regex) 
  #:export (make-datestamp-operation
	    make-rename-operation
	    make-transformed-operations 
	    %operations-alist))

;;;; Date operations
(define-inlinable (add-datestamp name stat date-format time-function)
  "Adds a datestamp to NAME using DATE-FORMAT. STAT is the stat info from ntfw,
TIME-FUNCTION the function used to generate the datestamp." 
  (string-append (strftime
		  date-format 
		  (localtime (time-function stat)))
		 "--" 
		 (remove-datestamp name)))

(define-inlinable (remove-datestamp filename)
 "Remove the datestamp from FILENAME." 
  (let ((found-regex  (find-regexp filename %datestamp-regexp-list)))
    (if found-regex 
	(regexp-substitute #f
			   (regexp-exec found-regex filename )
			   'pre "" 'post))
    filename))

(define (make-datestamp-operation name stat options)
  "Return  a datestamp  operation.NAME and  STAT are  the arguments  from
nftw.  OPTIONS are  the command  line options:  month, compact,  short
determine the datestamp format; ctime and atime the time value used to
generate the datestamp."
  (let ((filename (basename name))
	(date-format (cond ((option-ref options 'month #f) "%Y-%m")
			   ((option-ref options 'compact #f) "%Y%m%d")
			   ((option-ref options 'short #f) "%y%m%d")
			   (#t "%y-%m-%d")))
	(time-function (cond ((option-ref options 'ctime #f)
			      stat:ctime)
			     ((option-ref options 'atime #f)
			      stat:atime)
			     (#t  stat:mtime))))
    (add-datestamp filename stat date-format time-function)))

;;;; Using external programs for renaming.
(define (rename-pdf name options)
  "Use the  pdftk program to  get metadata information, title  and author
and rename NAME using that data. NAME is the name argument to ntfw."
  (let* ((filename (basename name))
	 (extension (get-extension filename))
	 (filename-no-extension (get-filename-without-extension filename))
	 (file  (canonicalize-path name))
	 (pages? (option-ref options 'pages #f))
	 (author? (option-ref options 'author #f))
	 (title? (or (option-ref options 'title #f)
		     (and (not author?) (not pages?)))))
    (if (not (equal? (string-downcase extension) ".pdf"))
	filename
	(call-with-values
	    (lambda ()
	      (get-pdf-metadata file)) 
	  (lambda (title author pages)
	    (string-append
	     (remove-invalid-characters
	      (string-trim-right
	       ;;title, author, pages
	       (string-append (if (and title? title (not (equal? title
						    "untitled")))
				  title
				  filename-no-extension)
			      (if (and author? author)
				 (string-append "--" author) "")
			      (if (and pages? pages)
				  (string-append "__" pages) ""))
	       ;; Remove trailing . and space characters
	       (char-set #\. #\ )))
	     ;;extension
	     extension))))))

(define (media-duration name )
  "Use the ffprobe program to get the duration of the file. NAME is the name argument to ntfw."
  (let* ((filename (basename name))
	 (file (canonicalize-path name))
	 (port (open-input-pipe (string-append "ffprobe -show_data -hide_banner \""
					       file 
					       "\" 2>&1 ")))
	 (str  (get-string-n port 1000)))
    (close-pipe port)
    (let ((duration  (string-match ".*Duration: ([0-9:]+).*" str))
	  (filename-no-extension (get-filename-without-extension filename)) 
	  (extension (get-extension filename)))
      (if (and duration
	       (not (equal? duration "N/A" )))
	  (string-append
	   filename-no-extension
	   "_"
	   (match:substring duration 1)
	   extension)
	  (begin 
	    filename)))))

;;;; text operations
(define (remove-punctuation name)
  "Removes punctuation  characters from NAME. The  punctuation characters
are included in %invalid-filename-chars-regexp." 
  (let* ((filename (basename name))
	 (extension (get-extension name))
	 (filename-no-extension (get-filename-without-extension filename))
	 (result
	  (string-append 
	   (regexp-substitute/global #f
				     %invalid-filename-chars-regexp
				     filename-no-extension
				     'pre "-" 'post)
	   extension)))
    result))

(define (replace-spaces name)
  "Replace whitespace with the '-' character."
  (let ((filename (basename name) ))
    (regexp-substitute/global
     #f
     ;; replace trailing '-' 
     "-\\."
     (regexp-substitute/global #f
			       ;; replace multiple '-' with a single one.
			       "-{2,15}" 
			       (regexp-substitute/global #f
							 %whitespace-regexp

							 (string-trim-both
							  filename) 
							 'pre "-" 'post)
			       'pre "-" 'post)
     'pre "." 'post)))

(define (remove-text name options)
  "Remove  text from  NAME.  OPTIONS  is the  command  line options,  the
remove-text field must have the pattern used to remove text." 
  (let* ((filename (basename name))
	 (text (option-ref options 'remove-text #f))
	 (filename-no-extension (get-filename-without-extension filename))
	 (extension (get-extension filename)))
    (string-append 
     (regexp-substitute/global #f
		               (or text "")
			       filename-no-extension 
			       'pre "" 'post)
     extension)))

(define (lowercase filename)
 "Lowercase FILENAME." 
  (string-downcase (basename filename)))

(define (separator name options)
  "Add  a separator to NAME.  OPTIONS is  the  command line  options,
the separator is inserted  before the text indicated  by the add-separator
field." 
  (let* ((filename (basename name))
	 (text (option-ref options 'separator #f))
	 (filename-no-extension (get-filename-without-extension filename))
	 (extension (get-extension filename)))
    (string-append
     (regexp-substitute #f
			(regexp-exec 
			 (make-regexp (or text ""))
			 filename-no-extension)
			'pre  (string-append "__" (or text "")) 'post)
     extension)))

;; NOTE: List of operations to be used  with compose . This is why the
;; list is reversed; operations that appear on top are executed first.
;; the number of operations determine the arity of the procedure.
(define %operations-alist
  (reverse!
   `(
     ;; Add or remove text
     (duration ,media-duration)
     (pdf ,rename-pdf options)
     (remove-text  ,remove-text options )
     (separator  ,separator options)
     (remove-punctuation  ,remove-punctuation)
     ;; Transform text. 
     (whitespace  ,replace-spaces)
     (lowercase  ,lowercase)
     ;; Add or remove text based on file properties.
     (datestamp ,make-datestamp-operation stat options)
     (month     ,make-datestamp-operation stat options )
     (compact   ,make-datestamp-operation stat options)
     (append-text append-text options )
     (remove-datestamp ,remove-datestamp ))))

;; NOTE:  instead  of  executing  the operations  directly,  they  are
;; transformed into procedures of arity  2 that returns 2 values. This
;; way new operations can be easily composed.
(define (make-transformed-operations options)
  "Creates a list of all the operations  that will be used on each of the
matched files. OPTIONS is the list of options selected by the user."
  (let ((operations 
	 (filter-map (lambda (op)
		       (if  (option-ref options (car op) #f)
			    (begin
			      (case  (length op)
				((2)
				 (lambda (name stat)
				   (values
				    ;; Just the filename. 
				    ((cadr op ) name)
				    stat)
				   ))
				((3)
				 (lambda (name stat )
				   (values
				    ;; The filename and options.
				    ((cadr op) name options )
				    stat))
				 )
				((4)
				 (lambda (name stat )
				   (values
				    ;; The filename, stat and options.
				    ((cadr op) name stat options )
				    stat)))))
			    #f)
		       )    %operations-alist)))
    operations))
