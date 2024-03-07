;; SPDX-License-Identifier: GPL-3.0-or-later
;; (renamer renamer) -- renamer control functions, including main.
;; Copyright (C) 2024 Mirko Hernandez
;;
;;; Commentary:
;;; Code:
(define-module (renamer renamer)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 ftw)
  #:use-module (renamer regex) 
  #:use-module (renamer utils) 
  #:use-module (renamer operations)
  #:export (main))

(define help-string
  "Usage:
         renamer [options] filenames.

Renamer runs in dry run mode,  use -A to apply changes.

Find files:
    -D, --directory           Where to search for files.
    -r, --recursive           Recursive search.
Date Operations:
    -d, --datestamp           Include datestamp.
    -c  --compact             use compact datestamp (YYYYMMDD).
        --month               use datestamp with year and month (YYYYMM).
        --ctime               take modification of the file attributes for the datestamp.
        --atime               take last access time for the datestamp. 
        --remove-datestamp    remove datestamp. 
Text Operations:
        --remove-text         remove text. 
    -w  --whitespace          replace whitespace with the '-'.
    -p  --remove-punctuation  remove punctuation characters.
External Operations:
        --pdf                 rename using pdftk (the metadata's title).
        --pdf-author          rename using pdftk (the metadata's title and author).
General Options:
    -h  --help                Display usage information.
        --no-color            Do not colorize output. 
        --omit-ignores        If there is an omit file, ignore it.
")

;;;;  getopt
(define option-spec
  '(
   ;; which files and from which directory
    (recursive (single-char #\R))
    (apply (single-char #\A))
    (directory (single-char #\D) (value #t))
   ;; datestamp format
    (datestamp (single-char #\d))
    (duration )
    (pdf )
    (pdf-author)
    (compact (single-char #\c))
    (month   (single-char #\m) )
    (ctime)
    (remove-datestamp)
   ;; rename operation
    (lowercase (single-char #\l))
    (remove-text  (value #t))
    (remove-punctuation (single-char #\p))
    (whitespace  (single-char #\w) )
   ;; options
    (omit-ignores)
    (no-color)
    (help  (single-char #\h))))

(define (apply-rename-operation combined-operation name stat apply? no-color?)
  "Apply a COMBINED-OPERATION renamer operation using NAME and STAT (from nftw).
Apply? indicates if the changes are applied." 
  (let* ((filename (basename name))
	 (new-name (combined-operation name stat))
	 (current-dir (dirname name)))

    (if no-color?
	(format #t "~40a → ~a ~%" filename new-name)
	(format #t "~40a → ~a ~%"
		(colorize-blue 
		 filename)
		(colorize-green
		 new-name)))
    (when apply?
      (rename-file
       (canonicalize-path name)
       (string-append 
	current-dir
	"/" 
	new-name)))))

(define (make-nftw-procedure regex-patterns
			     ignored-files
			     combined-operation
			     recursive?
			     apply?
			     no-color?)
  "Creates a ntfw procedure. REGEX-PATTERNS is the list of patterns
to  search for  files. LOCAL-IGNORE-LIST  is a  list of  patterns that
should  be  ignored.  COMBINED-OPERATION  is the  procedure  with  the
combined list  of selected operations to  execute. RECURSIVE? indicate
if the search  should traverse nested directories.  APPLY? indicate if
the changes are applied. NO-COLOR disables colorized output."
  (lambda (name stat flag base level)
    (if (and (not recursive?) (> level 1))
	#t
	(let ((filename (basename name))
	      (current-dir (dirname name)))
	  (if (find-regexp filename ignored-files)
	      (begin
		(unless (equal? filename ".")
		  (format #t "Ignoring: ~s~%" filename))
		#t)
	      (begin
		(when (find-regexp filename regex-patterns)
		  (apply-rename-operation combined-operation name stat apply? no-color? )))
	      )
	  #t))))

(define (create-ignore-list dir omit-default-ignores?)
 "Create a list of regexps of files  to be omitted. DIR is the directory
in which  the .renamer-ignore  file is  searched. OMIT-DEFAULT?  is an
option that omits the default ignores in %default-ignore-regexp-list."
(let ((local-ignores (get-ignore-list-from-dir dir)))
  (if omit-default-ignores?
      local-ignores
      (append local-ignores %default-ignore-regexp-list))))

(define (rename-files options dir filenames)
  "Execute renaming operations specified in OPTIONS.
FILENAMES  is the  list  of filenames  or regexps  to  be renamed."
  (let* ((compiled-regexp-list (make-compiled-regexp-list filenames))
	 (ignored-files (create-ignore-list dir (option-ref options 'omit-ignores #f)))
	 (transformed-operations (make-transformed-operations options))
	 (operations (if (> (length transformed-operations) 1)
			   (apply compose transformed-operations)
			  (car transformed-operations)))
	 (nftw-procedure (make-nftw-procedure compiled-regexp-list
					      ignored-files
					      operations
					      (option-ref options 'recursive #f)
					      (option-ref options 'apply #f)
					      (option-ref options 'no-color #f))))
    (nftw dir nftw-procedure)))

(define (main args)
  "ARGS is the  list of arguments created  by (command-line).The expected
arguments are options and a list of files or regexps.
Display a help message if no files  argument is provided."
  (let* ((options (getopt-long args option-spec))
	 (dir (option-ref options 'directory #f))
	 (files (option-ref options '()  #f)))
    (if (or (equal? '() files)
	    (option-ref options 'help #f)
	    (< (length options) 2))
	(display help-string)
	(begin
	  (if dir
	      (rename-files options dir files)
	      (for-each
	       (lambda (filepattern)
		 (let ((dir (dirname filepattern))
		       (file (list (basename filepattern))))
		   (rename-files options dir file)))
	       files))))))
