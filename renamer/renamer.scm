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

(define concise-help-string
  "Usage: renamer [options] filenames.

 Renamer runs  in dry  run mode, use  -A to apply  changes.

 Use  -h to display the full list of options.

 These are common options:
    -d, --datestamp           Include datestamp.
    -w, --whitespace          replace whitespace with the '-' character.
    -l, --lowercase           lowercase the file name. 
    -p, --remove-punctuation  remove punctuation characters.
        --pdf                 rename using pdftk (the metadata's title).
")

(define help-string
  "Usage: renamer [options] filenames.

Renamer runs in dry run mode,  use -A to apply changes.

 Find files:
    -D, --directory           Where to search for files.
    -r, --recursive           Recursive search.
 Date Operations [-d | -c --month]: 
    
    -d, --datestamp           Include datestamp.
    -c, --compact             use compact datestamp (YYYYMMDD).
        --month               use datestamp with year and month (YYYYMM).
        --ctime               take modification of the file attributes for the datestamp.
        --atime               take last access time for the datestamp. 
        --remove-datestamp    remove datestamp. 
 Text Operations:
        --remove-text         remove text. 
    -w, --whitespace          replace whitespace with the '-' character.
    -l, --lowercase           lowercase the file name. 
    -p, --remove-punctuation  remove punctuation characters.
 External Program Operations:
        --pdf                 rename using pdftk (the metadata's title).
        --title               option to add title.
        --author              option to add author.
        --pages               option to add pages. 
        --duration            add time duration to a media file.
 General Options:
    -h, --help                Display the full list of options.
        --no-color            Do not colorize output. 
        --quote-rx            Quote regular expression characters in filenames.
    -i  --ignore-rx           Ignore the files matching a regular expression.
        --omit-ignore-files   If there is an ignored file in a directory, omit it.
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
    (compact (single-char #\c))
    (month   (single-char #\m) )
    (ctime)
    (remove-datestamp)
    (duration )
   ;; pdf 
    (pdf)
    (title)
    (author)
    (pages)
   ;; rename operation
    (lowercase (single-char #\l))
    (remove-text  (value #t))
    (remove-punctuation (single-char #\p))
    (whitespace  (single-char #\w) )
   ;; options
    (omit-ignore-files)
    (ignore-rx (single-char #\i) (value #t))
    (quote-rx)
    (no-color)
    (help  (single-char #\h))))

(define (apply-rename-operation combined-operation name stat apply? no-color?)
  "Apply a COMBINED-OPERATION renamer operation using NAME and STAT (from nftw).
Apply? indicates if the changes are applied." 
  (let* ((filename (basename name))
	 (new-name (basename (combined-operation name stat)))
	 (current-dir (dirname name)))

    (if no-color?
	(format #t "~40a → ~a ~%" filename new-name)
	(format #t "~40a → ~a ~%"
		(colorize-blue  filename)
		(colorize-green new-name)))
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
		;; TODO: debug or verbose mode.
		(unless (equal? filename ".")
		  (format #t "Ignoring: ~s~%" filename))
		#t)
	      (begin
		(when (find-regexp filename regex-patterns)
		  (apply-rename-operation combined-operation name stat apply? no-color? )))
	      )
	  #t))))

(define (create-ignore-list dir omit-default-ignores? ignore-rx)
 "Create a list of regexps of files  to be omitted. DIR is the directory
in which  the .renamer-ignore  file is  searched. OMIT-DEFAULT?  is an
option that omits the default ignores in %default-ignore-regexp-list."
(let ((local-ignores (get-ignore-list-from-dir dir)))
  (let ((ignored-files (if ignore-rx  (make-compiled-regexp-list (list ignore-rx) #f) 
			   '())))
    (append ignored-files  
	    (if omit-default-ignores?
		local-ignores
		(append local-ignores %default-ignore-regexp-list))))))

(define (rename-files options dir filenames)
  "Execute renaming operations specified in OPTIONS.
FILENAMES  is the  list  of filenames  or regexps  to  be renamed."
  (let* ((compiled-regexp-list (make-compiled-regexp-list filenames (option-ref options 'quote-rx #f) ))
	 (ignored-files (create-ignore-list dir (option-ref options 'omit-ignore-files #f)
					    (option-ref options 'ignore-rx #f)))
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
    (cond
     ((option-ref options 'help #f)
      (display help-string))
     ((or (equal? '() files)
	  (< (length options) 2))
      (display concise-help-string))
     (else
      (if dir
	  (rename-files options dir files)
	  (for-each
	   (lambda (filepattern)
	     (let ((dir (dirname filepattern))
		   (file (list (basename filepattern))))
	       (rename-files options dir file)))
	   files))))))


