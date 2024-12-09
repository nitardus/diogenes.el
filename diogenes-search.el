;;; diogenes-search.el --- Corpus Search for diogenes.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Michael Neidhart
;;
;; Author: Michael Neidhart <mayhoth@gmail.com>
;; Keywords: classics, tools, philology, humanities

;;; Commentary:

;; This file contains functions for searching the Corpora that Diogenes can read

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'diogenes-lisp-utils)

(defun diogenes--process-pattern (pattern)
  "Make sure a pattern is all ASCII. When dealing with greek
unicode, convert it to beta code and correctly escape the regex metacharacters."
  (save-match-data
    (with-temp-buffer
      (save-excursion (insert pattern))
      (while (re-search-forward "\\cg+" nil t)
	(insert (replace-regexp-in-string "[)(+*\\\\]" "\\\\\\&"
					  (diogenes--utf8-to-beta (match-string 0))))
	(delete-region (match-beginning 0) (match-end 0)))
      (buffer-string))))

;;;; --------------------------------------------------------------------
;;;; SIMPLE SEARCH
;;;; --------------------------------------------------------------------

(defun diogenes--search-database (type &optional
				       options-or-pattern author-plist prefix)
  "Search for a phrase in Diogenes database using the Diogenes Perl Module.

This function is the generic dispacher for all corpora."
  (cond ((eql prefix 1)
	 (setf options-or-pattern
	       (list :pattern
		     (diogenes--process-pattern
		      (read-from-minibuffer "Enter search term: "))))
	 (unless (y-or-n-p (format "Search the whole %s corpus? "
				   type))
	   (setf author-plist
		 (list :author-nums
		       (diogenes--select-author-nums (list :type type))))))
	(prefix
	 (let ((patterns (cl-do ((patterns nil)
				 (pat (cl-loop for inp =
					       (diogenes--process-pattern
						(read-from-minibuffer "With pattern: "))
					       when (not (string-empty-p inp))
					       return inp)
				      (read-from-minibuffer "And with pattern: ")))
			     ((string-empty-p pat) (nreverse patterns))
			   (push pat patterns)))
	       (reject-pattern (read-from-minibuffer "Without pattern: "))
	       (context (completing-read "In context: "
					 (append '(("sentence" . "sent")
						   ("paragraph" . "para")
						   ("1 line" . 1))
						 (cl-loop for i from 2 upto 100
							  collect (cons (format "%d lines" i)
									i)))
					 nil t nil nil "sent"))
	       (min-matches (read-number "Minimum matches in context: " 1)))
	   (setf options-or-pattern
		 (append (when patterns `(:pattern-list ,patterns))
			 (when reject-pattern `(:reject-pattern ,reject-pattern))
			 (when context `(:context ,context))
			 (when min-matches `(:min-matches ,min-matches)))))
	 (unless (y-or-n-p (format "Search the whole %s corpus? "
				   type))
	   (setf author-plist
		 (list :author-nums
		       (diogenes--select-author-nums (list :type type))))))
	((stringp options-or-pattern)
	 (setf options-or-pattern (list :pattern options-or-pattern)))
	((not (plistp options-or-pattern))
	 (error "%s must be either a string (pattern) or a plist!"
		options-or-pattern)))
  (setf options-or-pattern (plist-put options-or-pattern :type type))
  (diogenes--do-search options-or-pattern author-plist))

;;;; --------------------------------------------------------------------
;;;; WORDLIST SEARCH
;;;; --------------------------------------------------------------------

;;; TO BE FILLED IN




(provide 'diogenes-search)

;;; diogenes-search.el ends here

