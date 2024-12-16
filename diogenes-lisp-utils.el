;;; diogenes-lisp-utils.el --- Lisp utilities for diogenes.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Michael Neidhart
;;
;; Author: Michael Neidhart <mayhoth@gmail.com>
;; Keywords: classics, tools, philology, humanities

;;; Commentary:

;; This file contains some lisp utilites needed by diogenes.el

;;; Code:
(require 'cl-lib)
(require 'seq)

(defmacro diogenes--replace-regexes-in-string (str &rest subst-lists)
  "Apply a list of regex-substitutions to a string in sequence.
Each SUBST-LIST contains the REGEXP REP, followed optionaleval
parameters of `replace-regexp-in-string', FIXEDCASE LITERAL SUBEXP
START. Alternativly, SUBST-LIST can be a string or a list of one
element, in which case this is taken as the REGEXP and all of its
matches are deleted. 

Returns the resulting string."
  (declare (indent 1))
  (let ((result str))
    (dolist (subst subst-lists result)
      (setf result
	    (cl-typecase subst
	      (list (let ((regex (car subst))
			  (rep (or (cadr subst) ""))
			  (rest  (cddr subst)))
		      `(replace-regexp-in-string ,regex ,rep ,result
						 ,@rest)))
	      (string `(replace-regexp-in-string ,subst "" ,result))
	      (t (error "%s must be either a list or a string!"
			subst)))))))

(defun diogenes--plist-keys (plist)
  "Traverse a plist and extract its keys"
  (unless (plistp plist) (error "Not a plist!"))
  (cl-loop for key in plist by #'cddr
	   collect key))

(defun diogenes--plist-values (plist)
  "Traverse a plist and extract its values"
  (unless (plistp plist) (error "Not a plist!"))
  (cl-loop for key in (cdr plist) by #'cddr
	   collect key))

(defun diogenes--keyword->string (kw)
  (unless (keywordp kw) (error "Not a keyword: %s" kw))
  (substring (symbol-name kw) 1))

(defun diogenes--string->keyword (s)
  (intern (concat ":" s)))

(defun diogenes--hash-to-alist (hash-table)
  (cl-loop for k being the hash-keys of hash-table
	   using (hash-values v)
	   collect (cons k v)))

(defun diogenes--split-once (regexp str)
  "Split a string once on regexp and return the substrings as a list."
  (save-match-data
    (if (string-match regexp str)
	(list (substring str 0 (match-beginning 0))
	      (substring str (match-end 0)))
      (list str))))

(defun diogenes--get-text-prop-boundaries (property)
  "Get the boundaries of the region where property does not change."
  (list (or (previous-single-char-property-change (point) 'invalid-xml)
	    (point-min))
	(or (next-single-char-property-change (point) 'invalid-xml)
	    (point-max))))

(defun diogenes--ascii-alpha-p (letter)
  (or (<= 65 letter 90)
      (<= 97 letter 122)))

(defsubst diogenes--ascii-alpha-only (str)
  (cl-remove-if-not #'diogenes--ascii-alpha-p str))

(defun diogenes--string-equal-letters-only (str-a str-b)
  "Compare two string, making them equal if they contain the same letters"
  (string-equal (replace-regexp-in-string "[^[:alpha:]]" "" str-a)
		(replace-regexp-in-string "[^[:alpha:]]" "" str-b)))

(defun diogenes--first-line-p ()
  "Return non-nil if on the first line in buffer."
  (save-excursion (beginning-of-line) (bobp)))

(defun diogenes--last-line-p ()
  "Return non-nil if on the last line in buffer."
  (save-excursion (end-of-line) (eobp)))

(defun diogenes-undo ()
  "Undo also when buffer is readonly."
  (interactive)
  (let ((inhibit-read-only t))
    (undo)))

(provide 'diogenes-lisp-utils)

;;; diogenes-lisp-utils.el ends here
