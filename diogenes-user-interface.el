;;; diogenes-user-interface.el --- User interface for diogenes.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Michael Neidhart
;;
;; Author: Michael Neidhart <mayhoth@gmail.com>
;; Keywords: classics, tools, philology, humanities

;;; Commentary:

;; This file contains functions that provide the user interface of diogenes.el

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'diogenes-lisp-utils)

;;; Selectors
(defun diogenes--select-author-num (options &optional author-regex)
  "Select one author from a diogenes database using a prompt."
  (let ((author-list (diogenes--get-author-list options author-regex)))
    (cadr (assoc (completing-read "Author: " author-list)
		 author-list))))

(defun diogenes--select-work-num (options author)
  "Select a single work from an author in a Diogenes database."
  (let ((works-list (diogenes--get-works-list options
					      author)))
    (cadr (assoc (completing-read "Work: "
				  works-list)
		 works-list))))

(defun diogenes--select-passage (options author work)
  "Select a specific passage from a given work in the Diogenes database."
  (let ((work-labels (diogenes--get-work-labels options (list author work))))
    (cl-loop for label in work-labels
	     collect (read-string (format "%s: " label)))))

(defun diogenes--select-tlg-categories ()
  (let* ((categories (diogenes--get-tlg-categories))
	 (category (intern
		    (completing-read "Select an category: "
				     (diogenes--plist-keys categories)))))
    (completing-read "Please select: " (plist-get categories category))))


;;; TODO: Selection with multiple regexes
(defun diogenes--select-author-nums (options &optional author-regex)
  "Select a list of authors from a diogenes database using a prompt.
Returns a list."
  (cl-loop collect (diogenes--select-author-num options author-regex)
	   while (y-or-n-p "Add another author?")))

;;; TODO: Selection with transient
(defun diogenes--select-authors-and-works (options &optional author-regex)
  "Select a list of authors and works from a diogenes database using a prompt.
Returns a plist."
  ())


(provide 'diogenes-user-interface)

;;; diogenes-user-interface.el ends here

