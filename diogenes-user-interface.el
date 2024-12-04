;;; diogenes-user-interface.el --- User interface for diogenes.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Michael Neidhart
;;
;; Author: Michael Neidhart <mayhoth@gmail.com>
;; Keywords: classics, tools, philology, humanities

;;; Commentary:

;; This file contains functions that provide the user interface of diogenes.el

;;; Code:

;;; Runners
(defun diogenes--do-search (options &optional authors)
  "Function that executes a search query in the Diogenes Databases."
  (diogenes--start-perl "search"
			(diogenes--search-script options authors)))

(defun diogenes--dump-work (options passage)
  "Function that dumps a work from the Diogenes Databases.

Passage has to be a list of strings containing the four digit
number of the author and the number of the work."
  (diogenes--start-perl "dump"
			(diogenes--browser-script
			 (append options '(:browse-lines 1000000))
			 passage)))

(defun diogenes--browse-work (options passage)
  "Function that browses a work from the Diogenes Databases.

Passage has to be a list of strings containing the four digit
number of the author and the number of the work."
  ;; (diogenes--make-comint 'diogenes-browser-mode
  ;; 			 (diogenes--browse-interactivly-script options
  ;; 							       passage))
  (diogenes--start-perl "browser"
			(diogenes--browse-interactively-script options passage)
			#'diogenes--browser-filter)
  (diogenes-browser-mode))



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

