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

;; ----------------------------------------------------------------------
;;;; Diogenes Search Mode

(defvar diogenes-search-mode-map
  (let ((map (nconc (make-sparse-keymap) text-mode-map)))
    (keymap-set map "RET" #'diogenes-search-browse-passage)
    (keymap-set map "C-c C-c" #'diogenes-search-browse-passage)
    (keymap-set map "<double-mouse-1>" #'diogenes-search-browse-passage)
    (keymap-set map "n" #'diogenes-search-next)
    (keymap-set map "p" #'diogenes-search-previous)
    (keymap-set map "d" #'diogenes-search-delete)
    (keymap-set map "u" #'diogenes-undo)
    ;; (keymap-set map "<mouse-2>" #'diogenes-search-browse-passage)
    ;; (keymap-set map "C-c C-q" #'diogenes-browser-quit)
    ;; ;; Utilities
    ;; (keymap-set map "C-c C--" #'diogenes-browser-remove-hyphenation)
    ;; (keymap-set map "C-c C-+" #'diogenes-browser-reinsert-hyphenation)
    ;; (keymap-set map "C-c C-t" #'diogenes-browser-toggle-citations)
    map)
  "Basic mode map for the Diogenes Search.")

;; Buffer Local Var: diogenes-search-status: body, match, header, nil

(define-derived-mode diogenes-search-mode text-mode "Diogenes Search"
  "Major mode to search Diogenes' databases."
  (make-local-variable 'diogenes--search-active-block)
  (make-local-variable 'diogenes--search-language)
  (make-local-variable 'diogenes--search-corpus)
  (setq buffer-read-only t)
  (visual-line-mode))

;;;; --------------------------------------------------------------------
;;;; SIMPLE SEARCH
;;;; --------------------------------------------------------------------

(defun diogenes--search-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((eob (and (eobp)
		      (not (bobp))
		      (point)))
	    (inhibit-read-only t))
	(save-excursion
	  (goto-char (process-mark proc))
	  (save-restriction
	    (narrow-to-region (point) (point))
	    (insert string)
	    (set-marker (process-mark proc) (point))
	    (goto-char (point-min))
	    ;; Post-processing
	    (mapc (lambda (subst)
		    (save-excursion
		      (while (re-search-forward (car subst) nil t)
			(replace-match (cdr subst)))))
		  '((" " . "")
		    ("~" . "_")))
	    ;; handle header & body blocks
	    (save-excursion
	      (while (not (eobp))
		(let* ((blk diogenes--search-active-block)
		       (start (point))
		       (end (or (cl-case blk
				  ((body)
				   (and (re-search-forward "_\n"
							   nil t)
					(setq diogenes--search-active-block
					      'header)
					(match-end 0)))
				  ((header)
				   (and (re-search-forward "\n\n"
							   nil t)
					(setq diogenes--search-active-block
					      'body)
					(match-end 0))))
				(goto-char (point-max)))))
		  (add-text-properties
		   start end
		   (cl-case blk
		     (header '(header t font-lock-face shr-h2))
		     (body '(body t)))))))))
	(when eob (goto-char (process-mark proc)))))))

;; TODO: Handle final count
(defun diogenes--search-sentinel (process event)
  (with-current-buffer (process-buffer process)
    (pcase event
      ("finished\n"
       (save-excursion
	 (let ((inhibit-read-only t))
	   (goto-char (point-max))
	   (re-search-backward "\nPassages found: \\([0-9]+\\).*")
	   (replace-match (concat "\nPassages found: "
				  (propertize (match-string-no-properties 1)
					      'font-lock-face 'success)))))))))

;; Entry handling
(defun diogenes--search-get-entry (pos)
  "Get the boundaries of the active passage.
Returns the start position, the boundary between header and body,
and the end as alist."
  (cond ((get-text-property pos 'header)
	 (let* ((start (or (and (> pos (point-min))
				(get-text-property (1- pos) 'body)
				pos)
			   (previous-single-property-change pos 'header)
			   (point-min)))
		(dlm (next-single-property-change pos 'header))
		(end (or (next-single-property-change dlm 'body)
			 (point-max))))
	   (list start dlm end)))
	((get-text-property pos 'body)
	 (let* ((end (or (next-single-property-change pos 'body)
			 (point-max)))
		(dlm (or (and (get-text-property (1- pos) 'header)
			      (1- pos))
			 (previous-single-property-change pos 'header)))
		(start (or (previous-single-property-change dlm 'header)
			   (point-min))))
	   (list start dlm end)))
	(t (error "No entry at point"))))

(defun diogenes--search-get-header-lines (pos)
  "Get the header of the active passage, as s list of lines."
  (let ((entry (diogenes--search-get-entry pos)))
    (split-string (string-trim (buffer-substring (car entry)
						 (cadr entry)))
		  "\n")))

(defun diogenes--search-next-entry (pos)
  "Returns the beginning and end of the entry after POS."
  (let ((entry (diogenes--search-get-entry pos)))
    (if (or (>= (caddr entry) (point-max))
	    (not (get-text-property (1+ (caddr entry))
				    'header)))
	(error "No further entry!")
      (diogenes--search-get-entry (1+ (caddr entry))))))

(defun diogenes--search-previous-entry (pos)
  "Returns the beginning and end of the entry before POS."
  (let ((entry (diogenes--search-get-entry pos)))
    (if (<= (car entry) (point-min))
	(error "No previous entry!")
      (diogenes--search-get-entry (1- (car entry))))))

(defun diogenes--search-delete-entry (pos)
  "Deletes the entry at POS.
This function makes sure that the full citation remains accessible."
  (let ((entry (diogenes--search-get-entry pos))
	(header-lines (diogenes--search-get-header-lines pos))
	(inhibit-read-only t))
    (when-let* ((full-cit (string-match "(\\([0-9]+\\): \\([0-9]+\\)) *$"
					(car header-lines)))
		(next-entry (ignore-errors (diogenes--search-next-entry pos)))
		(next-entry-short-cit (not (string-match "(\\([0-9]+\\): \\([0-9]+\\)) *$"
							 (buffer-substring-no-properties
							  (car next-entry)
							  (cadr next-entry))))))
      (goto-char (cadr next-entry))
      (beginning-of-line -1)
      (delete-region (+ 2 (car next-entry)) (point))
      (mapc (lambda (s) (insert s) (newline))
	    (butlast header-lines)))
    (delete-region (car entry) (caddr entry))
    (ignore-errors (goto-char (cadr (diogenes--search-get-entry (point)))))))


(defun diogenes--search-get-citation (pos)
  "Get the full citation of the entry at point."
  (let* ((header-lines (diogenes--search-get-header-lines pos))
	 (cit (mapcar (lambda (s) (and (string-match "\\S-+$" s)
				  (match-string 0 s)))
		      (split-string (car (last header-lines))
				    ",")))
	 (author-and-work (if (string-match "(\\([0-9]+\\): \\([0-9]+\\)) *$"
					    (car header-lines))
			      (list (match-string 1 (car header-lines))
				    (match-string 2 (car header-lines)))
			    (save-excursion
			      (if (re-search-backward
				   "(\\([0-9]+\\): \\([0-9]+\\)) *$")
				  (list (match-string-no-properties 1)
					(match-string-no-properties 2))
				(error "Could not find full citation!"))))))
    (nconc author-and-work cit)))



;;; Search mode commands
(defun diogenes-search-next (pos)
  "Move to the beginning of the next passage found."
  (interactive "d")
  (goto-char (cadr (diogenes--search-next-entry pos))))

(defun diogenes-search-previous (pos)
  "Move to the beginning of the previous passage found."
  (interactive "d")
  (goto-char (cadr (diogenes--search-previous-entry pos))))

(defun diogenes-search-delete (pos)
  "Delete the passage at point."
  (interactive "d")
  (diogenes--search-delete-entry pos))

(defun diogenes-search-browse-passage (pos)
  "Open the passage at point in the browser"
  (interactive "d")
  (diogenes--browse-work (list :type diogenes--search-corpus)
			 (diogenes--search-get-citation pos)))




(defun diogenes--do-search (options &optional authors)
  "Function that executes a search query in the Diogenes Databases."
  (diogenes--start-perl "search"
			(diogenes--search-script options authors)
			#'diogenes--search-filter
			#'diogenes--search-sentinel)
  (diogenes-search-mode)
  (setq diogenes--search-active-block 'header)
  (setq diogenes--search-corpus (plist-get options :type))
  (setq diogenes--search-language
	(pcase diogenes--search-corpus
	  ("tlg" "greek")
	  ("phi" "latin"))))

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

