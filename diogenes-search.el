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
(require 'replace)
(require 'transient)
(require 'diogenes-lisp-utils)
(require 'diogenes-user-interface)
(require 'diogenes-perseus)

(require 'diogenes-forms)
(require 'diogenes-corpora)



;;----------------------------------------------------------------------
;;;; HELPER FUNCTIONS

;; (defun diogenes--process-pattern (pattern)
;;   "Make sure a pattern is all ASCII. When dealing with greek
;; unicode, convert it to beta code and correctly escape the regex metacharacters."
;;   (save-match-data
;;     (with-temp-buffer
;;       (save-excursion (insert pattern))
;;       (while (re-search-forward "\\cg+" nil t)
;; 	(insert (replace-regexp-in-string "[)(+*\\\\]" "\\\\\\&"
;; 					  (diogenes--utf8-to-beta (match-string 0))))
;; 	(delete-region (match-beginning 0) (match-end 0)))
;;       (buffer-string))))

(defun diogenes--process-pattern (pattern)
  "Dummmy function."
  pattern)

(defun diogenes--probable-corpus-language (type)
  "Determine the probable language of a corpus."
  (pcase type
    ("tlg" "greek")
    ("phi" "latin")
    (_ (error "Language not yet implemented for %s" type))))



;;----------------------------------------------------------------------
;;; Diogenes Search Mode

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
				  (match-string-no-properties 0 s)))
		      (split-string (car (last header-lines))
				    ",")))
	 (author-and-work (if (string-match "(\\([0-9]+\\): \\([0-9]+\\)) *$"
					    (car header-lines))
			      (list (match-string-no-properties 1 (car header-lines))
				    (match-string-no-properties 2 (car header-lines)))
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




;; --------------------------------------------------------------------
;;; SIMPLE SEARCH
(defun diogenes--search-sentinel (process event)
  (when (buffer-live-p (process-buffer process))
   (with-current-buffer (process-buffer process)
     (pcase event
       ("finished\n"
	(save-excursion
	  (let ((inhibit-read-only t))
	    (goto-char (point-max))
	    (re-search-backward "\nPassages found: \\([0-9]+\\).*")
	    (replace-match (concat "\nPassages found: "
				   (propertize (match-string-no-properties 1)
					       'font-lock-face 'success))))))))))

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
	(diogenes--probable-corpus-language diogenes--search-corpus)))

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
							  collect
							  (cons (format "%d lines" i)
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


;;--------------------------------------------------------------------
;;;; WORDLIST SEARCH
(defun diogenes--indexed-search-sentinel (process event)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (pcase event
	("finished\n"
	 (save-excursion
	   (let ((inhibit-read-only t))
	     (goto-char (point-max))
	     (re-search-backward
	      "\nIncidence of all words as reported by word list: \\([0-9]+\\).*")
	     (replace-match (concat
			     "\nIncidence of all words as reported by word list: "
			     (propertize (match-string-no-properties 1)
					 'font-lock-face 'success)))
	     (re-search-forward
	      "\nPassages containing those words reported by Diogenes: \\([0-9]+\\).*")
	     (replace-match
	      (concat "\nPassages containing those words reported by Diogenes: "
		      (propertize (match-string-no-properties 1)
				  'font-lock-face 'success))))))))))

(defun diogenes--do-wordlist-search (options words &optional authors)
  "Function that executes an indexed search query in the Diogenes Databases."
  (diogenes--start-perl "search"
			(diogenes--indexed-search-script options words authors)
			#'diogenes--search-filter
			#'diogenes--indexed-search-sentinel)
  (diogenes-search-mode)
  (setq diogenes--search-active-block 'header)
  (setq diogenes--search-corpus (plist-get options :type))
  (setq diogenes--search-language
	(diogenes--probable-corpus-language diogenes--search-corpus)))



;;--------------------------------------------------------------------
;;;; MORPHOLOGICAL SEARCH
(defun diogenes--lemma-forms-list (lemma lang)
  "Returns a list of all attested forms of LEMMA in LANG."
  (let ((entry (let ((lemmata (or (diogenes--get-all-forms lemma lang)
				  (error "No results for %s" lemma))))
		 (if (> (length lemmata) 1)
		     (assoc (completing-read "Please choose a lemma: "
					     lemmata)
			    lemmata)
		   (car lemmata)))))
    (cl-remove-duplicates (mapcar #'car (cdddr entry)))))

(defun diogenes--lemma-regexp (lemma lang &optional retain-diacritics)
  "Returns a regexp that matches all forms of a given lemma."
  (regexp-opt
   (mapcar (lambda (e)
	     (funcall (if retain-diacritics
			  #'identity
			#'diogenes--strip-diacritics)
		      (funcall (if (string= lang "greek")
				   #'diogenes--beta-to-utf8
				 #'identity)
			       e)))
	   (diogenes--lemma-forms-list lemma lang))))

(defun diogenes--morphological-search (type lemma-or-forms &optional authors)
  (cl-typecase lemma-or-forms
    (string
     (diogenes--select-forms lemma-or-forms
			     (diogenes--probable-corpus-language type)
			     (lambda (l) (diogenes--morphological-search type
								    l
								    authors))
			     (format "Search for the following forms of %s:"
				     lemma-or-forms)))
    (list 
     (let ((options (list :type type))
	   (forms (mapcar #'diogenes--utf8-to-beta lemma-or-forms))
	   (author-plist (when authors (list :author-nums authors))))
       (diogenes--do-wordlist-search options forms author-plist)))
    (t (error "Invalid type %s" lemma-or-forms))))



;;--------------------------------------------------------------------
;;;; TRANSIENT DISPATCHER
(transient-define-prefix diogenes--search-test ()
  "A simple test dispatcher."
  ["Test Dispatcher"
   ("g" "Search TLG"
    (lambda () (interactive) (transient-setup 'diogenes--search--select-mode nil nil :scope "tlg"))
    :transient transient--do-recurse)
   ("l" "Search PHI"
    (lambda () (interactive) (transient-setup 'diogenes--search--select-mode nil nil :scope "phi"))
    :transient transient--do-recurse)
   ("d" "Search the Duke Documentary Papyri"
    (lambda () (interactive) (transient-setup 'diogenes--search--select-mode nil nil nil nil :scope "ddp"))
    :transient transient--do-recurse)
   ("i" "Search the Classical Inscriptions"
    (lambda () (interactive) (transient-setup 'diogenes--search--select-mode nil nil :scope "ins"))
    :transient transient--do-recurse)
   ("c" "Search the Christian Inscriptions"
    (lambda () (interactive) (transient-setup 'diogenes--search--select-mode nil nil :scope "chr"))
    :transient transient--do-recurse)
   ("m" "Search the Miscellaneous PHI Texts"
    (lambda () (interactive) (transient-setup 'diogenes--search--select-mode nil nil :scope "misc"))
    :transient transient--do-recurse)
   ])

(transient-define-prefix diogenes--search--select-mode (type)
  "Select the mode of the search, and dispatch."
  [:description
   (lambda () (format "Select the mode for searching the %s" (upcase (transient-scope))))
   [("s" "Simple" diogenes--simple-search)
    ("a" "Advanced" diogenes--advanced-search)
    ("l" "Lemma" diogenes--lemma-search)
    ("w" "Wordlist" diogenes--wordlist-search)]]
  (interactive (list (if transient-current-command (transient-scope)
		       (diogenes--select-database))))
  (transient-setup 'diogenes--search--select-mode nil nil :scope type))

(transient-define-argument diogenes--search-pattern ()
  "Select a pattern"
  :class 'transient-option
  :argument "pattern "
  :format "%k %d %v"
  :allow-empty nil
  :always-read t
  :init-value (lambda (obj) (oset obj value
			     (read-from-minibuffer "Enter search term: "))))

(transient-define-argument diogenes--search-mode-infix ()
  "Infix for choosing the search mode."
  :class 'transient-option
  :allow-empty nil
  :argument ""
  ;; :argument-format "%s"
  ;; :argument-regexp "simple\\|wordlist\\|lemma"
  :choices '("simple" "wordlist" "lemma")
  :init-value (lambda (obj) (oset obj value "simple"))
  :format "%k %d %v"
  :always-read t
  :unsavable t)

(transient-define-prefix diogenes--simple-search (type)
  "Start a simple search."
  [:description
   (lambda () (format "SIMPLE %s SEARCH:" (upcase (transient-scope))))
   [("p" "Change pattern" "pattern="
     :allow-empty nil
     :init-value (lambda (obj) (oset obj value (read-from-minibuffer "Enter search term: ")))
     :always-read t)
    ("s" "Do search"
    (lambda (type args)
      (interactive (list (transient-scope)
			 (transient-args 'diogenes--simple-search)))
      (message "TYPE: %s\nARGS: %s" type args)))]]
  (interactive (list (if transient-current-command (transient-scope)
		       (diogenes--select-database))))
  (transient-setup 'diogenes--simple-search nil nil :scope type))

(transient-define-prefix diogenes--advanced-search (type pattern)
  "Start a advanced search."
  [:description
   (lambda () (format "Advanced %s Search:" (upcase (transient-scope))))
   [("m" "Change search mode:" diogenes--search-mode-infix)
    ("p" "Change pattern:" diogenes--search-pattern)
    ("s" "Do search"
    (lambda (type args)
      (interactive (list (transient-scope)
			 (transient-args 'diogenes--search-dispatcher)))
      (message "TYPE: %s\nARGS: %s" type args)))]]
  (interactive (list (if transient-current-command (transient-scope)
		       (diogenes--select-database))
		     (read-from-minibuffer "Enter search term: ")))
  (transient-setup 'diogenes--advanced-search nil nil :scope type))

(transient-define-prefix diogenes--lemma-search (type pattern)
  "Start a lemma search."
  [:description
   (lambda () (format "Lemma %s Search:" (upcase (transient-scope))))
   [("m" "Change search mode:" diogenes--search-mode-infix)
    ("p" "Change pattern:" diogenes--search-pattern)
    ("s" "Do search"
    (lambda (type args)
      (interactive (list (transient-scope)
			 (transient-args 'diogenes--search-dispatcher)))
      (message "TYPE: %s\nARGS: %s" type args)))]]
  (interactive (list (if transient-current-command (transient-scope)
		       (diogenes--select-database))
		     (read-from-minibuffer "Enter search term: ")))
  (transient-setup 'diogenes--lemma-search nil nil :scope type))

(transient-define-prefix diogenes--wordlist-search (type pattern)
  "Start a wordlist search."
  [:description
   (lambda () (format "Wordlist %s Search:" (upcase (transient-scope))))
   [("m" "Change search mode:" diogenes--search-mode-infix)
    ("p" "Change pattern:" diogenes--search-pattern)
    ("s" "Do search"
    (lambda (type args)
      (interactive (list (transient-scope)
			 (transient-args 'diogenes--search-dispatcher)))
      (message "TYPE: %s\nARGS: %s" type args)))]]
  (interactive (list (if transient-current-command (transient-scope)
		       (diogenes--select-database))
		     (read-from-minibuffer "Enter search term: ")))
  (transient-setup 'diogenes--wordlist-search nil nil :scope type))




(provide 'diogenes-search)

;;; diogenes-search.el ends here

