;;; diogenes-forms.el --- Form selection for diogenes.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Michael Neidhart
;;
;; Author: Michael Neidhart <mayhoth@gmail.com>
;; Keywords: classics, tools, philology, humanities

;;; Commentary:

;; This file contains functions that construct form lists for Diogenes

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'replace)
(require 'transient)
(require 'diogenes-lisp-utils)
(require 'diogenes-perseus)

(defun diogenes--change-form-entry (pos properties &optional form-string-face mark)
  "Change the form entry at POS in `diogenes--select-forms'.
MARK should contain the marker string, FORM-STRING-FACE the face
that fontifies the form itself."
  (when (get-text-property pos 'form)
    (let ((inhibit-read-only t)
	  (range (diogenes--get-text-prop-boundaries pos 'form)))
      (add-text-properties (car range) (cadr range)
			   properties)
      (save-excursion
	(save-restriction
	  (narrow-to-region (car range) (cadr range))
	  (let ((form-region (and (goto-char (point-min))
				  (text-property-search-forward 'form-string)))
		(mark-region (and (goto-char (point-min))
				  (text-property-search-forward 'mark))))
	    (when form-string-face
	      (put-text-property (prop-match-beginning form-region)
				 (prop-match-end form-region)
				 'font-lock-face form-string-face))
	    (when mark
	      (goto-char (prop-match-end mark-region))
	      (insert-and-inherit mark)
	      (delete-region (prop-match-beginning mark-region)
			     (prop-match-end mark-region)))))))))

(defun diogenes--goto-next-form ()
  (interactive)
  (goto-char (or (next-single-property-change (point) 'form)
		 (point-max))))

(defun diogenes--select-form-at-point ()
  (interactive)
  (diogenes--change-form-entry (point) (list 'selected t
					'marked nil)
			       'bold
			       (propertize "✓" 'font-lock-face 'success))
  (diogenes--goto-next-form))

(defun diogenes--unselect-form-at-point ()
  (interactive)
  (diogenes--change-form-entry (point)
			       (list 'selected nil
				     'marked nil)
			       'font-lock-comment-face
			       " ")
    (diogenes--goto-next-form))

(defun diogenes--mark-form-at-point ()
  (interactive)
  (diogenes--change-form-entry (point)
			       (list 'marked t)
			       'highlight)
  (diogenes--goto-next-form))

(defun diogenes--unmark-form-at-point ()
  (interactive)
  (diogenes--change-form-entry (point)
			       (list 'marked nil)
			       (if (get-text-property (point) 'selected)
				   'bold
				 'font-lock-comment-face))
    (diogenes--goto-next-form))

(defun diogenes--toggle-form-at-point ()
  (interactive)
  (if (get-text-property (point) 'selected)
      (diogenes--unselect-form-at-point)
    (diogenes--select-form-at-point)))

(defun diogenes--select-all-forms ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (diogenes--select-form-at-point))))

(defun diogenes--unselect-all-forms ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (diogenes--unselect-form-at-point))))

(defun diogenes--mark-all-forms ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (diogenes--mark-form-at-point))))

(defun diogenes--unmark-all-forms ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (diogenes--unmark-form-at-point))))

(defun diogenes--execute-on-marked-forms (function)
  (save-excursion
    (goto-char (point-min))
    (cl-loop for match = (text-property-search-forward 'marked)
	     while match
	     do (progn (goto-char (prop-match-beginning match))
		       (funcall function)))))

(defun diogenes--select-marked-forms ()
  (interactive)
  (diogenes--execute-on-marked-forms #'diogenes--select-form-at-point))

(defun diogenes--unselect-marked-forms ()
  (interactive)
  (diogenes--execute-on-marked-forms #'diogenes--unselect-form-at-point))

(defun diogenes--select-marked-or-all-forms ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (text-property-search-forward 'marked)
	(diogenes--select-marked-forms)
      (diogenes--select-all-forms))))

(defun diogenes--unselect-marked-or-all-forms ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (text-property-search-forward 'marked)
	(diogenes--unselect-marked-forms)
      (diogenes--unselect-all-forms))))

(defun diogenes--match-forms-and-execute (regexp command)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (and (get-text-property (point) 'form)
	       (string-match regexp
			     (apply #'buffer-substring-no-properties 
				    (diogenes--get-text-prop-boundaries (point)
									'form))))
	  (funcall command)
	(diogenes--goto-next-form)))))

(defun diogenes--regexp-mark-forms (regexp)
  (interactive (list (read-regexp "Mark forms matching regexp: ")))
  (diogenes--match-forms-and-execute regexp #'diogenes--mark-form-at-point))

(defun diogenes--regexp-unmark-forms (regexp)
  (interactive (list (read-regexp "Unmark forms matching regexp: ")))
  (diogenes--match-forms-and-execute regexp #'diogenes--unmark-form-at-point))

(defun diogenes--regexp-select-forms (regexp)
  (interactive (list (read-regexp "Mark forms matching regexp: ")))
  (diogenes--match-forms-and-execute regexp #'diogenes--select-form-at-point))

(defun diogenes--regexp-unselect-forms (regexp)
  (interactive (list (read-regexp "Unmark forms matching regexp: ")))
  (diogenes--match-forms-and-execute regexp #'diogenes--unselect-form-at-point))

(defun diogenes--submit-forms ()
  "Submit the selected forms in the Diogenes Select Forms buffer.
The forms are passed as a list to the function saved in
`diogenes--select-forms-function'."
  (interactive)
  (unless (eq major-mode 'diogenes-select-forms-mode)
    (error "Not in `Diogenes Select Forms'!"))
  (let ((forms (cl-loop initially (goto-char (point-min))
			for region = (text-property-search-forward 'form)
			while region
			for beg = (prop-match-beginning region)
			if (get-text-property beg 'selected)
			collect (get-text-property beg 'form)))
	(function diogenes--select-forms-function))
    (kill-buffer)
    (funcall function forms)))

;; (makunbound 'diogenes-select-forms-mode-map)
(defvar diogenes-select-forms-mode-map
  (let ((map (nconc (make-sparse-keymap) text-mode-map)))
    (keymap-set map "RET"     #'diogenes--toggle-form-at-point)
    (keymap-set map "m"       #'diogenes--select-form-at-point)
    (keymap-set map "s"       #'diogenes--select-form-at-point)
    (keymap-set map "d"       #'diogenes--unselect-form-at-point)
    (keymap-set map "m"       #'diogenes--mark-form-at-point)
    (keymap-set map "u"       #'diogenes--unmark-form-at-point)
    (keymap-set map "S"       #'diogenes--select-marked-or-all-forms)
    (keymap-set map "D"       #'diogenes--unselect-marked-or-all-forms)
    (keymap-set map "M"       #'diogenes--mark-all-forms)
    (keymap-set map "U"       #'diogenes--unmark-all-forms)
    (keymap-set map "%"       #'diogenes--regexp-mark-forms)
    (keymap-set map "!"       #'diogenes--regexp-unmark-forms)
    (keymap-set map "c"       #'diogenes--submit-forms)
    (keymap-set map "x"       #'diogenes--submit-forms)
    (keymap-set map "C-c C-c" #'diogenes--submit-forms)
    (keymap-set map "n"       #'next-line)
    (keymap-set map "p"       #'previous-line)
    (keymap-set map "f"       #'forward-char)
    (keymap-set map "b"       #'backward-char)
    (keymap-set map "q"       #'diogenes--quit)
    map)
  "Mode map for the Diogenes Select Forms.")

(define-derived-mode diogenes-select-forms-mode text-mode "Select Forms"
  "Mode for the forms selection buffer."
  (make-local-variable 'diogenes--select-forms-lang)
  (make-local-variable 'diogenes--select-forms-function)
  (setq buffer-read-only t))

(defun diogenes--select-forms (query lang callback &optional header)
  "Let the user select forms of a LEMMA.
Then call CALLBACK with this list as its single argument."
  (let* ((choices (or (diogenes--get-all-forms query lang)
		      (error "No results for %s" query)))
	 (lemma (if (= (length choices) 1)
		    (car choices)
		  (let ((completion-extra-properties
			 '(:annotation-function
			   (lambda (s)
			     (concat
			      "\t("
			      (string-join
			       (mapcar #'car
				       (cdddr
					(assoc s minibuffer-completion-table)))
			       " ")
			      ")")))))
		    (assoc (completing-read "Please select the correct lemma: "
					    choices nil t)
			   choices))))
	 (buffer (diogenes--get-fresh-buffer "select-forms"))
	 (inhibit-read-only t))
    (pop-to-buffer buffer)
    (diogenes-select-forms-mode)
    ;; TODO: Code duplication with diogenes--format-lemma-and-forms 
    (insert (propertize (or header
			    "Search for the following words:")
			'font-lock-face 'shr-h1))
    (newline 2)
    (save-excursion
      (insert (cl-loop
	       for (form . analyses) in (cdddr lemma) concat
	       (propertize (concat (propertize "✓"
					       'mark t
					       'font-lock-face 'success)
				   " "
				   (propertize (format "%-18s " form)
					       'form-string t
					       'font-lock-face 'bold)
				   " "
				   (propertize (car analyses)
					       'font-lock-face 'italic)
				   "\n"
				   (cl-loop
				    for a in (cdr analyses) concat
				    (concat (make-string 22 ? )
					    (propertize a
							'font-lock-face 'italic)
					    "\n")))
			   'form form
			   'selected t))))
    (setq diogenes--select-forms-lang lang
	  diogenes--select-forms-function callback)
    nil))

(defun diogenes--select-from-tlg-wordlist (query callback &optional header)
  "Let the user select entries that match QUERY in the TLG word-list"
  (let* ((raw-entries
	  (or (diogenes--get-wordlist-matches '(:type "tlg")
					      (diogenes--utf8-to-beta query))
	     (error "No results for %s" query)))
	 (entries
	  (mapcar (lambda (e)
		    (list (car e)
			  (diogenes--beta-to-utf8
			   (diogenes--replace-regexes-in-string (downcase (car e))
			     ("!" ".")))
			  (cadr e)))
		  raw-entries))
	 (buffer (diogenes--get-fresh-buffer "select-forms"))
	 (inhibit-read-only t))
    (pop-to-buffer buffer)
    (diogenes-select-forms-mode)
    (insert (propertize (or header
			    "Search for the following words:")
			'font-lock-face 'shr-h1))
    (newline 2)
    (save-excursion
      (insert (cl-loop
	       for (beta-form form frequency) in entries concat
	       (propertize (concat (propertize "✓"
					       'mark t
					       'font-lock-face 'success)
				   " "
				   (propertize (format "%-18s " form)
					       'form-string t
					       'font-lock-face 'bold)
				   " "
				   (propertize frequency
					       'font-lock-face 'italic)
				   "\n")
			   'form beta-form
			   'selected t))))
    (setq diogenes--select-forms-lang "greek"
	  diogenes--select-forms-function callback)))

(provide 'diogenes-forms)

;;; diogenes-forma.el ends here

