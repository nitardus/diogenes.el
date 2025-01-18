;;; diogenes-corpora.el --- Edit search corpora for diogenes.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Michael Neidhart
;;
;; Author: Michael Neidhart <mayhoth@gmail.com>
;; Keywords: classics, tools, philology, humanities

;;; Commentary:

;; This file contains functions for managing custom corpora for Diogenes

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'replace)
(require 'transient)
(require 'diogenes-lisp-utils)
(require 'diogenes-user-interface)

;;; EDIT EXISTING CORPORA
(defvar diogenes--user-corpora []
  "The user defined subcorpora of the Diogenes databases.")

(defun diogenes--load-user-corpora ()
  (unless (json-available-p)
    (error "JSON support needed to load user defined corpora!"))
  (let ((filter-file (or (diogenes--get-filter-file)
			 (error "Unable to locate filter file!"))))
    (setq diogenes--user-corpora
	  (if (file-exists-p filter-file)
	      (with-temp-buffer
		(insert-file-contents filter-file)	
		(json-parse-buffer :object-type 'plist
				   :array-type 'array))))))

(defun diogenes--select-user-corpus (&optional type)
  "Select a saved user corpus interactively."
  (diogenes--load-user-corpora)
  (let ((corpora (mapcar (lambda (c) (cons (plist-get c :name) c))
			 (if type
			     (cl-remove-if-not
			      (lambda (c) (string= (plist-get c :type)
					      type))
			      diogenes--user-corpora)
			   diogenes--user-corpora))))
    (if corpora
	(cdr (assoc (completing-read "Saved user corpora: " corpora nil t)
		    corpora))
      (message "No saved corpora!"))))

(defun diogenes--user-corpora-exist-p (&optional type)
  "Return non-nil if there is a corpus defined of type TYPE.
If no type is given, check wheter there is any defined corpus."
  (let ((corpora (if type
		     (cl-remove-if-not (lambda (c) (string= (plist-get c :type)
						       type))
						    diogenes--user-corpora)
		   diogenes--user-corpora)))
    (and (not (zerop (length corpora)))
	 t)))

(defun diogenes--remove-user-corpus (index)
  "Removes corpus with index INDEX from diogenes--user-corpora."
  (setf (elt diogenes--user-corpora index) nil))

(defun diogenes--save-user-corpora ()
  (unless (json-available-p)
    (error "JSON support needed to save user defined corpora!"))
  (let ((filter-file (or (diogenes--get-filter-file)
			 (error "Unable to locate filter file!"))))
    (setq diogenes--user-corpora
	  (remove nil diogenes--user-corpora))
    (with-temp-file filter-file
      (json-insert diogenes--user-corpora))))

;; (makunbound 'diogenes-corpus-mode-map)
(defvar diogenes-corpus-mode-map
  (let ((map (nconc (make-sparse-keymap) text-mode-map)))
    (keymap-set map "+"   #'diogenes-add-user-corpus)
    (keymap-set map "a"   #'diogenes--add-author-to-corpus)
    (keymap-set map "w"   #'diogenes--add-work-to-corpus)
    (keymap-set map "d"   #'diogenes--corpus-remove-element)
    (keymap-set map "D"   #'diogenes--corpus-delete-corpus)
    (keymap-set map "c"   #'diogenes--corpus-make-duplicate)
    ;; (keymap-set map "m"   #'diogenes--corpus-merge)
    (keymap-set map "r"   #'diogenes--corpus-rename)
    (keymap-set map "n"   #'next-line)
    (keymap-set map "p"   #'previous-line)
    (keymap-set map "f"   #'forward-char)
    (keymap-set map "b"   #'backward-char)
    (keymap-set map "b"   #'backward-char)
    (keymap-set map "u"   #'diogenes-undo)
    (keymap-set map "C-c C-c" #'diogenes--corpora-save)
    (keymap-set map "q"   #'diogenes--ask-and-quit)
    map)
  "Mode map for the Diogenes Manage Corpora.")

(define-derived-mode diogenes-corpus-mode text-mode "Diogenes Manage Corpora"
  "Mode for editing custom Diogenes corpora."
  (setq buffer-read-only t))

(defvar diogenes-corpus-edit-mode-map
  (let ((map (nconc (make-sparse-keymap) text-mode-map)))
    (keymap-set map "a"       #'diogenes--add-author-to-corpus)
    (keymap-set map "w"       #'diogenes--add-work-to-corpus)
    (keymap-set map "d"       #'diogenes--corpus-remove-element)
    (keymap-set map "r"       #'diogenes--corpus-rename)
    (keymap-set map "n"       #'next-line)
    (keymap-set map "p"       #'previous-line)
    (keymap-set map "f"       #'forward-char)
    (keymap-set map "b"       #'backward-char)
    (keymap-set map "b"       #'backward-char)
    (keymap-set map "u"       #'diogenes-undo)
    (keymap-set map "x"       #'diogenes--corpus-submit)
    (keymap-set map "C-c C-c" #'diogenes--corpus-submit)
    (keymap-set map "q"       #'diogenes--ask-and-quit)
    map)
  "Mode map for the Diogenes Edit Corpus.")

(define-derived-mode diogenes-corpus-edit-mode text-mode
  "Diogenes edit corpus"
  "Mode for editing one freshly created Diogenes corpora."
  (make-local-variable 'diogenes--corpus-edit-callback)
  (setq buffer-read-only t))

(defun diogenes--other-corpora-visited-p ()
  "Return t if more than one corpus is open in zhe current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (and (text-property-search-forward 'corpus-id)
	   (text-property-search-forward 'corpus-id)
	   t))))

(defun diogenes--get-defined-corpus-names ()
  "Get all names of the corpora that are defined.
This function searches both diogenes--user-corpora and the
dedicated *diogenes-corpora* buffer."
  (cl-union
   (cl-loop for corpus across diogenes--user-corpora
	    when corpus collect
	    (plist-get corpus :name))
   (when (get-buffer "*diogenes-corpora*")
     (with-current-buffer "*diogenes-corpora*"
       (save-excursion
	 (goto-char (point-min))
	 (cl-loop for match = (text-property-search-forward 'corpus-name)
		  while match
		  collect (prop-match-value match)))))))

(defun diogenes--get-corpus-element (pos)
  (cond ((get-text-property pos 'work-num)
	 (diogenes--get-text-prop-boundaries pos 'work-num))
	((get-text-property pos 'author-num)
	 (diogenes--get-text-prop-boundaries pos 'author-num))))

(defun diogenes--print-user-corpus (corpus id)
  (let* ((authors (plist-get corpus :authors))
	 (type (plist-get corpus :type))
	 (name (plist-get corpus :name))
	 (print-name (or name "Anonymous corpus"))
	 (author-map (diogenes--get-author-list (list :type type))))
    (propertize 
     (concat
      (propertize (if (numberp id)
		      (format "[%d] %s (%s):\n\n" (1+ id) print-name type)
		    (format "[] %s (%s):\n\n" print-name type))
		  'font-lock-face 'shr-h2)
      (cl-typecase authors
	(vector
	 (cl-loop
	  for author-num across authors
	  for author = (car (diogenes--assoc-cadr author-num author-map))
	  concat (propertize (format "- %s\n" author)
			     'author-num author-num)))
	(list
	 (cl-loop
	  for (author-kw works) on authors by #'cddr
	  for author-num = (diogenes--keyword->string author-kw)
	  for author = (car (diogenes--assoc-cadr author-num author-map))
	  for works-map = (diogenes--get-works-list `(:type ,type)
						    author-num)
	  concat (propertize
		  (concat
		   (format "- %s\n" author)		      
		   (cond
		    ((vectorp works) (cl-loop
				      for work-num across works
				      for work = (car (diogenes--assoc-cadr
						       work-num works-map))
				      concat (propertize (format "  - %s\n" work)
							 'work-num work-num)))
		    ((eql works 1) "")
		    (t (error "Illegal value %s for author %s" works author))))
		  'author-num author-num)))))
     'corpus-id id
     'corpus-type type
     'corpus-name name)))

(defun diogenes--read-user-corpus (pos &optional buffer)
  "Read a corpus from BUFFER at POS and return it.
BUFFER defaults to the current buffer."
  (interactive "d")
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-restriction
	(let ((corpus-region (diogenes--get-text-prop-boundaries pos 'corpus-id))
	      (corpus-type (get-text-property pos 'corpus-type))
	      (corpus-name (get-text-property pos 'corpus-name)))
	  (apply #'narrow-to-region corpus-region)
	  (let ((works-p (progn (goto-char (point-min))
				(text-property-search-forward 'work-num)))
		(corpus nil))
	    (goto-char (point-min))
	    (cl-loop
	     for author-entry = (text-property-search-forward 'author-num)
	     while author-entry do
	     (let ((author-start (prop-match-beginning author-entry))
		   (author-end (prop-match-end author-entry))
		   (author-num (prop-match-value author-entry))
		   (works nil))
	       (if works-p
		   (save-excursion
		     (save-restriction
		       (narrow-to-region author-start author-end)
		       (goto-char (point-min))
		       (cl-loop for work-entry =
				(text-property-search-forward 'work-num)
				while work-entry do
				(push (prop-match-value work-entry) works))
		       (push (intern (concat ":" author-num)) corpus)
		       (push (if works (vconcat works) 1) corpus)))
		 (push author-num corpus))))
	    (list :name corpus-name
		  :type corpus-type
		  :authors (if works-p
			       (nreverse corpus)
			     (vconcat (nreverse corpus))))))))))

(defun diogenes--add-author-to-corpus (pos)
  (interactive "d")
  (when-let* ((corpus-region (diogenes--get-text-prop-boundaries
			      pos 'corpus-id))
	      (corpus-type (get-text-property pos 'corpus-type))
	      (corpus-id (get-text-property pos 'corpus-id))
	      (insert-pos
	       (save-excursion
		 (save-restriction
		   (apply #'narrow-to-region corpus-region)
		   (goto-char pos)
		   (or (next-single-property-change pos 'author-num)
		       (point-max)))))
	      (authors (diogenes--get-author-list (list :type corpus-type)))
	      (collection
	       (let ((sel
		      (save-excursion
			(save-restriction
			  (apply #'narrow-to-region corpus-region)
			  (goto-char (point-min))
			  (cl-loop
			   for match = (text-property-search-forward 'author-num)
			   while match collect (prop-match-value match))))))
		 (cl-remove-if (lambda (x) (member (cadr x) sel))
			       authors)))
	      (author (completing-read "Author: " collection nil t))
	      (entry
	       (propertize (format "- %s\n" author)
			   'corpus-type corpus-type
			   'corpus-id corpus-id
			   'author-num (cadr (assoc author authors))))
	      (inhibit-read-only t))
    (goto-char insert-pos)
    (insert entry)
    (backward-char)))

(defun diogenes--add-work-to-corpus (pos)
  (interactive "d")
  (when-let* ((properties (copy-list (text-properties-at pos)))
	      (corpus-type (plist-get properties 'corpus-type))
	      (author-region (diogenes--get-text-prop-boundaries
			      pos 'author-num))
	      (insert-pos
	       (save-excursion
		 (save-restriction
		   (apply #'narrow-to-region author-region)
		   (goto-char pos)
		   (or (next-single-property-change pos 'work-num)
		       (point-max)))))
	      (works (diogenes--get-works-list (list :type corpus-type)
					       (plist-get properties
							  'author-num)))
	      (collection
	       (let ((sel
		      (save-excursion
			(save-restriction
			  (apply #'narrow-to-region author-region)
			  (goto-char (point-min))
			  (cl-loop
			   for match = (text-property-search-forward 'work-num)
			   while match collect (prop-match-value match))))))
		 (cl-remove-if (lambda (x) (member (cadr x) sel))
			       works)))
	      (work (completing-read "Work: " collection nil t))
	      (entry
	       (apply #'propertize (format "  - %s\n" work)
		      (plist-put properties
				 'work-num (cadr (assoc work works)))))
	      (inhibit-read-only t))
    (goto-char insert-pos)
    (insert entry)
    (backward-char)))

(defun diogenes--corpus-remove-element (pos &optional limit)
  "Delete the element of a corpus at POS. 
If LIMIT is supplied, repeat for all entries until LIMIT.

 If called interactively, pos defaults to the point in the
current buffer. If the region is active or, remove all entries
inside the region."
  (interactive (if (use-region-p)
		   (list (region-beginning)
			 (region-end))
		 (list (point))))
  (let ((inhibit-read-only t))
    (seq-let (start end)
	(diogenes--get-corpus-element pos)
      (cl-loop while (and limit (< end limit))
	       for next-element = (diogenes--get-corpus-element (1+ end))
	       while next-element
	       do (setq end (cadr next-element)))
      (when (and start end)
	(delete-region start end)))))

(defun diogenes--corpus-rename ()
  "Rename the corpus at point."
  (interactive)
  (let* ((corpus-region (diogenes--get-text-prop-boundaries
			 (point) 'corpus-id))
	 (corpus-id (or (get-text-property (point) 'corpus-id)
			(error "No corpus under point!")))
	 (corpus-name (get-text-property (point) 'corpus-name))
	 (new-name (read-from-minibuffer (format "Rename %s to: "
						 (or corpus-name
						     "anonymous corpus"))))
	 (inhibit-read-only t))
    (when (member new-name (diogenes--get-defined-corpus-names))
      (error "Name `%s' already in use!" new-name))
    (put-text-property (car corpus-region)
		       (cadr corpus-region)
		       'corpus-name new-name)
    (save-excursion
      (goto-char (car corpus-region))
      (when (re-search-forward (concat "^\\[[0-9]*\\] \\(.*\\) ("
				       (regexp-opt diogenes--corpora-abbrevs)
				       "):$")
			       nil t)
	(let ((properties (text-properties-at (match-beginning 0))))
	 (replace-match (apply #'propertize new-name properties)
			nil nil nil 1))))))

(defun diogenes--corpus-make-duplicate ()
  "Make a duplicate of the corpus at point."
  (interactive)
  (let* ((corpus-region (diogenes--get-text-prop-boundaries
			 (point) 'corpus-id))
	 (corpus-id (or (get-text-property (point) 'corpus-id)
			(error "No corpus under point!")))
	 (corpus-name (get-text-property (point) 'corpus-name))
	 (corpus-str (if corpus-name (format "corpus `%s'" corpus-name)
		       "anonymous corpus"))
	 (defined-names (diogenes--get-defined-corpus-names))
	 (inhibit-read-only t))
    (when (y-or-n-p (format "Make duplicate of %s [%s]"
			    corpus-str (if (integerp corpus-id)
					   (1+ corpus-id)
					 "")))
      (let ((corpus (apply #'buffer-substring corpus-region))
	    (new-name (or (and (y-or-n-p "Rename corpus? ")
			       (cl-loop for inp =
					(read-from-minibuffer "Enter new name: ")
					unless (member inp defined-names)
					return inp))
			  (when corpus-name
			    (format "Duplicate of `%s'" corpus-name)))))
	;;(goto-char (cadr corpus-region))
	(goto-char (point-max))
	(insert "\n")
	(save-excursion
	  (insert (propertize corpus
			      'corpus-id (gensym)
			      'corpus-name new-name)))
	(save-excursion
	  (when (re-search-forward (concat "^\\[\\(.*\\)("
					   (regexp-opt diogenes--corpora-abbrevs)
					   "):$")
				   nil t)
	    (let ((properties (text-properties-at (match-beginning 0))))
	 (replace-match (apply #'propertize (format "] %s "
						    new-name)
			       properties)
			nil nil nil 1))))))))

(defun diogenes--corpus-delete-corpus ()
  "Delete the corpus at point, both from the buffer and from memory."
  (interactive)
  (let ((corpus-region (diogenes--get-text-prop-boundaries
			(point) 'corpus-id))
	(corpus-id (or (get-text-property (point) 'corpus-id)
		       (error "No corpus under point!")))
	(corpus-name (let ((name (get-text-property (point) 'corpus-name)))
		       (if name (format "corpus `%s'" name)
			 "anonymous corpus")))
	(other-corpora (diogenes--other-corpora-visited-p))
	(inhibit-read-only t))
    (cond ((integerp corpus-id)
	   (when (y-or-n-p (format "Delete %s (changes cannot be undone)? "
				   corpus-name))
	     (buffer-disable-undo)
	     (apply #'delete-region corpus-region)
	     (delete-blank-lines)
	     (diogenes--remove-user-corpus corpus-id)
	     (buffer-enable-undo)
	     (unless other-corpora (kill-buffer))))
	  (other-corpora
	   (when (y-or-n-p (format "Delete %s? " corpus-name))
	     (apply #'delete-region corpus-region)
	     (delete-blank-lines)))
	  (t
	   (when (y-or-n-p (format "Delete %s and quit? " corpus-name))
	     (kill-buffer))))))

(defun diogenes--corpora-save ()
  "Save the changes in the corpora"
  (interactive)
  (unless (eq (current-buffer) (get-buffer "*diogenes-corpora*"))
    (error "Not in Diogenes Corpora!"))
  (save-excursion
    (goto-char (point-min))
    (let ((corpora nil))
      (cl-loop
       for match = (text-property-search-forward 'corpus-id)
       while match
       for id = (prop-match-value match)
       for corpus = (diogenes--read-user-corpus (prop-match-beginning match))
       do
       (let ((name (plist-get corpus :name))
	     (read-answer-short t))
	 (if (null name)
	     (save-excursion
	       (goto-char (prop-match-beginning match))
	       (pcase (read-answer
		       "Anonymous corpus encountered: (r)ename or (d)iscard "
		       '(("rename" ?r "rename the corpus")
			 ("discard" ?d "discard the corpus")))
		 ("rename"
		  (cl-loop
		   for new-name = (read-from-minibuffer
				   (format "Rename %s to: "
					   (or name)
					   "anonymous corpus"))
		   unless (member new-name
				  (diogenes--get-defined-corpus-names))
		   return (progn (setq corpus
				       (plist-put corpus :name new-name))
				 (push (cons id corpus) corpora))
		   unless (or (y-or-n-p
			       "Name already in use! Choose another name?")
			      (not (y-or-n-q
				    "Are you sure you want to discard it?")))
		   return nil))
		 ("discard")
		 (_ (error "Flow error!"))))
	   (push (cons id corpus) corpora))))
      (let ((new-corpora (cl-loop for corpus in (nreverse corpora)
				  if (integerp (car corpus))
				  do (setf (elt diogenes--user-corpora
						(car corpus))
					   (cdr corpus))
				  else collect (cdr corpus))))
	(setf diogenes--user-corpora
	      (vconcat diogenes--user-corpora new-corpora)))
      (diogenes--save-user-corpora)
      (kill-buffer))))

(defun diogenes--corpus-submit ()
  "Execute diogenes--corpus-edit-callback.
If this variable is not bound in the current buffer, raise an
error."
  (interactive)
  (unless (eq (current-buffer) (get-buffer "*diogenes-edit-corpus*"))
    (error "Not in Diogenes Edit Corpus"))
  (unless (boundp 'diogenes--corpus-edit-callback)
    (error "I do not know what to do, since the continuation function is not properly defined!"))
  (let ((function diogenes--corpus-edit-callback)
	(corpus (diogenes--read-user-corpus (point-min))))
    (kill-buffer)
    (funcall function corpus)))

(defun diogenes-manage-user-corpora ()
  "Manage all user defined corpora."
  (interactive)
  (if-let ((corpora-buffer (get-buffer "*diogenes-corpora*")))
      (pop-to-buffer corpora-buffer) 
    (diogenes--load-user-corpora)
    (pop-to-buffer (get-buffer-create "*diogenes-corpora*"))
    (diogenes-corpus-mode)
    (save-excursion
      (let ((inhibit-read-only t))
	(insert
	 (string-join
	  (cl-loop
	   for i from 0 upto (1- (length diogenes--user-corpora))
	   for corpus across diogenes--user-corpora
	   when corpus collect
	   (diogenes--print-user-corpus corpus i))
	  "\n"))))))

;; ;; Non-transient definition
;; (defun diogenes-add-user-corpus ()
;;   "Add a new corpus after point"
;;   (interactive)
;;   (let* ((type (diogenes--select-database))
;; 	 (corpus (diogenes--new-user-corpus type))
;; 	 (inhibit-read-only t))
;;     (diogenes-manage-user-corpora)
;;     (goto-char (point-max))
;;     (unless (bobp) (newline))
;;     (save-excursion
;;       (insert (diogenes--print-user-corpus corpus (gensym))))))

(defun diogenes-add-user-corpus ()
  "Add a new corpus after point"
  (interactive)
  (let ((type (diogenes--select-database))
	(callback
	 (lambda (corpus)
	   (diogenes-manage-user-corpora)
	   (goto-char (point-max))
	   (let ((inhibit-read-only t))
	     (unless (bobp) (newline))
	     (save-excursion
	       (insert
		(diogenes--print-user-corpus corpus (gensym))))))))
    (diogenes--tr--create-user-corpus
     (list :type type :callback callback :no-ask 'no))))

(defun diogenes-edit-user-corpus (corpus &optional callback)
  "Edit a user defined CORPUS.
If callback is supplied, it should either be nil or contain a
function that will be called when the user submits the corpus."
  (pop-to-buffer (get-buffer-create "*diogenes-edit-corpus*"))
  (diogenes-corpus-edit-mode)
  (setq diogenes--corpus-edit-callback callback)
  (let ((inhibit-read-only t))
    (insert (diogenes--print-user-corpus corpus (gensym)))))

(defun diogenes--edit-user-corpus-or-execute (corpus &optional
						     callback
						     no-ask)
  "QUERY if the CORPUS should be modified. If not, either return the
corpus or, when supplied, call CALLBACK on it. If NO-ASK is not nil, it should be the symbol YES or NO."
  (unless corpus
    (error "Corpus must not be empty!"))
  (cond ((or (eq no-ask 'yes)
	     (and (not no-ask)
		  (y-or-n-p "Show and edit selected authors and works? ")))
	 (diogenes-edit-user-corpus corpus callback))
	((and no-ask
	      (not (eq no-ask 'no)))
	 (error "NO-ASK must be YES, NO, OR NIL, but is %s" no-ask))
	(callback (funcall callback corpus))
	(t corpus)))



;; ----------------------------------------------------------------
;;; MAKE NEW CORPORA
(defun diogenes--define-user-corpus (type &rest author-plist)
  "Define a subset of the databases to be used for searching."
  (when-let (authors (diogenes--get-info #'diogenes--define-corpus-script
					 (list :type type)
					 author-plist))
    (list :type type :authors authors)))

(defun diogenes--define-simple-corpus (type)
  "Define a simple corpus for searching the Diogenes database TYPE."
  (when-let ((authors (diogenes--select-author-nums (list :type type))))
    (list :type type :authors authors)))

(defun diogenes--define-regexp-corpus (type)
  "Define a corpus for searching using a perl regexp."
  (when-let ((regexp (read-from-minibuffer
		      "Select authors matching perl regex: ")))
    (diogenes--define-user-corpus type :author-regex regexp)))

;; Complex TLG corpus
(defun diogenes--define-complex-tlg-corpus (&optional args)
  "Define a complex corpus for searching the TLG database."
  (let ((categories (copy-tree (diogenes--get-tlg-categories))))
    (cl-loop
     with alist = '((?g . :genre)
		    (?x . :genre_clx)
		    (?e . :epithet)
		    (?l . :location)
		    (?s . :gender)
		    (?d . :date)
		    (?a . :author-regex)
		    (?c . :confirm)
		    (?q . :quit))
     for choice = (alist-get (read-char-from-minibuffer
			      (concat
			       "Select category [(c)onfirm, (q)uit]:\n"
			       "(g)enre, "
			       "genre_cl(x), "
			       "(e)pithet, "
			       "(l)ocation, "
			       "(s)ex, "
			       "(d)ate, "
			       "(a)uthor regex: ")
			      (mapcar #'car alist))
			     alist)
     do
     (cl-case choice
       (:quit
	(setq args nil)
	(message "")
	(cl-return))
       (:confirm
	(if args
	    (cl-return)
	  (message "No values set; to quit, press q!")
	  (sit-for 1)))
       (:author-regex
	(setq args
	      (plist-put args :author-regex
			 (read-from-minibuffer
			  "Select authors matching (perl) regex: "))))
       (:date
	(let* ((dates (plist-get categories :date))
	       (start (completing-read "Select a start date: "
				       dates nil t))
	       (end   (completing-read "Select an end date: "
				       dates nil t))
	       (varia (if (y-or-n-p "Include varia? ")
			  :varia))
	       (incerta (if (y-or-n-p "Include incerta? ")
			    :incerta)))
	  (setq args
		(plist-put args :date (list start end varia incerta)))))
       (:gender
	(setq args (plist-put args :gender
			      (if (y-or-n-p "Filter for women authors?")
				  (list "Femina ")))))
       (t
	(setq args
	      (plist-put
	       args choice
	       (diogenes--filter-in-minibuffer
		(plist-get (diogenes--get-tlg-categories) choice)
		(format "Select a %s: " (diogenes--keyword->string choice))
		:initial-selection (plist-get args choice)
		:remove-prompt "Remove from selection: "
		:remove-string "REMOVE"
		:all-string "ALL"
		:regexp-string "REGEXP"
		:commit-string "FINISHED"))))))
    (cond
     ((null args))
     ((let ((max-mini-window-height 0.8))
	(y-or-n-p (concat (cl-loop for (k v) on args by #'cddr
				   concat (format "%s: %s\n"
						  (diogenes--keyword->string k)
						  v))
			  "\n"
			  "Please confirm this selection: ")))
      (apply #'diogenes--define-user-corpus "tlg"
	     (let ((args-len (/ (length args) 2)))
	       (if (> args-len 1)
		   (plist-put args :criteria
			      (completing-read
			       "How many of the categories should match? "
			       (append (list "all" "any")
				       (when (> args-len 2)
					 (cl-loop for i from 2 to (1- args-len)
						  collect (number-to-string i))))
			       nil t nil nil "all"))
		 args))))
     (t (diogenes--define-complex-tlg-corpus args)))))

(defun diogenes--new-user-corpus (type)
  "Construct a new corpus of TYPE."
  (let* ((choices
	  (list (cons "manual" #'diogenes--define-simple-corpus)
		(cons "regexp" #'diogenes--define-regexp-corpus)
		(when (diogenes--user-corpora-exist-p type)
		  (cons "saved"  #'diogenes--select-user-corpus))
		(when (string= type "tlg")
		  (cons "complex" (lambda (junk)
				    (diogenes--define-complex-tlg-corpus))))))
	 (answers
	  (list '("manual" ?m "select authors manually")
		'("regexp" ?r "select authors matching (perl) regexp")
		(when (diogenes--user-corpora-exist-p type)
		  '("saved"  ?s "use a saved corpus"))
		(when (string= type "tlg")
		  '("complex" ?c "define a complex corpus"))))
	 (read-answer-short t))
    (funcall (cdr (assoc (read-answer "How would you like to define the corpus "
				      (cl-remove-if #'null answers))
			 (cl-remove-if #'null choices)))
	     type)))


;;; Transient-interface (experimental)
(defmacro diogenes--define-tlg-category-argument (category)
  `(transient-define-argument ,(intern (format "diogenes--tlg-%s" category)) ()
     :class 'transient-option
     :description ,(format "Select a %s" category)
     :argument ,(format "%-14s" (concat category ": "))
     :format " %k %d %v"
     :prompt ,(format "Select a %s" category)
     :always-read t
     :reader
     (let ((previous-selection nil))
       (lambda (prompt initial-input history)
	 (prin1-to-string
	  (setq previous-selection
		(diogenes--filter-in-minibuffer
		 (plist-get (diogenes--get-tlg-categories)
			    ,(diogenes--string->keyword category))
		 prompt
		 :initial-selection previous-selection
		 :remove-prompt "Remove from selection: "
		 :remove-string "REMOVE"
		 :all-string "ALL"
		 :regexp-string "REGEXP"
		 :commit-string "FINISHED")))))))

(diogenes--define-tlg-category-argument "genre")
(diogenes--define-tlg-category-argument "genre_clx")
(diogenes--define-tlg-category-argument "epithet")
(diogenes--define-tlg-category-argument "location")

(transient-define-argument diogenes--tlg-date ()
  :class 'transient-option
  :argument "date:         "
  :format " %k %d %v"
  :prompt "Select a genre: "
  :always-read t
  :reader
  (lambda (prompt initial-input history)
    (prin1-to-string
     (let* ((dates (plist-get (diogenes--get-tlg-categories) :date))
	    (start (completing-read "Select a start date: "
				    dates nil t initial-input history))
	    (end   (completing-read "Select an end date: "
				    dates nil t initial-input history))
	    (varia (if (y-or-n-p "Include varia? ")
		       :varia))
	    (incerta (if (y-or-n-p "Include incerta? ")
			 :incerta)))
       (list start end varia incerta)))))

(transient-define-argument diogenes--tlg-gender ()
  :class 'transient-option
  :argument "gender:       "
  :format " %k %d %v"
  :reader (lambda (&rest _) (prin1-to-string '("Femina "))))

(transient-define-argument diogenes--tlg-author-regex ()
  :class 'transient-option
  :argument "author-regex: "
  :prompt "Select authors matching (perl) regex: "
  :format " %k %d %v"
  :reader (lambda (prompt initial-input history)
	    (prin1-to-string (read-string prompt initial-input history))))

(transient-define-argument diogenes--tlg-criteria ()
  :class 'transient-option
  :argument "criteria:     "
  :format " %k %d %v"
  :prompt "How many of the categories should match? "
  :choices '("any" "2" "3" "4" "5" "6" "all")
  :always-read t
  :allow-empty nil
  :init-value (lambda (o) (oset o value "any")))

(transient-define-prefix diogenes--tr--complex-tlg-corpus (scope)
  "Define a complex tlg corpus using transient."
  [:pad-keys t
   ("a" "Author name        " diogenes--tlg-author-regex)
   ("d" "Author's date      " diogenes--tlg-date)
   ("l" "Author's location  " diogenes--tlg-location)
   ("w" "Women authors only " diogenes--tlg-gender)
   ("e" "Author's epithet   " diogenes--tlg-epithet)
   ("g" "Text Genre         " diogenes--tlg-genre)
   ("x" "Text Genre (ext.)  " diogenes--tlg-genre_clx)
   ""
   ("c" "Criteria to match  " diogenes--tlg-criteria)
   ("<RET>" "CONFIRM"
    (lambda () (interactive)
      (let* ((args
	      (cl-loop
	       for entry in (transient-args 'diogenes--tr--complex-tlg-corpus)
	       for (key val) = (diogenes--split-once ":[[:space:]]+" entry)
	       nconc (list (diogenes--string->keyword key)
			   (if (string= key "criteria") val
			     (read val)))))
	     (corpus (apply #'diogenes--define-user-corpus "tlg" args)))
	(if corpus 
	    (diogenes--edit-user-corpus-or-execute corpus
						   (diogenes--tr--callback)
						   (diogenes--tr--no-ask))
	  (message "No results for these criteria!")
	  ;; (transient-set)
	  (transient-setup 'diogenes--tr--complex-tlg-corpus
			   nil nil :scope (transient-scope))))))]
  (interactive (list (or (ignore-errors (transient-scope))
			 (list :type "tlg"))))
  (transient-setup 'diogenes--tr--complex-tlg-corpus nil nil :scope scope))

(defun diogenes--tr--define-user-corpus (method)
  (let ((corpus (funcall (cl-case method
			   (saved  #'diogenes--select-user-corpus)
			   (regexp #'diogenes--define-regexp-corpus)
			   (t      #'diogenes--define-simple-corpus))
			 (diogenes--tr--type))))
    (if corpus
	(diogenes--edit-user-corpus-or-execute corpus
					       (diogenes--tr--callback)
					       (diogenes--tr--no-ask))
      (message "No results for these criteria!")
      (transient-setup 'diogenes--tr--create-user-corpus nil nil
		       :scope (transient-scope)))))

(transient-define-prefix diogenes--tr--create-user-corpus (scope)
  "Construct a new corpus of TYPE."
  ["How would you like to define the corpus?"
   ("m" "Select authors manually"
    (lambda () (interactive) (diogenes--tr--define-user-corpus 'simple)))
   ("r" "Select authors matching (perl) regexp"
    (lambda () (interactive) (diogenes--tr--define-user-corpus 'regexp)))
   ("s" "Use saved user corpus"
    (lambda () (interactive) (diogenes--tr--define-user-corpus 'saved))
    :if (lambda () (diogenes--user-corpora-exist-p (diogenes--tr--type))))
   ("c" "Define a complex TLG corpus" diogenes--tr--complex-tlg-corpus
    :if (lambda () (string= (diogenes--tr--type) "tlg")))]
  (interactive (list (if transient--prefix (transient-scope)
		       (list :type (diogenes--select-database)))))
  (transient-setup 'diogenes--tr--create-user-corpus nil nil :scope scope))


(provide 'diogenes-corpora)

;;; diogenes-corpora.el ends here

