;;; diogenes.el --- Interface to diogenes -*- lexical-binding: t -*-

;; An interface to Peter Heslin's Diogenes
;; Copyright (C) 2024 Michael Neidhart
;;
;; Author: Michael Neidhart <mayhoth@gmail.com>
;; Keywords: classics, tools, philology, humanities
;;
;; Version: 0.3
;; Package-Requires: (cl-lib transient)

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Agenda:
;;   - select_authors:
;;     - author_nums => { auth => [wk1, wk2] }
;;     - criteria => 'all' || 'any'
;;     - get_tlg_categories => 1 (hash keys => [val1, val2,..])
;;     - only TLG:
;;       - date(start, end, varia, incerta). Mögliche Werte : date ()
;;       - genre, genre_clx, epithet
;;       - gender ('Femina ')
;;       - location
;;     - interface with transient
;; - Indexed (...)
;; - Diogenes minor mode: TLG lookup at-point

;;; Code:

(require 'cl-lib)
(require 'seq)
;; (require 'transient)

;;;; --------------------------------------------------------------------
;;;; CUSTOM VARIABLES (and their getter functions)
;;;; --------------------------------------------------------------------

(defgroup diogenes nil
  "Interface to P. Heslin's Diogenes."
  :group 'tools)

(defcustom diogenes-perl-executable "perl"
  "Path to perl executable."
  :type 'string
  :group 'diogenes)

(defcustom diogenes-path nil
  "Path to the Diogenes installation."
  :type 'directory
  :group 'diogenes)

(defconst diogenes-perl-min-version 5.10
  "Minimal required verson of perl.")

(defcustom diogenes-browser-show-citations t
  "Whether to show embedded citations in the browser by default."
  :type 'boolean
  :group 'diogenes
  :local t)

(defcustom diogenes-preferred-lsj-file "grc.lsj.logeion.xml"
  "Filename of the preferred version of the LSJ dictionary."
  :type 'string
  :group 'diogenes)

(defun diogenes--path ()
  (if diogenes-path
      (expand-file-name diogenes-path)
    (error "diogenes-path is not set! 
Please set it to the root directory of your Diogenes installation!")))

(defun diogenes--include-server ()
  (concat "-I" (file-name-concat (diogenes--path)
				 "server")))

(defun diogenes--include-cpan ()
  (concat "-I" (file-name-concat (diogenes--path)
				 "dependencies"
				 "CPAN")))

(defun diogenes--perseus-path ()
  (directory-file-name (file-name-concat (diogenes--path)
					 "dependencies"
					 "data")))

(defun diogenes--dict-file (lang)
  (pcase lang
    ("greek" (file-name-concat (diogenes--perseus-path)
			       diogenes-preferred-lsj-file))
    ("latin" (file-name-concat (diogenes--perseus-path)
			       "lat.ls.perseus-eng1.xml"))
    (_ (error "Undefined language %s" lang))))


;;; Validate that all data is present
(unless (file-exists-p (file-name-concat (diogenes--path)
					 "server"
					 "Diogenes"
					 "Base.pm"))
  (error "Could not find a working Diogenes installation in %s"
	 (diogenes--path)))

(mapc (lambda (lang)
	(unless (file-exists-p (diogenes--dict-file lang))
	  (error "Could not find %s lexicon at %s."
		 lang (diogenes--dict-file lang))))
      '("greek" "latin"))

(mapc (lambda (file)
	(unless (file-exists-p (file-name-concat (diogenes--perseus-path)
						 file))
	  (error "Could not find %s in %s. Did you build them?"
		 file (diogenes--perseus-path))))
      '("greek-analyses.txt" "greek-lemmata.txt"
	"latin-analyses.txt" "latin-lemmata.txt"))

;;;; --------------------------------------------------------------------
;;;; TOPLEVEL FUNCTIONS
;;;; --------------------------------------------------------------------

;;; SEARCH
;;;###autoload
(defun diogenes-search-tlg (options-or-pattern
			    &optional author-plist prefix)
  "Search for a phrase in the Greek TLG database.
Uses the Diogenes Perl Module."
  (interactive "i\ni\np")
  (diogenes--search-database "tlg" options-or-pattern author-plist prefix))

;;;###autoload
(defun diogenes-search-phi (options-or-pattern
			    &optional author-plist prefix)
  "Search for a phrase in the Latin PHI database.
Uses the Diogenes Perl Module."
  (interactive "i\ni\np")
  (diogenes--search-database "phi" options-or-pattern author-plist prefix))

;;;###autoload
(defun diogenes-search-ddp (options-or-pattern
			    &optional author-plist prefix)
  "Search for a phrase in the Duke Documentary Database.
Uses the Diogenes Perl module."
  (interactive "i\ni\np")
  (diogenes--search-database "ddp" options-or-pattern author-plist prefix))

;;;###autoload
(defun diogenes-search-ins (options-or-pattern
			    &optional author-plist prefix)
  "Search for a phrase in the Classical Inscriptions Database.
Uses the Diogenes Perl module."
  (interactive "i\ni\np")
  (diogenes--search-database "ins" options-or-pattern author-plist prefix))

;;;###autoload
(defun diogenes-search-chr (options-or-pattern
			    &optional author-plist prefix)
  "Search for a phrase in the Christian Inscriptions Database.
Uses the Diogenes Perl module."
  (interactive "i\ni\np")
  (diogenes--search-database "chr" options-or-pattern author-plist prefix))

;;;###autoload
(defun diogenes-search-misc (options-or-pattern
			    &optional author-plist prefix)
  "Search for a phrase in the Miscellaneous PHI Texts  Database.
Uses the Diogenes Perl module."
  (interactive "i\ni\np")
  (diogenes--search-database "misc" options-or-pattern author-plist prefix))

;;;###autoload
(defun diogenes-search-cop (options-or-pattern
			    &optional author-plist prefix)
  "Search for a phrase in the  PHI Coptic Texts Database.
Uses the Diogenes Perl module."
  (interactive "i\ni\np")
  (diogenes--search-database "cop" options-or-pattern author-plist prefix))



;;; DUMP
;;;###autoload
(defun diogenes-dump-tlg (&optional author work)
  "Dump a work from the Greek TLG database in its entirety.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--dump-from-database "tlg" author work))

;;;###autoload
(defun diogenes-dump-phi (&optional author work)
  "Dump a work from the Latin PHI database in its entirety.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--dump-from-database "phi" author work))

;;;###autoload
(defun diogenes-dump-ddp (&optional author work)
  "Dump a work from the Duke Documentary Database in its entirety.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--dump-from-database "ddp" author work))

;;;###autoload
(defun diogenes-dump-ins (&optional author work)
  "Dump a work from the Classical Inscriptions Database in its entirety.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--dump-from-database "ins" author work))

;;;###autoload
(defun diogenes-dump-chr (&optional author work)
  "Dump a work from the Christian Inscriptions Database in its entirety.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--dump-from-database "chr" author work))

;;;###autoload
(defun diogenes-dump-misc (&optional author work)
  "Dump a work from the Miscellaneous PHI Texts Database in its entirety.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--dump-from-database "misc" author work))

;;;###autoload
(defun diogenes-dump-cop (&optional author work)
  "Dump a work from the PHI Coptic Texts Database in its entirety.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--dump-from-database "cop" author work))



;;; BROWSE
;;;###autoload
(defun diogenes-browse-tlg (&optional author work)
  "Browse a specific passage in a work from the Greek TLG database.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--browse-database "tlg" author work))

;;;###autoload
(defun diogenes-browse-phi (&optional author work)
  "Browse a work from the Latin PHI database.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--browse-database "phi" author work))

;;;###autoload
(defun diogenes-browse-ddp (&optional author work)
  "Browse a work from the Duke Documentary Database.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--browse-database "ddp" author work))

;;;###autoload
(defun diogenes-browse-ins (&optional author work)
  "Browse a work from the Classical Inscriptions Database.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--browse-database "ins" author work))

;;;###autoload
(defun diogenes-browse-chr (&optional author work)
  "Browse a work from the Christian Inscriptions Database.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--browse-database "chr" author work))

;;;###autoload
(defun diogenes-browse-misc (&optional author work)
  "Browse a work from the Miscellaneous PHI Texts  Database.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--browse-database "misc" author work))

;;;###autoload
(defun diogenes-browse-cop (&optional author work)
  "Browse a work from the  PHI Coptic Texts Database.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--browse-database "cop" author work))



;;; DICTIONARY LOOKUP
;;;###autoload
(defun diogenes-lookup-greek (word)
  "Search for a greek word in the LSJ Greek Dictionary.
Accepts both Unicode and Beta Code as input."
  (interactive (list (read-from-minibuffer "Search LSJ for: "
					   (thing-at-point 'word t))))
  (let ((normalized (diogenes--beta-normalize-gravis
		     (diogenes--greek-ensure-beta word))))
    (diogenes--search-dict normalized "greek"
			   #'diogenes--beta-sort-function
			   #'diogenes--xml-key-fn)))

(defun diogenes-lookup-latin (word)
  "Search for a greek word in the Lewis & Short Latin Dictionary."
  (interactive (list (read-from-minibuffer "Search Lewis & Short for: "
					   (thing-at-point 'word t))))
  (diogenes--search-dict word "latin"
			 #'diogenes--ascii-sort-function
			 #'diogenes--xml-key-fn))


;;; MORPHEUS PARSING
;;;###autoload
(defun diogenes-parse-greek-exact (word)
  "Parse a greek word and display the results."
  (interactive "sParse greek word: ")
  (pop-to-buffer (get-buffer-create "*diogenes morphology raw output*"))
  (emacs-lisp-mode)
  (insert (prin1-to-string (diogenes--parse-word word "greek"))))

;;;###autoload
(defun diogenes-parse-greek (query)
  "Parse a greek word and display the results.
QUERY is interpreted as a regular expression which must match the forms."
  (interactive "sParse greek word: ")
  (pop-to-buffer (get-buffer-create "*diogenes morphology raw output*"))
  (emacs-lisp-mode)
  (insert (prin1-to-string (diogenes--parse-all query "greek" #'string-match))))

;;;###autoload
(defun diogenes-show-lemma-and-forms-greek-exact (word)
  "Show all attested forms in a lemma."
  (interactive "sShow all forms of: ")
  (pop-to-buffer (get-buffer-create "*diogenes morphology raw output*"))
  (emacs-lisp-mode)
  (insert (prin1-to-string (diogenes--show-all-forms word "greek"))))

;; ;;;###autoload
;; (defun diogenes-parse-latin-exact (word)
;;   "Parse a greek word and display the results."
;;   (interactive "sParse latin word: ")
;;     (emacs-lisp-mode)
;;   (insert (prin1-to-string (diogenes--parse-word word "latin"))))


;;; UTILITIES
;;;###autoload
(defun diogenes-utf8-to-beta (str)
  "Convert greek beta code to utf-8. 
If a region is active, convert the contents of the region in place; 
otherwise, prompt the user for input."
  (interactive "i")
  (cond (str (with-temp-buffer (insert str)
			       (translate-region (point-min) (point-max)
						 diogenes--utf8-to-beta-table)
			       (buffer-string)))
	((use-region-p) (translate-region (point) (mark)
					  diogenes--utf8-to-beta-table))
	(t (let ((str (read-from-minibuffer "Convert to Greek Beta Code: "
					    nil nil nil nil
					    (thing-at-point 'word t))))
	     (message "%s" (diogenes-utf8-to-beta str))))))

;;;###autoload
(defun diogenes-beta-to-utf8 (str)
  "Convert greek unicode to beta code.
If a region is active, convert the contents of the region in place; 
otherwise, prompt the user for input."
  (interactive "i")
  (cond (str (with-temp-buffer (insert str)
			       (translate-region (point-min) (point-max)
						 diogenes--beta-to-utf8-table)
			       (buffer-string)))
	((use-region-p) (translate-region (point) (mark)
					  diogenes--beta-to-utf8-table))
	(t (let ((str (read-from-minibuffer "Convert from Greek Beta Code: "
					    nil nil nil nil
					    (thing-at-point 'word t))))
	     (message "%s" (diogenes-beta-to-utf8-table str))))))

;;;###autoload
(defun diogenes-ol-to-ad (ol)
  "Converts Ol. to A.D."
  (interactive "nPlease enter the Olypiad: ")
  (let ((ad (diogenes--ol-to-ad ol)))
    (message "%s – %s"
	     (diogenes--bc-and-ad ad)
	     (diogenes--bc-and-ad (let ((last-year (+ ad 3)))
				    (if (zerop last-year) 1
				      last-year))))))

;;;###autoload
(defun diogenes-ad-to-ol (ad)
  "Converts A.D. to Ol."
  (interactive "nPlease enter a year: ")
  (when (= ad 0)
    (error "There is no year 0!"))
  (let ((year (+ ad (if (< ad 0) 780 779))))
    (message "Ol. %d/%d" (/ year 4)
	     (1+ (mod year 4)))))





;;; Implementation:

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
		     (read-from-minibuffer "Enter search term: ")))
	 (unless (y-or-n-p (format "Search the whole %s corpus? "
				   type))
	   (setf author-plist
		 (list :author-nums
		       (diogenes--select-author-nums (list :type type))))))
	(prefix
	 (let ((patterns (cl-do ((patterns nil)
				 (pat (cl-loop for inp =
					       (read-from-minibuffer "With pattern: ")
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

;;;; --------------------------------------------------------------------
;;;; DUMPER
;;;; --------------------------------------------------------------------

(defun diogenes--dump-from-database (type &optional author work)
  "Dump a work from a Diogenes database in its entirety.
Uses the Diogenes Perl module."
  (let* ((author (or author
		     (diogenes--select-author-num `(:type ,type))))
	 (work (or work
		   (diogenes--select-work-num `(:type ,type)
					      author))))
    (diogenes--dump-work `(:type ,type) (list author work))))

;;;; --------------------------------------------------------------------
;;;; BROWSER
;;;; --------------------------------------------------------------------

(defun diogenes--browse-database (type &optional author work)
  "Select a specific passage in a work from a diogenes database for browsing.
Uses the Diogenes Perl module."
  (let* ((author (or author
		     (diogenes--select-author-num (list :type type))))
	 (work (or work
		   (diogenes--select-work-num (list :type type)
					      author)))
	 (passage (when (y-or-n-p "Specify passage? ")
		    (diogenes--select-passage (list :type type)
					      author
					      work))))
    (diogenes--browse-work (list :type type) (nconc (list author work)
						    passage))))

;;; BROWSER MODE
(defvar diogenes-browser-mode-map
  (let ((map (nconc (make-sparse-keymap) text-mode-map)))
    (keymap-set map "<down>" #'diogenes-browser-forward-line)
    (keymap-set map "C-n" #'diogenes-browser-forward-line)
    (keymap-set map "<up>" #'diogenes-browser-backward-line)
    (keymap-set map "C-p" #'diogenes-browser-backward-line)
    (keymap-set map "C-c C-c" #'diogenes-browser-quit)
    (keymap-set map "C-c C--" #'diogenes-browser-remove-hyphenation)
    (keymap-set map "C-c C-+" #'diogenes-browser-reinsert-hyphenation)
    (keymap-set map "C-c C-n" #'diogenes-browser-toggle-citations)
    map)
  "Basic mode map for the Diogenes Browser.")

(define-derived-mode diogenes-browser-mode text-mode "Diogenes Browser"
  "Major mode to browse Diogenes' databases."
  (make-local-variable 'diogenes--browser-backwards))

(defun diogenes--browser-format-citation (citation)
  (propertize (format "%-14s"
		      (mapconcat (lambda (x) (format "%s" x))
				 citation
				 "."))
	      'diogenes-citation t
	      'face 'font-lock-comment-face
	      'font-lock-face 'font-lock-comment-face
	      'rear-nonsticky t))

(defun diogenes--browser-format-header (header-lines)
  (propertize (concat (string-join header-lines
				   "\n")
		      "\n\n")
	      'diogenes-header t
	      'face 'info-title-1
	      'font-lock-face 'info-title-1
	      ;; 'read-only t
	      'front-sticky t
	      'rear-nonsticky t))

(defvar-local diogenes--browser-output-buffer ""
  "Buffers the output of the diogenes browser output, if it is an
incomplete lisp expression.")
(defun diogenes--read-browser-output (str)
  "Try to read a lisp expression from browser output.
If it is incomplete, buffer it and prepend it when called again."
  (let ((form (ignore-errors (read (concat diogenes--browser-output-buffer
					   str)))))
    (cond ((and form (listp form))
	   (setq diogenes--browser-output-buffer "") form)
	  (t (setq diogenes--browser-output-buffer
		   (concat diogenes--browser-output-buffer str))
	     nil))))

(defun diogenes--browser-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (when-let ((data (diogenes--read-browser-output string)))
     (with-current-buffer (process-buffer proc)
       (seq-let (cit header &rest lines) data
	 (unless lines (error "No input received!"))
	 (cond ((and (boundp 'diogenes--browser-backwards)
		     diogenes--browser-backwards)
		(cond (header (goto-char (point-min))
			      (newline)
			      (goto-char (point-min)))
		      (t (or (text-property-search-forward 'diogenes-header)
			     (goto-char (point-min))))))
	       (t (goto-char (point-max))))
	 (when header (insert (diogenes--browser-format-header header)))
	 (dolist (alist lines)
	   (when diogenes-browser-show-citations
	     (insert (diogenes--browser-format-citation (car alist))))
	   (insert (propertize (format "%s\n" (cdr alist))
			       'cit (car alist))))
	 (set-marker (process-mark proc) (point-max))
	 (recenter -1 t))))))

;;; Browser Commands
(defun diogenes--send-cmd-to-browser (cmd)
  (let ((diogenes-process (or (get-buffer-process (current-buffer))
			      (error (format "No process in buffer %s!"
					     (current-buffer))))))
    (process-send-string diogenes-process (concat cmd "\n"))))

(defun diogenes--browser-set-height (height)
  (interactive "NLines to display: ")
  (diogenes--send-cmd-to-browser (number-to-string height)))

(defun diogenes-browser-forward ()
  (interactive)
  (setq diogenes--browser-backwards nil)
  (diogenes--send-cmd-to-browser
   (concat (number-to-string (- (floor (window-screen-lines))
				next-screen-context-lines))
	   "n")))

(defun diogenes-browser-backward ()
  (interactive)
  (setq diogenes--browser-backwards t)
  (diogenes--send-cmd-to-browser
   (concat (number-to-string (- (floor (window-screen-lines))
				next-screen-context-lines))
	   "p")))

(defun diogenes-browser-quit ()
  (interactive)
  (diogenes--send-cmd-to-browser "q"))

(defun diogenes-browser-forward-line (&optional N)
  (interactive "p")
  (forward-line N)
  (when (eobp) (diogenes-browser-forward)))

(defun diogenes-browser-backward-line (&optional N)
  (interactive "p")
  (forward-line (- N))
  (when (bobp) (diogenes-browser-backward)))

(defun diogenes-browser-toggle-citations ()
  "Toggle display of the embedded citations in the Diogenes Browser."
  (interactive)
  (save-excursion
    (cond (diogenes-browser-show-citations
	   (setq diogenes-browser-show-citations nil)
	   (goto-char (point-min))
	   (let (cit-match)
	     (while (setq cit-match
			  (text-property-search-forward 'diogenes-citation))
	       (delete-region (prop-match-beginning cit-match)
			      (prop-match-end cit-match)))))
	  (t
	   (setq diogenes-browser-show-citations t)
	   (goto-char (point-min))
	   (let (prop-change)
	     (while (and (setq prop-change
			       (next-single-property-change (point) 'cit))
			 (goto-char prop-change)
			 (not (eobp)))
	       (when-let ((citation (get-text-property (point) 'cit)))
		 (insert (diogenes--browser-format-citation citation)))))))))

(defun diogenes-browser-remove-hyphenation (&optional mark-with-vertical-bar)
  "Join all hyphenated words in the current Diogenes Browser Buffer."
  (interactive "P")
  (unless (eq major-mode 'diogenes-browser-mode)
    (error "Not in a Diogenes Browser buffer!"))
  (with-undo-amalgamate
   (save-excursion
     (goto-char (point-min))
     (let (part-a)
       (while (setq pos-a (and (re-search-forward "\\([^ <]+\\)-\\s-*$"
						  (point-max) t)
			       (cons (match-beginning 1)
				     (match-end 1))))
	 (when-let* ((line-a (text-property-search-backward 'cit))
		     (line-b (text-property-search-forward 'cit nil nil t))
		     (pos-b  (and (goto-char (prop-match-beginning line-b))
				  (re-search-forward "\\S-+"
						     (point-max) t)
				  (cons (match-beginning 0)
					(match-end 0))))
		     (word-a (buffer-substring-no-properties (car pos-a)
							     (cdr pos-a)))
		     (word-b (buffer-substring-no-properties (car pos-b)
							     (cdr pos-b))))
	   (put-text-property (prop-match-beginning line-a)
			      (prop-match-end line-a)
			      'hyphen-start word-a)
	   (put-text-property (prop-match-beginning line-b)
			      (prop-match-end line-b)
			      'hyphen-end word-b)	
	   (delete-region (car pos-b) (1+ (cdr pos-b)))
	   (goto-char (cdr pos-a))
	   (delete-char 1)
	   (when mark-with-vertical-bar (insert-and-inherit "|"))
	   (insert-and-inherit word-b)))))))

(defun diogenes-browser-reinsert-hyphenation ()
  (interactive)
  (unless (eq major-mode 'diogenes-browser-mode)
    (error "Not in a Diogenes Browser buffer!"))
  (with-undo-amalgamate
    (save-excursion
      (goto-char (point-min))
      (let (line-a line-b)
	(while (and (setq line-a (text-property-search-forward 'hyphen-start))
		    (setq line-b (text-property-search-forward 'hyphen-end)))
	  (let ((word-a (prop-match-value line-a))
		(word-b (prop-match-value line-b))
		(bol-a (prop-match-beginning line-a))
		(bol-b (prop-match-beginning line-b))
		(eol-a (prop-match-end line-a))
		(eol-b (prop-match-end line-b)))
	    (remove-text-properties bol-a eol-a '(hyphen-start nil))
	    (remove-text-properties bol-b eol-b '(hyphen-end nil))
	    (let ((prop-a (text-properties-at bol-a))
		  (prop-b (text-properties-at bol-b)))
	      (goto-char bol-b)
	      (insert (apply #'propertize (concat word-b " ")
			     prop-b))
	      (goto-char eol-a)
	      (re-search-backward (regexp-quote word-b))
	      (delete-char (length word-b))
	      (when (string= (buffer-substring (1- (point)) (point))
			     "|")
		(delete-char -1))
	      (insert (apply #'propertize "-"
			     prop-a)))))))))

;;;; --------------------------------------------------------------------
;;;; PERSEUS DICTIONARY LOOKUP
;;;; --------------------------------------------------------------------

(defun diogenes--search-dict (word lang sort-fn key-fn)
  "Search for a word in a diogenes Dictionary."
  (seq-let (xml-bytes start end exact-hit)
      (diogenes--binary-search (diogenes--dict-file lang)
			       sort-fn key-fn word)
    (unless exact-hit (message "No results for %s! Showing nearest entry" word))
    (let* ((xml (decode-coding-string xml-bytes 'utf-8))
	   (formatted (diogenes--dict-parse-xml xml start end))
	   (lookup-buffer (diogenes--get-fresh-buffer "lookup")))
      (pop-to-buffer lookup-buffer)
      (diogenes-lookup-mode)
      (setq diogenes--lookup-file (diogenes--dict-file lang)
	    diogenes--lookup-bufstart start
	    diogenes--lookup-bufend end
	    diogenes--lookup-lang lang)
      (cond (formatted (diogenes--lookup-insert-and-format formatted))
	    (t (diogenes--lookup-insert-xml xml start end lookup-buffer))))))

(defun diogenes-lookup-next ()
  "Find and show the next entry in the active dictionary."
  (interactive)
  (unless (eq major-mode 'diogenes-lookup-mode)
    (error "Not in Diogenes Lookup Mode!"))
  (seq-let (xml-bytes start end)
      (diogenes--get-dict-line diogenes--lookup-file
			       (1+ diogenes--lookup-bufend))
    (unless xml-bytes (error "No further entries!"))
    (let* ((xml (decode-coding-string xml-bytes 'utf-8))
	   (formatted (diogenes--dict-parse-xml xml start end)))
      (setq diogenes--lookup-bufend end)
      (goto-char (point-max))
      (diogenes--lookup-print-separator)
      (if formatted
	  (diogenes--lookup-insert-and-format formatted)
	(diogenes--lookup-insert-xml xml start end (current-buffer))))))

(defun diogenes-lookup-previous ()
  "Find and show the previous entry in the active dictionary."
  (interactive)
  (unless (eq major-mode 'diogenes-lookup-mode)
    (error "Not in Diogenes Lookup Mode!"))
  (seq-let (xml-bytes start end)
      (diogenes--get-dict-line diogenes--lookup-file
			       (1- diogenes--lookup-bufstart))
    (unless xml-bytes (error "No further entries!"))
    (let* ((xml (decode-coding-string xml-bytes 'utf-8))
	   (formatted (diogenes--dict-parse-xml xml start end)))
      (setq diogenes--lookup-bufstart start)
      (goto-char (point-min))
      (diogenes--lookup-print-separator)
      (goto-char (point-min))
      (if formatted
	  (diogenes--lookup-insert-and-format formatted)
	(diogenes--lookup-insert-xml xml start end (current-buffer)))
      
      (goto-char (point-min)))))



;;; LOOKUP MODE
(defvar diogenes-lookup-mode-map
  (let ((map (nconc (make-sparse-keymap) text-mode-map)))
    (keymap-set map "<down>" #'diogenes-lookup-forward-line)
    (keymap-set map "C-n" #'diogenes-lookup-forward-line)
    (keymap-set map "<up>" #'diogenes-lookup-backward-line)
    (keymap-set map "C-p" #'diogenes-lookup-backward-line)
    map)
  "Basic mode map for the Diogenes Lookup Mode.")

(define-derived-mode diogenes-lookup-mode text-mode "Diogenes Lookup"
  "Major mode to browse databases."
  (make-local-variable 'diogenes--lookup-file)
  (make-local-variable 'diogenes--lookup-bufstart)
  (make-local-variable 'diogenes--lookup-bufend)
  (make-local-variable 'diogenes--lookup-lang))



;;; LOOKUP MODE COMMANDS
(defun diogenes-lookup-forward-line (&optional N)
  (interactive "p")
  (forward-line N)
  (when (eobp) (diogenes-lookup-next)))

(defun diogenes-lookup-backward-line (&optional N)
  (interactive "p")
  (forward-line (- N))
  (when (bobp) (diogenes-lookup-previous)))

(defun diogenes--lookup-button-invoke (char)
  "Callback for the links in Diogenes Lookup Mode."
  (interactive "d")
  (let ((action (get-text-property char 'action)))
    (cl-case action
      (bibl (apply #'diogenes--browse-work (diogenes--lookup-parse-bibl-string
					    (get-text-property char 'bibl)))))))

(defun diogenes--lookup-parse-bibl-string (str)
  "Parse a DICT bibliography reference string.
Returns a list that diogenes--browse-work can be applied to."
  (seq-let (corpus author work-and-passage)
      (split-string (replace-regexp-in-string "^Perseus:abo:" "" str)
		    ",")
    (seq-let (work &rest passage)
	(split-string work-and-passage ":")
      (let ((labels-missing
	     (- (length (diogenes--get-work-labels (list :type corpus)
						   (list author work)))
		(length passage))))
	(cond ((< labels-missing 0) (error "Too many labels! %s" str))
	      ((> labels-missing 0)
	       (setq passage (nconc passage (cl-loop for i from 1 to labels-missing
						     collect "")))))
	(list (list :type corpus)
	      (append (list author work) passage))))))



;;; Format and insert contents
(defun diogenes--lookup-insert-and-format (str)
  (let ((start (point)))
    (insert str)
    (fill-region start (point))
    (recenter -1)
    (goto-char start)))

(defun diogenes--lookup-print-separator ()
  "Print a separator line between entries"
  (insert "\n\n")
  (cl-loop repeat fill-column do (insert "—"))
  (insert "\n\n"))



;;; Parse XML
(defun diogenes--dict-parse-xml (str begin end)
  "Try to parse a string containing the XML of a dictionary entry."
  (let ((parsed (with-temp-buffer (insert (diogenes--try-correct-xml str))
				  (libxml-parse-xml-region))))
    (when parsed (diogenes--dict-process-elt parsed (list 'begin begin 'end end)))))

(defun diogenes--dict-process-elt (elt properties)
  "Process a parsed XML element of a dictionary entry recursively.
The properties list is an accumulator that holds all properties
of the active element."
  (cl-typecase elt
    (string (apply #'propertize elt properties))
    (list (let ((p (append (diogenes--dict-handle-elt elt)
			   properties)))
	    (mapconcat (lambda (e) (diogenes--dict-process-elt e p))
		       (cddr elt))))))

(defun diogenes--try-correct-xml (xml)
  "Try to hotfix invalid xml in the greek LSJ files."
  (diogenes--replace-regexes-in-string xml
				       ("<\\([[:multibyte:][:space:]]+\\)>"
					"&lt;\\1&gt;")))

(defvar diogenes--dict-xml-handlers-extra
  '(
    ;;(author . '(font-lock-face bold))
    ;;(title . '(font-lock-face italic))
    (i . (font-lock-face diary))
    (b . (font-lock-face bold)))
  "An alist of property lists to be applied to a simple tag in a dictionary.")

(defun diogenes--dict-handle-elt (elt)
  "Handle the more complicated tags of a Diogenes dictionary file.
Each element is a list whose car is the element, whose cadr is an
a-list containing all the properties, and whose cddr is the
actual contents of the list. This function selects an approriate
handler based on the car and returns a property list that
represents the properties of the element. It may also manipulate
the contents of the element (cddr). Elements that only require
special formatting are handled by th
diogenes--dict-xml-handlers-extra variable."
  (let ((tag (car elt)))
    (cl-case tag
      (head (when-let ((orth-orig (cdr (assoc 'orth_orig (cadr elt)))))
	      (setf (cddr elt) (list orth-orig)))
	    '(font-lock-face shr-h1))
      (sense (push (concat "\n\n"
			   (propertize (cdr (assoc 'n (cadr elt)))
				       'font-lock-face 'success)
			   " ")
		   (cddr elt))
	     nil)
      (bibl (let ((map (make-sparse-keymap))
		  (reference (cdr (assoc 'n (cadr elt)))))
	      (keymap-set map "RET" #'diogenes--lookup-button-invoke)
	      (keymap-set map "<mouse-1>" #'diogenes--lookup-button-invoke)
	      (keymap-set map "<mouse-2>" #'diogenes--lookup-button-invoke)
	      (list 'font-lock-face 'link
		    'keymap map
		    'action 'bibl
		    'bibl reference
		    'help-echo reference)))
      (quote (when (stringp (caddr elt))
	       (setf (caddr elt) (concat (caddr elt) " ")))
	     nil)
      (t (or (cdr (assoc tag diogenes--dict-xml-handlers-extra)))))))



;;; Let the user handle corrupt XML
;;; ... in the lookup mode
(let ((numeric-id 0))
  (defun diogenes--lookup-insert-xml (xml start end buffer)
    "Give the user the change to fix invalid XML in the dictionaries."
    (let* ((key (and (string-match "key=\"\\([^\"]+\\)\"" xml)
		     (match-string 1 xml)))
	   (id (or key (cl-incf numeric-id))))
      (message "Invalid xml in entry: Showing entry!")
      (insert (propertize (diogenes--fontify-nxml xml)
			  'invalid-xml id
			  'begin start
			  'end end))
      (setq diogenes--lookup-buffer buffer
	    diogenes--lookup-entry-id id
	    diogenes--lookup-bufstart start
	    diogenes--lookup-bufend end))))

(defun diogenes--lookup-xml-validate ()
  "Try to validate, parse, format and insert a corrected dictionary entry."
  (interactive)
  (let* ((id (or (get-text-property (point) 'invalid-xml)
	         (error "No XML here to validate!")))
	 (line-start (get-text-property (point) 'begin))
	 (line-end (get-text-property (point) 'end))
	 (prop-boundaries (diogenes--get-text-prop-boundaries 'invalid-xml))
	 (xml (apply #'buffer-substring prop-boundaries))
	 (parsed (diogenes--dict-parse-xml xml line-start line-end)))
    (apply #'delete-region prop-boundaries)
    (if parsed
	(diogenes--lookup-insert-and-format parsed)
      (insert (propertize (diogenes--fontify-nxml xml)
			  'invalid-xml id
			  'begin line-start
			  'end line-end)))))

(defun diogenes--fontify-nxml (str)
  "Use nxml-mode to fontify a string. 
All overlays added by rng-validate-mode are converted to text
properties."
  (with-temp-buffer
    (pop-to-buffer (current-buffer))
    (insert str)
    (nxml-mode)
    (rng-validate-mode)
    (font-lock-ensure)
    (cl-loop for ov in (overlays-in (point-min) (point-max))
	     for start = (overlay-start ov)
	     for end = (overlay-end ov)
	     for values = (overlay-properties ov)
	     when (eq (plist-get values 'category) 'rng-error)
	     do (add-text-properties
		 start end
		 (list 'face 'rng-error
		       'font-lock-face 'rng-error
		       'help-echo (plist-get values 'help-echo))))
    ;; (remove-overlays)
    (let ((map (make-sparse-keymap)))
      (keymap-set map "C-c C-c" #'diogenes--lookup-xml-validate)
      (keymap-set map "C-c '" #'diogenes--lookup-xml-edit)
      (propertize (buffer-string)
		  'keymap map))))

;;; ... in a dedicated NXML buffer
(defun diogenes--lookup-xml-edit ()
  "Edit a corrupt dictionary entry in XML-mode"
  (interactive)
  (let* ((id (or (get-text-property (point) 'invalid-xml)
	         (error "No corrupt XML at point to edit!")))
	 (prop-boundaries (diogenes--get-text-prop-boundaries 'invalid-xml))
	 (xml (apply #'buffer-substring prop-boundaries))
	 (lookup-buffer (current-buffer))
	 (xml-buffer (diogenes--get-fresh-buffer "xml"))
	 (map (make-sparse-keymap)))
    (keymap-set map "C-c C-c" #'diogenes--xml-submit)
    (pop-to-buffer xml-buffer)
    (nxml-mode)
    (insert (propertize xml
			'lookup-buffer lookup-buffer
			'id id
			'prop-boundaries prop-boundaries
			'keymap map))
    (rng-first-error)))

(defun diogenes--xml-submit ()
  "Try to submit a fixed XML dictionary entry."
  (interactive)
  (let* ((id (or (get-text-property (point) 'invalid-xml)
	         (error "No corrupt XML at point to edit!")))
	(prop-boundaries (diogenes--get-text-prop-boundaries 'invalid-xml))
	(lookup-buffer (get-text-property (point) 'lookup-buffer))
	(xml-buffer (current-buffer))
	(invalid-xml (with-current-buffer lookup-buffer
		       (save-excursion
			 (goto-char (point-min))
			 (text-property-search-forward 'invalid-xml id t))))
	(prop-start (prop-match-beginning invalid-xml))
	(prop-end (prop-match-end invalid-xml))
	(line-start (get-text-property prop-start 'begin))
	(line-end (get-text-property prop-start 'end))
	(parsed (diogenes--dict-parse-xml (buffer-string) line-start line-end)))
    (cond (parsed (kill-buffer xml-buffer)
		  (pop-to-buffer lookup-buffer)
		  (delete-region prop-start prop-end)
		  (diogenes--lookup-insert-and-format parsed))
	  (t (rng-first-error)))))



;;;; --------------------------------------------------------------------
;;;; PERSEUS PARSING
;;;; --------------------------------------------------------------------

(defun diogenes--parse-word (word lang)
  "Search the ananlyses file of lang for word using a binary search.
Returns the exact match. "
  (let* ((normalized (downcase (diogenes--beta-normalize-gravis
				(diogenes--greek-ensure-beta word))))
	 (analyses-file (file-name-concat (diogenes--perseus-path)
					  (concat lang "-analyses.txt")))
	 (index (diogenes--get-analyses-index "greek"))
	 (key (if (> (length normalized) 3) (substring normalized 0 3) normalized))
	 (start (let ((s (cdr (assoc key (plist-get index :index-start)))))
		  (if s (- s 2) 0)))
	 (end (or (cdr (assoc key (plist-get index :index-end)))
		  (plist-get index :index-max))))
    (let ((result (diogenes--binary-search analyses-file
					   #'diogenes--ascii-sort-function
					   #'diogenes--tab-key-fn
					   normalized
					   start end)))
      (unless (nth 3 result)
	(message "No result for %s! Showing nearest entry" word))
      (cons (and (car result)
		 (diogenes--format-parse-result (car result) lang))
	    (cdr result)))))

(defun diogenes--parse-all (query lang &optional filter)
  "Search all the forms in the analyses file.
Return all the entries whose keys match query when filter is applied to them.
Unless specified, filter defaults to string-equal."
  (let ((filter (or filter #'string-equal)))
    (cl-loop for k being the hash-keys of (diogenes--get-all-analyses lang)
	     using (hash-values v)
	     when (funcall filter query k)
	     collect (cons k v))))

(defun diogenes--format-parse-result (encoded-str lang)
  (cl-loop with str = (decode-coding-string encoded-str 'utf-8)
	   for entry in (split-string (cadr (diogenes--split-once "\t" str))
				      "[{}]" t "\\s-")
	   for (lemma-str translation analysis) = (split-string entry "\t")
	   for (lemma-nr lemma-cat lemma) = (split-string lemma-str)
	   collect
	   (list (cond ((string= lang "greek") (diogenes--beta-to-utf8 lemma))
		       (t lemma))
		 lemma-nr
		 translation
		 analysis)))

(defun diogenes--show-all-forms (query lang &optional filter)
  "Show all attested forms of a lemma in lang. 
When filter function is supplied, it determines what lemma should match."
  (let ((filter (or filter #'string-equal)))
    (cl-loop for k being the hash-keys of (car (diogenes--get-all-lemmata lang))
	     using (hash-values v)
	     when (funcall filter query k)
	     collect (cons k v))))

(defun diogenes--format-lemmata (lemma lang)
  (cl-loop for entry in (cdr lemma)
	   for (form analysis) = (diogenes--split-once "\\s-" entry)
	   collect (cons (cond ((string= lang "greek") (diogenes--beta-to-utf8 form)
				(t form)))
			 analysis)))




;;;; --------------------------------------------------------------------
;;;; PERSEUS LOW LEVEL FUNCTIONS
;;;; --------------------------------------------------------------------

(let ((cache (make-hash-table :test 'equal)))
  (defun diogenes--get-all-analyses (lang)
    (or (gethash (cons lang 'analyses) cache)
	(setf (gethash (cons lang 'analyses) cache)
	      (diogenes--analyses-file-to-hashtable
	       (file-name-concat (diogenes--perseus-path)
				 (concat lang "-analyses.txt"))))))
  
  (defun diogenes--get-all-lemmata (lang)
    (or (gethash (cons lang 'lemmata) cache)
	(setf (gethash (cons lang 'lemmata) cache)
	      (diogenes--lemmata-file-to-hashtable
	       (file-name-concat (diogenes--perseus-path)
				 (concat lang "-lemmata.txt")))))))

(let ((cache (make-hash-table :test 'equal)))
  (defun diogenes--get-analyses-index (lang)
    (or (gethash lang cache)
	(setf (gethash lang cache)
	      (diogenes--read-analyses-index lang)))))



;;; File access functions
(defun diogenes--analyses-file-to-hashtable (file)
  "Loads a whole analyses file as a hashtable into memory."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (cl-loop with table = (make-hash-table :test 'equal :size 950000)
	     with begin = 1
	     for tab = (re-search-forward "\t" nil t)
	     unless tab return table
	     for key = (buffer-substring begin (1- tab))
	     for newline = (or (re-search-forward "\n" nil t)
			       (point-max))
	     do (setf (gethash key table)
		      (buffer-substring tab (1- newline)))
	     do (setf begin newline))))

(defun diogenes--lemmata-file-to-hashtable (file)
  "Loads a whole lemmata file into memory.
Returns two hash-tables as a cons: The first is indexed by key,
the other by number."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (cl-loop with lemmata = (make-hash-table :test 'equal :size 950000)
	     with numbers = (make-hash-table :test 'equal :size 950000)
	     with begin = 1
	     for tab-1 = (re-search-forward "\t" nil t)
	     for tab-2 = (re-search-forward "\t" nil t)
	     unless tab-2 return (cons lemmata numbers)
	     for lem = (buffer-substring begin (1- tab-1))
	     for nr  = (string-to-number (buffer-substring tab-1 (1- tab-2)))
	     for newline = (or (re-search-forward "\n" nil t)
			       (point-max))
	     for entries = (split-string (buffer-substring tab-2 (1- newline))
					 "\t")  
	     do (setf (gethash lem lemmata) (cons nr entries))
	     do (setf (gethash nr numbers)  (cons lem entries))
	     do (setf begin newline))))

(defun diogenes--read-analyses-index-script (file)
  (diogenes--perl-script
   "sub quote {"
   "  local $_ = shift;"
   "  s/\\\\/\\\\\\\\/g;"
   "  s/\\\"/\\\\\\\"/gr"
   "}"
   "my (%index_start, %index_end, $index_max);"
   (format "open my $fh, '<', '%s' or die $!;" file)
   "eval do { undef local $/; <$fh> };"
   "print '(:index-start (';"
   "while ( my ($k, $v) = each %index_start ) { printf '(\"%s\" . %s)', quote($k), $v }"
   "print ') :index-end (';"
   "while ( my ($k, $v) = each %index_end   ) { printf '(\"%s\" . %s)', quote($k), $v }"
   "print qq') :index-max $index_max)';"))

(defun diogenes--read-analyses-index (lang)
  (let ((file (concat (diogenes--perseus-path) "/" lang "-analyses.idt")))
    (unless (file-exists-p file)
      (error "Cannot find %s idt file %s" lang file))
    (unless (file-readable-p file)
      (error "Cannot read %s idt file %s" lang file))
    (read
     (with-temp-buffer
       (unless (zerop (call-process
		       diogenes-perl-executable
		       nil '(t nil) nil
		       "-e" (diogenes--read-analyses-index-script file)))
	 (error "Perl exited with errors, no data received!"))
       (buffer-string)))))

(defun diogenes--binary-search (dict-file comp-fn key-fn word &optional start stop)
  "A binary search for finding entries in the lexicographical files. 
Upon success, it returns a list containing the entry, its start
and end offsets, and the symbol t to indicate success. Otherwise,
the nearest entry and its offsets are returned."
  (cl-loop with size = (file-attribute-size (file-attributes dict-file))
	   with left = (or start 0)
	   with right = (or stop size)
	   unless (< left right) return (list buf buf-start buf-end)
	   for mid = (floor (+ left right) 2)
	   for (buf buf-start buf-end)
	   = (diogenes--get-dict-line dict-file mid size)
	   for (key value) = (funcall key-fn buf)
	   for comp-result = (funcall comp-fn key word)
	   unless comp-result return (list buf buf-start buf-end t)
	   do (cond ((eq comp-result 'a) (setq right (1- buf-start)))
		    ((eq comp-result 'b) (setq left (1+ buf-end))))))

(defun diogenes--get-dict-line (file pos &optional file-length)
  "Jump at POS into a FILE, and returns the next complete line.
It returns additionally the start and end offsets of the line.
If file-length is not supplied, it will be determined."
  (setq file-length (or file-length
			(file-attribute-size (file-attributes file))))
  (let ((bufsize 5000)
	(buf-start 0)
	(line-start 1)
	line-end)
    (with-temp-buffer 
      (unless (zerop pos)
	(seq-setq (line-start buf-start)
		  (diogenes--read-backward-until-newline file pos bufsize))
	(goto-char (point-max)))
      (seq-setq (line-end)
		(diogenes--read-forward-until-newline file pos bufsize))
      (when (and line-start line-end)
	(cl-decf line-end)		; Chop off newline
	(list (buffer-substring line-start line-end)
	      (+ buf-start (1- line-start))
	      (+ buf-start (1- line-end)))))))

(defun diogenes--read-forward-until-newline (file file-pos bufsize)
  "Try to read forward from a file until the next newline."
  (when file-pos
    (cl-loop for newline = (re-search-forward "\n" nil t)
	     when newline return (list newline file-pos)
	     for chars-read = (progn (goto-char (point-max))
				     (cadr (insert-file-contents-literally
					    file nil
					    file-pos (+ file-pos bufsize))))
	     when (zerop chars-read) return nil
	     do (cl-incf file-pos chars-read))))

(defun diogenes--read-backward-until-newline (file file-pos bufsize)
  "Try to read backward from a file untilg the next newline."
  (when file-pos
    (cl-loop for newline = (re-search-backward "\n" nil t)
	     when newline return (list (1+ newline) file-pos)
	     for chars-read = (progn (goto-char (point-min))
				     (cadr (insert-file-contents-literally
					    file nil
					    (let ((start (- file-pos bufsize)))
					      (if (> start 0) start 0))
					    file-pos)))
	     when (zerop chars-read) return (list 1 0)
	     do (forward-char chars-read)
	     do (cl-decf file-pos chars-read))))



;;; Sort functions
;;; ASCII
(defun diogenes--ascii-sort-function (a b)
  (let ((word-a (downcase a))
	(word-b (downcase b)))
    (cond ((string-greaterp word-a word-b) 'a)
	  ((string-greaterp word-b word-a) 'b)
	  (t nil))))

(defconst diogenes--beta-code-alphabet
  [?0 ?a ?b ?g ?d ?e ?v ?z ?h ?q
      ?i ?k ?l ?m ?n ?c ?o ?p
      ?r ?s ?t ?u ?f ?x ?y ?w]
  "The greek alphabet in beta code.")

;;; BETA CODE
(defun diogenes--beta-sort-function (a b)
  (let ((a (downcase (diogenes--ascii-alpha-only a)))
	(b (downcase (diogenes--ascii-alpha-only b))))
    (cl-case
	(cl-loop for i from 0 to (1- (min (length a) (length b)))
		 for pos-char-a = (cl-position (elt a i)
					       diogenes--beta-code-alphabet)
		 for pos-char-b = (cl-position (elt b i)
					       diogenes--beta-code-alphabet)
		 do (cond ((not pos-char-a)
			   (error "Illegal character %c" (elt a i)))
			  ((not pos-char-b)
			   (error "Illegal character %c" (elt b i))))
		 if (> pos-char-a pos-char-b) return 'a
		 if (> pos-char-b pos-char-a) return 'b)
      (a 'a)
      (b 'b)
      (t (cond ((> (length a) (length b)) 'a)
	       ((> (length b) (length a)) 'b)
	       (t nil))))))

;;; Key function
(defun diogenes--tab-key-fn (buf)
  (let ((split (string-match "\t" buf)))
    (when split (list (substring buf 0 split)
		      (substring buf (1+ split))))))

(defun diogenes--xml-key-fn (buf)
  (if (string-match "key\\s-*=\\s-*\"\\([^\"]*\\)\""
		    buf)
      (list (match-string-no-properties 1 buf)
	    buf)
    (error "Could not find key in str:\n %s" buf)))

;;;; --------------------------------------------------------------------
;;;; USER INTERFACE FUNCTIONS
;;;; --------------------------------------------------------------------

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



;;;; --------------------------------------------------------------------
;;;; PERL INTERFACE
;;;; --------------------------------------------------------------------

;;; Perl scripts
(defun diogenes--search-script (option-plist &optional authors-plist)
  "Return a perl script that executes a diogenes search.

option-plist is an plist that will be converted into a perl hash
accepted by the Diogenes::Search constructor. authors-plist, when supplied,
are the arguments for the select_authors method."
  (plist-put option-plist :chunk-size "inf")
  (diogenes--perl-script
   "use Diogenes::Search;"
   (format "my $q = Diogenes::Search->new(%s);"
	   (diogenes--list->perl option-plist))
   (when authors-plist
     (format "$q->select_authors(%s);"
	     (diogenes--list->perl authors-plist)))
   "$q->do_search"))

(defun diogenes--list-authors-script (option-plist &optional author-regex)
  "Return a perl script that returns a list of all authors in a corpus."
  (diogenes--perl-script
   "use Diogenes::Browser;"
   diogenes--perl-to-lisp-sub
   (format "my $q = Diogenes::Browser->new(%s);" (diogenes--list->perl option-plist))
   (format "my %%a = $q->browse_authors(\"%s\");" (or author-regex ""))
   "print perl_to_lisp([ map {[ $a{$_}, $_ ]} sort keys %a ]);"))

(defun diogenes--list-works-script (option-plist author)
  "Return a perl script that returns a list of all works of an author in a corpus."
  (diogenes--perl-script
   "use Diogenes::Browser;"
   diogenes--perl-to-lisp-sub
   (format "my $q = Diogenes::Browser->new(%s);" (diogenes--list->perl option-plist))
   (format "my %%w = $q->browse_works(\"%s\");" author)
   "print perl_to_lisp([ map {[ \"$w{$_} ($_)\", $_ ]} sort keys %w ]);"))

(defun diogenes--list-work-labels-script (option-plist author-and-work)
  "Return a perl script that returns a list of the defined labels for a work."
  (diogenes--perl-script
   "use Diogenes::Browser;"
   diogenes--perl-to-lisp-sub
   (format "my $q = Diogenes::Browser->new(%s);" (diogenes--list->perl option-plist))
   (format "my @l = $q->browse_location(%s);" (diogenes--list->perl author-and-work))
   "print perl_to_lisp(\\@l);"))

(defun diogenes--get-tlg-categories-script (&rest junk)
  "Return a perl script that returns a list of all categories in the tlg."
  (diogenes--perl-script
   "use Diogenes::Search;"
   diogenes--perl-to-lisp-sub
   "my $q = Diogenes::Search->new(type => 'tlg');"
   "my $c = $q->select_authors(get_tlg_categories => 1);"
   "print perl_to_lisp($c);"))

(defun diogenes--browser-script (option-plist passage)
  "Return a perl script that opens a work with the Diogenes Browser."
  (diogenes--perl-script
   "use Diogenes::Browser;"
   (format "my $q = Diogenes::Browser->new(%s);"
	   (diogenes--list->perl option-plist))
   (format "$q->seek_passage(%s);"
	   (diogenes--list->perl passage))
   ;; "$q->browse_half_backward();"
   "$q->browse_forward();"))

(defconst diogenes--browse-interactively-parse-capture-sub
   "my $capture = '';
open my $fh, '>', \\$capture;
select $fh;
sub parse_capture {
  my $out = '';
  $capture =~ s/([()\"])/\\\\$1/g;
  $capture =~ s/\\0\\n?//g;
  my ($header, $body) = split /\\n\\n/, $capture, 2;
  my @header_lines = split /\\n/, $header;
  my $last_header_line = pop @header_lines;
  my @levels = $last_header_line =~ /(\\S+)(?:,|$)/g;
  $out .= qq#(\"$last_header_line\" #;
  $out .= '(';
  $out .= join ' ', map qq#\"$_\"#, @header_lines;
  $out .= ') ';
  for my $line (split /\\n/, $body) {
    my ($label, $text) = $line =~ /^(\\S*)(.*)$/g;
    next unless $text && $text =~ /\\S/;
    $text = substr $text, 14 - length $label;
    if ($label and not $label =~ /^\\D/) {
      @levels = split /\\./, $label 
    }
    $out .= qq#((@levels) . \"$text\")#;
    if ( $levels[-1] eq 't' ) { $levels[-1] = 1 }
    else                      { $levels[-1]++ }
  }
  $out .= qq#)\\n#;
  $capture = '';
  print STDOUT $out;
  STDOUT->eof();
}\n"
   "Source code for a perl sub that captures and parses the browser output.

It addes the missing line numbers and prints a LISP list
containing the header as a string an a series of a-lists whose keys are a
lists of the citations and whose values are the lines.")

(defun diogenes--browse-interactively-script (option-plist passage)
  "Return a perl script that browses a work form a corpus interactively."
  (diogenes--perl-script
   "use Diogenes::Browser;"
   "STDOUT->autoflush(1);"
   ;; diogenes--discard-perl-stderr
   diogenes--browse-interactively-parse-capture-sub
   "my ($beg, $end);"
   (format "my ($author, $work) = ( \"%s\", \"%s\" );"
	   (car passage) (cadr passage))
   (format "my $b = Diogenes::Browser::Stateless->new(%s);"
	   (diogenes--list->perl option-plist))
   (format "($beg, $end) = $b->seek_passage(%s);"
	   (diogenes--list->perl passage))
   "(undef, $end) = $b->browse_forward( $beg, $end, $author, $work );"
   "parse_capture;"
   "while (<STDIN>) {"
   "  chomp;"
   "  if (s/^(\\d+)//) { $b->{browse_lines} = $1 - 1 }"
   "  if    (/^q$/) { last }"
   "  elsif (/^[nf]$/) {"
   "    (undef, $end) = $b->browse_forward( $beg, $end, $author, $work );"
   "    parse_capture;"
   "  }"
   "  elsif (/^[pb]$/)  {"
   "    ($beg, undef) = $b->browse_backward( $beg, $end, $author, $work );"
   "    parse_capture;"
   "  }"
   "}"))



;;; Perl Callers
(defun diogenes--read-info (script)
  (read
   (with-temp-buffer
     (unless (zerop (call-process diogenes-perl-executable
				  nil '(t nil) nil
				  "-e" script
				  (diogenes--include-server)
				  (diogenes--include-cpan)))
       (error "Perl exited with errors, no data received!"))
     (buffer-string))))

(let ((cache (make-hash-table :test 'equal)))
  (defun diogenes--get-info (script &optional options1 options2)
    (let ((key (list script options1 options2)))
      (or (gethash key cache)
	  (setf (gethash key cache)
		(diogenes--read-info (apply script
					    (list options1 options2))))))))

(defun diogenes--get-author-list (options &optional author-regex)
  (diogenes--get-info #'diogenes--list-authors-script
		      options author-regex))

(defun diogenes--get-works-list (options author)
  (diogenes--get-info #'diogenes--list-works-script options author))

(defun diogenes--get-work-labels (options author-and-work)
  (diogenes--get-info #'diogenes--list-work-labels-script
		      options author-and-work))

(defun diogenes--get-tlg-categories ()
  (diogenes--get-info #'diogenes--get-tlg-categories-script))



;;; Perl Runners
(defun diogenes--start-perl (type code &optional filter)
  "Starts and a perl process named diogenes-type.
It is associated with a buffer with the same name, in asterisks."
  (let ((buffer (diogenes--get-fresh-buffer type)))
    (make-process :name    (format "diogenes-%s" type)
		  :buffer  buffer
		  :command (list diogenes-perl-executable
				 "-e" code
				 (diogenes--include-server)
				 (diogenes--include-cpan))
		  :coding  'utf-8
		  :stderr  " *diogenes-warnings*"
		  :noquery t
		  :filter filter)
    (pop-to-buffer buffer)))

(defun diogenes--get-fresh-buffer (type)
  "Returns a fresh buffer for the mode to use."
  (if (get-buffer (format "*diogenes-%s*" type))
      (cl-loop for i from 1
	       unless (get-buffer (format "*diogenes-%s<%d>*" type i))
	       return (get-buffer-create (format "*diogenes-%s<%d>*" type i)))
    (get-buffer-create (format "*diogenes-%s*" type))))

(defun diogenes--get-comint-buffer (mode)
  "Returns a fresh buffer for comint to use."
  (if (get-buffer (format "*%s*" mode))
      (cl-loop for i from 1
	       unless (get-buffer (format "*%s<%d>*" mode i))
	       return (get-buffer-create (format "*%s<%d>*" mode i)))
    (get-buffer-create (format "*%s*" mode))))

(defun diogenes--make-comint (mode script)
  "Make a new buffer and connect it to a perl script.
Mode should be a maior mode derived from comint-mode."
  (let* ((name (replace-regexp-in-string "-mode$" ""
					   (symbol-name mode)))
	 (buffer (diogenes--get-comint-buffer name)))
    (make-comint-in-buffer name buffer "perl" nil
			   "-e" script
			   (diogenes--include-server)
			   (diogenes--include-cpan))
    (pop-to-buffer buffer)
    (funcall mode)))

;; Use this function to debug scripts created buy a diogenes--*script
;; function.
(defun diogenes--debug-perl (script)
  "Output a script generated by a diognes--*script function in a new buffer.
It can then be edited, saved to file and called directly for better testing."
  (switch-to-buffer (generate-new-buffer "*diogenes-perldebug*"))
  (insert script)
  (cperl-mode))

(defun diogenes--perl-execute-current-buffer (in-buffer)
  "Execute the contents of a buffer as a perl program"
  (interactive "b")
  (with-current-buffer in-buffer
    (let ((out-buffer (diogenes--get-fresh-buffer "perl-run")))
      (make-process :name    "perl-run"
		    :buffer  out-buffer
		    :command (list diogenes-perl-executable
				   "-e" (buffer-string)
				   (diogenes--include-server)
				   (diogenes--include-cpan))
		    :coding  'utf-8)
      (pop-to-buffer out-buffer))))



;;; Low Level Perl Interface
(defmacro diogenes--perl-script (&rest lines)
  `(string-join (list (format "use v%.2f;" diogenes-perl-min-version)
		      "use strict;"
		      "use warnings;"
		      ;;diogenes--discard-perl-stderr
		      ,@lines)
		"\n"))

(defconst diogenes--perl-to-lisp-sub
  "sub perl_to_lisp {
  my $elt = shift;
  unless ($elt) {}
  elsif (ref $elt eq 'ARRAY') {
    my $str = join ' ', map { perl_to_lisp($_) } @$elt;
    '('. $str . ')';
  }
  elsif (ref $elt eq 'HASH') {
    my $str = join ' ',
      map { ':' . $_ . perl_to_lisp($elt->{$_}) } sort keys %$elt;
    '(' . $str . ')';
  }
  elsif (ref $elt) {
    die 'No rule for ', ref $elt, 'defined!';
  }
  # elsif ($elt =~ /^\\d+$/) { $elt }
  else { '\"' . $elt . '\"' }
}"
  "Source for a perl sub that prints perl data for the LISP reader.")

(defconst diogenes--discard-perl-stderr
  "local *STDERR;
my $junk = '';
open *STDERR, '>', \\$junk;"
  "Perl source that discards all output to STDERR in current scope.")

(defun diogenes--elt->perl (elt)
  "Transform lisp data structures to analogous perl ones.

An ordinary list becomes a arrayref, a plist with keywords at odd
positions an arrayref."
  (cl-typecase elt
    (list (diogenes--list->perl elt t))
    (keyword (replace-regexp-in-string "-" "_"
				       (diogenes--keyword->string elt)))
    (t (prin1-to-string elt))))

(defun diogenes--list->perl (lst &optional ref)
  "Transform a lisp list into a perl list.

If ref is non-nil, make the list into a hash- or an arrayref."
  (unless (listp lst) (error "Not a list: %s" lst))
  (if (and lst
	   (diogenes--plist-keyword-keys-p lst))
      (format (if ref "{ %s }" "%s")
	      (diogenes--plist->perlpairs lst))
    (format (if ref "[ %s ]" "%s")
	    (mapconcat #'diogenes--elt->perl lst ", "))))

(defun diogenes--plist-keyword-keys-p (plist)
  "Check if all keys of a plist are keywords"
  (cond ((not (plistp plist)) nil)
	((cdr plist) (and (keywordp (car plist))
			  (diogenes--plist-keyword-keys-p (cddr plist))))
	(t t)))

(defun diogenes--plist->perlpairs (plist)
  (when plist
    (concat (diogenes--elt->perl (car plist))
	    " => "
	    (diogenes--elt->perl (cadr plist))
	    (let ((next-pair (diogenes--plist->perlpairs (cddr plist))))
	      (when next-pair (concat ", " next-pair))))))

;;;; --------------------------------------------------------------------
;;;; USER UTILITIES
;;;; --------------------------------------------------------------------

;;; Conversion from and to greek beta code
(defun diogenes--beta-to-utf8 (str)
  "Convert a string from greek beta code to utf-8."
  (replace-regexp-in-string
   "σ\\(?:\\b\\|$\\)" "ς"
   (with-temp-buffer
     (insert str)
     (translate-region (point-min) (point-max)
		       diogenes--beta-to-utf8-table)
     (buffer-string))))

(defun diogenes--utf8-to-beta (str)
  "Convert utf-8 greek in a string to greek beta code."
  (with-temp-buffer
    (insert str)
    (translate-region (point-min) (point-max)
		      diogenes--utf8-to-beta-table)
    (buffer-string)))

(defun diogenes--greek-ensure-beta (str)
  "Ensures that a greek string is encoded in beta code."
  (save-match-data
    (let ((latin (string-match "\\cr" str))
	  (greek (string-match "\\cg" str)))
      (when (and latin greek)
	(error "\"%s\" contains both Latin and Greek characters!" str))
      (if greek (diogenes--utf8-to-beta str)
	str))))

(defun diogenes--beta-normalize-gravis (str)
  (replace-regexp-in-string "\\\\" "/" str))

(defconst diogenes--beta-to-utf8-map
  '(;; MAJUSCULE
    ;; acute + smooth B.
    ("*)/a".?Ἄ)("*)/e".?Ἔ)("*)/h".?Ἤ)("".?Ἴ)("*)/o".?Ὄ)("*)/w".?Ὤ) ;;("*)/u".?῎Υ )
    ;; acute + rough b.
    ("*(/a".?Ἅ)("*(/e".?Ἕ)("*(/h".?Ἥ)("*(/i".?Ἵ)("*(/o".?Ὅ)
    ("*(/u".?Ὕ)("*(/w".?Ὥ)("*)/a".?Ἂ)
    ;; grave + smooth b.
    ("*)/e".?Ἒ)("*)/h".?Ἢ)("*)/i".?Ἲ)("*)/o".?Ὂ)("*)/w".?Ὢ) ;;("*)/u".?Υ̓̀)
    ;; grave + rough b.
    ("*(/a".?Ἃ)("*(/e".?Ἓ)("*(/h".?Ἣ)("*(/i".?Ἳ)("*(/o".?Ὃ)("*(/u".?Ὓ)("*(/w".?Ὣ)
    ;; circumflex + smooth b.
    ("*)=a".?Ἆ)("*)=h".?Ἦ)("*)=i".?Ἶ)("*)=w".?Ὦ) ;; (?.?*)=u")
    ;; circumflex + rough b.
    ("*(=a".?Ἇ)("*(=h".?Ἧ)("*(=i".?Ἷ)("*(=u".?Ὗ)("*(=w".?Ὧ)
    ;; smooth b.
    ("*)a".?Ἀ)("*)e".?Ἐ)("*)h".?Ἠ)("*)i".?Ἰ)("*)o".?Ὀ)("*)u".?Ὑ)("*)w".?Ὠ)
    ;; rough b.
    ("*(a".?Ἁ)("*(e".?Ἑ)("*(h".?Ἡ)("*(i".?Ἱ)("*(o".?Ὁ)("*(u".?Ὑ)("*(w".?Ὡ)
    ;; rho 
    ("*(r".?Ῥ)
    ;; simple majuscule
    ("*a".?Α)("*b".?Β)("*g".?Γ)("*d".?Δ)("*e".?Ε)("*z".?Ζ)("*h".?Η)("*q".?Θ)
    ("*i".?Ι)("*k".?Κ)("*l".?Λ)("*m".?Μ)("*n".?Ν)("*c".?Ξ)("*o".?Ο)("*p".?Π)
    ("*r".?Ρ)("*s".?Σ)("*t".?Τ)("*u".?Υ)("*f".?Φ)("*x".?Χ)("*y".?Ψ)("*w".?Ω)
    ;; MINUSCULE
    ;; circumflex rough b. iota    
    ("a(=|".?ᾇ)("h(=|".?ᾗ)("w(=|".?ᾧ)
    ;; circumflex smooth b. iota
    ("a)=|".?ᾆ)("h)=|".?ᾖ)("w)=|".?ᾦ)
    ;; circumflex iota
    ("a=|".?ᾷ)("h=|".?ῇ)("w=|".?ῷ)
    ;; circumflex rough b.
    ("a(=".?ἇ)("h(=".?ἧ)("i(=".?ἷ)("u(=".?ὗ)("w(=".?ὧ)
    ;; circumflex smooth b.
    ("a)=".?ἆ)("h)=".?ἦ)("i)=".?ἶ)("u)=".?ὖ)("w)=".?ὦ)
    ;; circumflex
    ("a=".?ᾶ)("h=".?ῆ)("i=".?ῖ)("u=".?ῦ)("w=".?ῶ)
    ;; grave rough b. iota
    ("a(\\|".?ᾃ)("h(\\|".?ᾓ)("w(\\|".?ᾣ)
    ;; grave smooth b. iota	    
    ("a)\\|".?ᾂ)("h)\\|".?ᾒ)("w)\\|".?ᾢ)
    ;; grave iota 
    ("a\\|".?ᾲ)("h\\|".?ῂ)("w\\|".?ῲ)
    ;; grave rough b.
    ("a(\\".?ἃ)("e(\\".?ἓ)("h(\\".?ἣ)("i(\\".?ἳ)("o(\\".?ὃ)("u(\\".?ὓ)("w(\\".?ὣ)
    ;; grave smooth b.
    ("a)\\".?ἂ)("e)\\".?ἒ)("h)\\".?ἢ)("i)\\".?ἲ)("o)\\".?ὂ)("u)\\".?ὒ)("w)\\".?ὢ)
    ;; grave
    ("a\\".?ὰ)("e\\".?ὲ)("h\\".?ὴ)("i\\".?ὶ)("o\\".?ὸ)("u\\".?ὺ)("w\\".?ὼ)
    ;; acute rough b. iota
    ("a(/|".?ᾅ)("h(/|".?ᾕ)("w(/|".?ᾥ)
    ;; acute smooth b. iota	    
    ("a)/|".?ᾄ)("h)/|".?ᾔ)("w)/|".?ᾤ)
    ;; acute iota 
    ("a/|".?ᾴ)("h/|".?ῄ)("w/|".?ῴ)
    ;; acute rough b.
    ("a(/".?ἅ)("e(/".?ἕ)("h(/".?ἥ)("i(/".?ἵ)("o(/".?ὅ)("u(/".?ὕ)("w(/".?ὥ)
    ;; acute smooth b.
    ("a)/".?ἄ)("e)/".?ἔ)("h)/".?ἤ)("i)/".?ἴ)("o)/".?ὄ)("u)/".?ὔ)("w)/".?ὤ)
    ;; acute	    
    ("a/".?ά)("e/".?έ)("h/".?ή)("i/".?ί)("o/".?ό)("u/".?ύ)("w/".?ώ)
    ;; tonos...	    
    ("a/".?ά)("e/".?έ)("h/".?ή)("i/".?ί)("o/".?ό)("u/".?ύ)("w/".?ώ)
    ;; rough b. iota
    ("a(|".?ᾁ)("h(|".?ᾑ)("w(|".?ᾡ)	    
    ;; smooth b. iota
    ("a)|".?ᾀ)("h)|".?ᾐ)("w)|".?ᾠ)
    ;; iota
    ("a|".?ᾳ)("h|".?ῃ)("w|".?ῳ)
    ;; rough b.
    ("a(".?ἁ)("e(".?ἑ)("h(".?ἡ)("i(".?ἱ)("o(".?ὁ)("u(".?ὑ)("w(".?ὡ)
    ;; spiritus smooth b.
    ("a)".?ἀ)("e)".?ἐ)("h)".?ἠ)("i)".?ἰ)("o)".?ὀ)("u)".?ὐ)("w)".?ὠ)
    ;; rho
    ("r(".?ῥ)("r)".?ῤ)			;("s ".?ς)
    ;; simple minuscule
    ("a".?α)("b".?β)("g".?γ)("d".?δ)("e".?ε)("z".?ζ)("h".?η)("q".?θ)
    ("i".?ι)("k".?κ)("l".?λ)("m".?μ)("n".?ν)("c".?ξ)("o".?ο)("p".?π)
    ("r".?ρ)("s".?σ)("t".?τ)("u".?υ)("f".?φ)("x".?χ)("y".?ψ)("w".?ω)
    ("s".?ς)
    ;; interpunction
					;("'".?᾿)
    )
  "Mapping of greek beta code to utf-8 greek.")

(defconst diogenes--beta-to-utf8-table
  (make-translation-table-from-alist
   (mapcar (lambda (cons)
	     (cons (string-to-vector (car cons))
		   (cdr cons)))
	   diogenes--beta-to-utf8-map))
  "Translation table to convert greek beta code to utf-8.")

(defconst diogenes--utf8-to-beta-table
  (make-translation-table-from-alist
   (mapcar (lambda (cons)
	     (cons (cdr cons)
		   (string-to-vector (car cons))))
	   diogenes--beta-to-utf8-map))
  "Translation table to convert utf-8 greek to beta code.")



;;; Conversion between A.D. and Ol.
(defun diogenes--ol-to-ad (ol)
  (let ((with-0-ad (- (* ol 4) 780)))
    (if (< with-0-ad 0) with-0-ad
      (1+ with-0-ad))))

(defun diogenes--bc-and-ad (year)
  (if (< year 0)
      (format "%s B.C." (- year))
    (format "%s A.D." year)))

;;;; --------------------------------------------------------------------
;;;; Legacy post-processing functions (TODO: should be superseded)
;;;; --------------------------------------------------------------------

;;;###autoload
(defun diogenes-apostrophe (&optional start end)
  "Replace all greek apostrophes with the typographical correct ῾."
  (interactive "r")
  (save-excursion
    (save-restriction
      (when (use-region-p)
        (progn (narrow-to-region start end)
               (goto-char (point-min))))
      (replace-regexp "\\([[:nonascii:]]+\\)['’]" "\\1᾿"))))

;; Problems with bracketed [] words
;;;###autoload
(defun diogenes-remove-hyphenation-greek (&optional start end)
  "Delete hypenation in active region or until EOBP.
It works by adding the remainder of the word to the truncated
word and deleting it in the following line. Works on the rest of
the buffer starting at point, or on the active region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (when (use-region-p)
        (progn (narrow-to-region start end)
               (goto-char (point-min))))
      (while (re-search-forward "[^<]- ?$" (point-max) t)
        (delete-horizontal-space)
        (delete-char -1)
        (push-mark)
        (forward-line)
        (re-search-forward "[[:nonascii:]]+[.,·:;!?']?" (point-max) t)
        (delete-horizontal-space)
        (kill-word -1)
        (exchange-point-and-mark)
        (yank)
        (pop-mark)
        (setq kill-ring (cdr kill-ring))
        (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer))))))

;;;###autoload
(defun diogenes-remove-hyphenation (col &optional start end)
  "Delete hypenation in active region or until EOBP.
It works by adding the remainder of the word (starting at column
14, or column PREFIX) to the truncated word and deleting it in the following line.
Works on the rest of the buffer starting at point, or on the
active region."
  (interactive "p/nr")
  (when (= 1 col) (setq col 14))
  (save-excursion
    (save-restriction
      (when (use-region-p)
        (progn (narrow-to-region start end)
               (goto-char (point-min))))
      (while (re-search-forward "[^<]- ?$" (point-max) t)
        (delete-horizontal-space)
        (delete-char -1)
        (push-mark)
        (forward-line)
        (move-to-column (or col 14))
        (re-search-forward "[[:alpha:]]+[.,·:;!?']?" (point-max) t)
        (delete-horizontal-space)
        (kill-word -1)
        (exchange-point-and-mark)
        (yank)
        (pop-mark)
        (setq kill-ring (cdr kill-ring))
        (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer))))))

;;;###autoload
(defun diogenes-delete-line-numbers ()
  "Delete line numbers, starting at point"
  (interactive)
  (save-excursion
    (let ((start (progn
                   (unless
                       (re-search-backward
                        "\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]: [[:digit:]][[:digit:]][[:digit:]]\)"
                        nil t)
                     (re-search-forward
                      "\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]: [[:digit:]][[:digit:]][[:digit:]]\)"))
                   (re-search-forward "^$")
                   (point)))
          (end (progn
                 ;; (goto-char (point-max))
                 (unless
                     (re-search-forward "diogenes-browse finished" nil t)
                   (goto-char (point-max)))
                 (re-search-backward "[Α‐ω]")
                 (beginning-of-line)
                 (forward-char 14)
                 (point))))
      (delete-rectangle start end))))

;;;###autoload
(defun diogenes-tidy-up-search-results ()
  "Post-processes search results of diogenes"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (diogenes-unhyphen-greek)
    (goto-char (point-min))
    (replace-regexp "->\\([[:alpha:]]*\\)<-\\([[:alpha:]]*\\)" "-> \\1\\2 -<")))

;;;; --------------------------------------------------------------------
;;;; Lisp Utilities
;;;; --------------------------------------------------------------------

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

(defmacro diogenes--replace-regexes-in-string (str &rest subst-lists)
  "Apply a list of regex-substitutions to a string in sequence.
Returns the resulting string"
  (let ((result str))
    (dolist (subst subst-lists result)
      (let ((regex (if (listp subst)
		       (car subst)
		     subst))
	    (rep   (if (listp subst)
		       (or (cadr subst) "")
		     ""))
	    (rest  (cddr subst)))
	(setf result `(replace-regexp-in-string ,regex ,rep ,result ,@rest))))))

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

(defun diogenes--ascii-alpha-only (str)
  (cl-remove-if-not #'diogenes--ascii-alpha-p str))

(provide 'diogenes)

;;; diogenes.el ends here
