;;; diogenes.el --- Interface to diogenes -*- lexical-binding: t -*-

;; An interface to Peter Heslin's Diogenes
;; Copyright (C) 2024 Michael Neidhart
;;
;; Author: Michael Neidhart <mayhoth@gmail.com>
;; Keywords: classics, tools, philology, humanities
;;
;; Version: 0.4
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
;; - Browser Mode: Try to read reference at point
;;     - FIXME: browse-forward and browse-backward broken in latin texts
;; - Info Manual

;;; Code:
;; (require 'transient)
(require 'cl-lib)
(require 'thingatpt)
(require 'seq)

(require 'diogenes-lisp-utils)
(require 'diogenes-utils)
(require 'diogenes-perl-interface)
(require 'diogenes-user-interface)
(require 'diogenes-browser)
(require 'diogenes-search)
(require 'diogenes-perseus)
(require 'diogenes-legacy)


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
  "Search for a phrase in the Miscellaneous PHI Texts Database.
Uses the Diogenes Perl module."
  (interactive "i\ni\np")
  (diogenes--search-database "misc" options-or-pattern author-plist prefix))

;;;###autoload
(defun diogenes-search-cop (options-or-pattern
			    &optional author-plist prefix)
  "Search for a phrase in the PHI Coptic Texts Database.
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
  (diogenes--lookup-dict word "greek"))

;;;###autoload
(defun diogenes-lookup-latin (word)
  "Search for a greek word in the Lewis & Short Latin Dictionary."
  (interactive (list (read-from-minibuffer "Search Lewis & Short for: "
					   (thing-at-point 'word t))))
  (diogenes--lookup-dict word "latin"))

;;; MORPHEUS PARSING
;;;###autoload
(defun diogenes-parse-and-lookup-greek (word)
  "Try to parse a greek word."
  (interactive (list (read-from-minibuffer "Parse greek word: "
					   (thing-at-point 'word t))))
  (diogenes--parse-and-lookup (diogenes--greek-ensure-beta word)
			      "greek"))

;;;###autoload
(defun diogenes-parse-and-lookup-latin (word)
  "Try to parse a latin word."
  (interactive (list (read-from-minibuffer "Parse greek word: "
					   (thing-at-point 'word t))))
  (diogenes--parse-and-lookup word "latin"))

;;;###autoload
(defun diogenes-parse-greek (query)
  "Parse a greek word and display the results.
QUERY is interpreted as a regular expression which must match the forms."
  (interactive (list (read-from-minibuffer "Parse Greek word: "
					   (thing-at-point 'word t))))
    (diogenes--parse-and-show (diogenes--greek-ensure-beta query)
			      "greek"))

;;;###autoload
(defun diogenes-parse-latin (query)
  "Parse a latin word and display the results.
QUERY is interpreted as a regular expression which must match the forms."
  (interactive (list (read-from-minibuffer "Parse Latin word: "
					   (thing-at-point 'word t))))
  (diogenes--parse-and-show query "latin"))

;;;###autoload
(defun diogenes-show-all-forms-greek (lemma)
  "Show all attested forms of a Greek lemma."
  (interactive "sShow all forms of: ")
  (diogenes--show-all-forms (diogenes--greek-ensure-beta lemma) "greek"))

;;;###autoload
(defun diogenes-show-all-forms-latin (lemma)
  "Show all attested forms of a Latin lemma."
  (interactive "sShow all forms of: ")
  (diogenes--show-all-forms lemma "latin"))


;;; UTILITIES
;;;###autoload
(defun diogenes-utf8-to-beta (str)
  "Convert greek beta code to utf-8. 
If a region is active, convert the contents of the region in place; 
otherwise, prompt the user for input."
  (interactive "i")
  (cond (str (diogenes--uft8-to-beta str))
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
  (cond (str (diogenes--beta-to-utf8 str))
	((use-region-p) (let ((start (min (point) (mark)))
			      (end (max (point) (mark))))
			  (translate-region start end diogenes--beta-to-utf8-table)
			  (replace-regexp-in-region "σ\\b\\|σ$" "ς" start end)))
	(t (let ((str (read-from-minibuffer "Convert from Greek Beta Code: "
					    nil nil nil nil
					    (thing-at-point 'word t))))
	     (message "%s" (diogenes--beta-to-utf8 str))))))

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



(provide 'diogenes)

;;; diogenes.el ends here
