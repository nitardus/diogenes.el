;;; diogenes.el --- Interface to diogenes -*- lexical-binding: t -*-

;; An interface to Peter Heslin's Diogenes
;; Copyright (C) 2024 Michael Neidhart
;;
;; Author: Michael Neidhart <mayhoth@gmail.com>
;; Keywords: classics, tools, philology, humanities
;;
;; Version: 0.55
;; Package-Requires: (cl-lib thingatpt seq transient)

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
;; -  Coptic!
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
;; - Browser Mode:
;;     - Readonly mode, view-mode key bindings
;;     - Make keybindings work in Evil Mode
;; - Info Manual

;;; Code:
(require 'cl-lib)
(require 'thingatpt)
(require 'transient)
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
  (warn "Could not find a working Diogenes installation in %s"
	 (diogenes--path)))

(mapc (lambda (lang)
	(unless (file-exists-p (diogenes--dict-file lang))
	  (warn "Could not find %s lexicon at %s."
		 lang (diogenes--dict-file lang))))
      '("greek" "latin"))

(mapc (lambda (file)
	(unless (file-exists-p (file-name-concat (diogenes--perseus-path)
						 file))
	  (warn "Could not find %s in %s. Did you build them?"
		 file (diogenes--perseus-path))))
      '("greek-analyses.txt" "greek-lemmata.txt"
	"latin-analyses.txt" "latin-lemmata.txt"))

(defconst diogenes--corpora
  '(("phi" . "PHI Latin Corpus")
    ("tlg" . "TLG Texts")
    ("ddp" . "Duke Documentary Papyri")
    ("ins" . "Classical Inscriptions")
    ("chr" . "Christian Inscriptions")
    ("misc" . "Miscellaneous PHI Texts")
    ("cop" . "PHI Coptic Texts")
    ("bib" . "TLG Bibliography"))
  "Alist of the corpora that are supported by Diogenes.
The form is (ABBREV . FULL-NAME")

(defconst diogenes--corpora-abbrevs
  (mapcar #'car diogenes--corpora)
  "The abbreviations of the corpora supported by Diogenes.")

(defconst diogenes--corpora-names
  (mapcar #'cdr diogenes--corpora)
  "The names of the corpora supported by Diogenes.")

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
  "Search for a phrase in the Duke Documentary Papyri.
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
  "Try to parse a greek word and look it up."
  (interactive (list (read-from-minibuffer "Parse greek word: "
					   (thing-at-point 'word t))))
  (diogenes--parse-and-lookup (diogenes--greek-ensure-beta word)
			      "greek"))

;;;###autoload
(defun diogenes-parse-and-lookup-latin (word)
  "Try to parse a latin word and look it up."
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

;;;###autoload
(defun diogenes-show-all-lemmata-greek (query)
  "Show all Greek lemmata and forms that match QUERY."
  (interactive "sShow all lemmata and their form matching: ")
  (diogenes--show-all-lemmata (diogenes--greek-ensure-beta query) "greek"))

;;;###autoload
(defun diogenes-show-all-lemmata-latin (query)
  "Show all Latin lemmata and forms that match QUERY."
  (interactive "sShow all lemmata and their form matching: ")
  (diogenes--show-all-lemmata query "latin"))


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
(defun diogenes-strip-diacritics (start end)
  "Remove all diacritics in the active region."
  (interactive "r")
  (when (region-active-p)
    (let ((stripped (diogenes--strip-diacritics (buffer-substring start end))))
      (delete-region start end)
      (insert stripped))))

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
  (when (zerop ad)
    (error "There is no year 0!"))
  (let ((year (+ ad (if (< ad 0) 780 779))))
    (message "Ol. %d/%d" (/ year 4)
	     (1+ (mod year 4)))))


;;; DISPATCHER
(transient-define-prefix diogenes-morphology-greek ()
  "Dispatcher for the Diogenes' Greek morphology tool collection."
  ["Greek Morphology Tools"
   ("a" "Show all possible analyses matching query" diogenes-parse-greek)
   ("f" "Show all attested forms of lemma" diogenes-show-all-forms-greek)
   ("l" "Show all lemmata and their forms matching query"
    diogenes-show-all-lemmata-greek)])

(transient-define-prefix diogenes-morphology-latin ()
  "Dispatcher for the Diogenes' Latin morphology tool collection."
  ["Latin Morphology Tools"
   ("a" "Show all possible analyses matching query" diogenes-parse-latin)
   ("f" "Show all attested forms of lemma" diogenes-show-all-forms-latin)
   ("l" "Show all lemmata and their forms matching query"
    diogenes-show-all-lemmata-latin)])

;;;###autoload (autoload 'diogenes "diogenes" nil t)
(transient-define-prefix diogenes ()
  "Study Greek and Latin Texts with Peter Heslin's Diogenes.
This is the main dispatcher function that starts the transient
user interface."
  [["SEARCH"
    ("sg" "Search the Greek TLG" diogenes-search-tlg)
    ("sl" "Search the Latin PHI" diogenes-search-phi)
    ("sd" "Search the Duke Documentary Papyri" diogenes-search-ddp)
    ("si" "Search the Classical Inscriptions" diogenes-search-ins)
    ("sc" "Search the Christian Inscriptions" diogenes-search-chr)
    ("sm" "Search the Miscellaneous PHI Texts" diogenes-search-misc)]
   ["BROWSE"
    ("bg" "Browse the Greek TLG" diogenes-browse-tlg)
    ("bl" "Browse the Latin PHI" diogenes-browse-phi)
    ("bd" "Browse the Duke Documentary Papyri" diogenes-browse-ddp)
    ("bi" "Browse the Classical Inscriptions" diogenes-browse-ins)
    ("bc" "Browse the Christian Inscriptions" diogenes-browse-chr)
    ("bm" "Browse the Miscellaneous PHI Texts" diogenes-browse-misc)]]
  [["MORPHOLOGY & DICTIONARY LOOKUP"
    ("lg" "Look up Greek word (LSJ)" diogenes-lookup-greek)
    ("ll" "Look up Latin word (Lewis & Short)" diogenes-lookup-latin)
    ("pg" "Try to parse and look up a Greek" diogenes-parse-and-lookup-greek)
    ("pl" "Try to parse and look up a Latin" diogenes-parse-and-lookup-latin)
    ("mg" "Greek morphology tools" diogenes-morphology-greek)
    ("ml" "Latin morphology tools" diogenes-morphology-latin)]
   ["DUMP AN ENTIRE WORK AS PLAIN TEXT"
    ("dg" "Dump from the Greek TLG" diogenes-dump-tlg)
    ("dl" "Dump from the Latin PHI" diogenes-dump-phi)
    ("dd" "Dump from the Duke Documentary Papyri" diogenes-dump-ddp)
    ("di" "Dump from the Classical Inscriptions" diogenes-dump-ins)
    ("dc" "Dump from the Christian Inscriptions" diogenes-dump-chr)
    ("dm" "Dump from the Miscellaneous PHI Texts" diogenes-dump-misc)]])

(provide 'diogenes)

;;; diogenes.el ends here
