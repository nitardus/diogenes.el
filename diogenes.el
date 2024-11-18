;;; diogenes.el --- Interface to diogenes -*- lexical-binding: t -*-

;; An interface to Peter Heslin's Diogenes
;; Copyright (C) 2024 Michael Neidhart
;;
;; Author: Michael Neidhart <mayhoth@gmail.com>
;; Keywords: classics, tools, philology, humanities
;;
;; Version: 0.2

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
;; - Search
;;   - constructor:
;;     - pattern_list + min_matches
;;     - reject pattern
;;     - context (num = lines, sent, para)
;;     - bib_info (0, 1, nur TLG);
;;   - select_authors:
;;     - author_nums => { auth => [wk1, wk2] }
;;     - criteria => 'all' || 'any'
;;     - get_tlg_categories => 1 (hash keys => [val1, val2,..])
;;     - nur TLG:
;;       - date(start, end, varia, incerta). Mögliche Werte : date ()
;;       - genre, genre_clx, epithet
;;       - gender ('Femina ')
;;       - location
;; - Indexed (...)
;; - Diogenes maior mode: Texte bewegen, ...
;; - Diogenes minor mode: Wörter parsen, TLG lookup at-point
;; - Perseus
;; - Transformationen: Diogenes::UnicodeInput->unicode_greek_to_beta($query);
;; - Diogenes::Search::simple_latin_to_beta({}, $query);

(require 'cl-lib)

;;;; --------------------------------------------------------------------
;;;; CUSTOMISATION
;;;; --------------------------------------------------------------------



(defgroup diogenes nil
  "Interface to P. Heslin's Diogenes"
  :group 'tools)

(defcustom diogenes-perl-executable "perl"
  "Path to perl executable"
  :type 'string
  :group 'diogenes)

(defcustom diogenes-path ""
  "Path to the Diogenes installation"
  :type 'string
  :group 'diogenes)

(defcustom diogenes--perl-min-version 5.10
  "Minimal required verson of perl"
  :type 'float
  :group 'diogenes)

(defcustom diogenes-browser-show-citations t
  "Whether to show embedded citations in the browser by default."
  :type 'boolean
  :group 'diogenes
  :local t)

(defun diogenes--path (&optional prefix suffix)
  (if diogenes-path
      (concat (or prefix "")
	      diogenes-path
	      (if (string= (substring diogenes-path -1) "/")
		  "server"
		"/server")
	      (or suffix ""))
    (error "diogenes-path is not set! Please set it to the \"server\" directory of your Diogenes installation!")))

;;;; --------------------------------------------------------------------
;;;; PERL INTERFACE
;;;; --------------------------------------------------------------------

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



;;; Perl scripts
(defmacro diogenes--perl-script (&rest lines)
  `(string-join (list (format "use v%.2f;" diogenes--perl-min-version)
		      "use strict;"
		      "use warnings;"
		      ;;diogenes--discard-perl-stderr
		      ,@lines)
		"\n"))

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
   diogenes--discard-perl-stderr
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

;;; Perl runners
(defun diogenes--start-perl (type code &optional filter)
  "Starts and a perl process named diogenes-type.
It is associated with a buffer with the same name, in asterisks."
  (let ((buffer (diogenes--get-fresh-buffer type)))
    (make-process :name    (format "diogenes-%s" type)
		  :buffer  buffer
		  :command (list diogenes-perl-executable
				 "-e" code
				 (diogenes--path "-I"))
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
			   (diogenes--path "-I"))
    (pop-to-buffer buffer)
    (funcall mode)))

(defun diogenes--debug-perl (script)
  (switch-to-buffer (generate-new-buffer "*diogenes-perldebug*"))
  (insert script)
  (cperl-mode))



;;; Low Level Perl Callers
(let ((cache (make-hash-table :test 'equal)))
  (defun diogenes--get-info (script &optional options1 options2)
    (let ((key (list script options1 options2)))
      (or (gethash key cache)
	  (setf (gethash key cache)
		(read (with-temp-buffer
			(call-process diogenes-perl-executable
				      nil '(t nil) nil
				      "-e" (apply script
						  (list options1 options2))
				      (diogenes--path "-I"))
			(buffer-string))))))))

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


;;;; --------------------------------------------------------------------
;;;; USER INTERFACE
;;;; --------------------------------------------------------------------

;;; Selectors
(defun diogenes--select-author-num (options &optional author-regex)
  "Select one author from a diogenes database using a prompt."
  (let ((author-list (diogenes--get-author-list options author-regex)))
    (cadr (assoc (completing-read "Author: " author-list)
		 author-list))))

(defun diogenes--select-author-nums (options &optional author-regex)
  "Select a list of authors from a diogenes database using a prompt.
Returns a list."
  (cl-loop collect (diogenes--select-author-num options author-regex)
	   while (y-or-n-p "Add another author?")))

(defun diogenes--select-work-num (options author)
  "Select a single work from an author in a Diogenes database."
  (let ((works-list (diogenes--get-works-list options
					      author)))
    (cadr (assoc (completing-read "Work: "
				  works-list)
		 works-list))))

(defun diogenes--select-work-nums (options author)
  "Select a list of works from an author in a Diogenes database."
  (let ((works-list (diogenes--get-works-list options author)))
    (cl-loop collect (diogenes--select-author-num options author)
	     while (y-or-n-p "Add another work?"))))

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

;; (defun diogenes--select-tlg-categories ()
;;   (let ((categories (diogenes--get-tlg-categories)))
;;     (completing-read "Narrow down by category: " (plist-get categories category))))


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
;;;; MAJOR MODES
;;;; --------------------------------------------------------------------

;;; Diogenes Browser
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
  "Major mode to browse diogenes databases."
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



;;;; --------------------------------------------------------------------
;;;; TOPLEVEL
;;;; --------------------------------------------------------------------

;;; Search
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
	 (error "Dispatcher for multiple patterns or categories not yet implemented!"))
	((stringp options-or-pattern)
	 (setf options-or-pattern (list :pattern options-or-pattern)))
	((not (plistp options-or-pattern))
	 (error "%s must be either a string (pattern) or a plist!"
		options-or-pattern)))
  (setf options-or-pattern (plist-put options-or-pattern :type type))
  (diogenes--do-search options-or-pattern author-plist))

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
  "Search for a phrase in the Greek TLG database.

Uses the Diogenes Perl Module."
  (interactive "i\ni\np")
  (diogenes--search-database "phi" options-or-pattern author-plist prefix))

;;;###autoload
(defun diogenes-search-ddp (options-or-pattern
			    &optional author-plist prefix)
  "Search for a phrase in the Greek TLG database.

Uses the Diogenes Perl Module."
  (interactive "i\ni\np")
  (diogenes--search-database "ddp" options-or-pattern author-plist prefix))



;;; Dump
(defun diogenes--dump-from-database (type &optional author work)
  "Dump a work from a Diogenes database in its entirety.
Uses the Diogenes Perl module."
  (let* ((author (or author
		     (diogenes--select-author-num `(:type ,type))))
	 (work (or work
		   (diogenes--select-work-num `(:type ,type)
					      author))))
    (diogenes--dump-work `(:type ,type) (list author work))))

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
  "Dump a work from the Latin PHI database in its entirety.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--dump-from-database "ddp" author work))


;;; Browse
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
;;;###autoload
(defun diogenes-browse-tlg (&optional author work)
  "Browse a specific passage in a work from the Greek TLG database.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--browse-database "tlg" author work))

;;;###autoload
(defun diogenes-browse-phi (&optional author work)
  "Browse a work from the Latin PHI database in its entirety.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--browse-database "phi" author work))

;;;###autoload
(defun diogenes-browse-ddp (&optional author work)
  "Browse a work from the Latin PHI database in its entirety.
Uses the Diogenes Perl module."
  (interactive)
  (diogenes--browse-database "ddp" author work))



;;; Communicating with the diogenes server
(defvar diogenes--server nil
  "Running diogenes server process")

(defun diogenes-start-server ()
  (interactive)
  (unless (and diogenes--server
	       (process-live-p diogenes--server))
    (prog1 (setf diogenes--server
		 (make-process :name "diogenes-server"
			       :buffer " diogenes-server-output"
			       :command (list (concat (directory-file-name
						       (concat (diogenes--path)
							       "/diogenes-server.pl"))))))
      (sit-for 0.2))))

(defun diogenes--server-adress ()
  (unless (and diogenes--server
	       (process-live-p diogenes--server))
    (error "Diogenes is not running!"))
  (with-current-buffer " diogenes-server-output"
    (goto-char (point-min))
    (re-search-forward "http://")
    (string-trim (thing-at-point 'line t))))

(defun diogenes--server-perseus-action (action query language
					       &optional encoding)
  (diogenes-start-server)
  (let ((browse-url-browser-function #'eww-browse-url)))
  (browse-url (concat (diogenes--server-adress)
		      (format "/Perseus.cgi?do=%s&lang=%s&q=%s&popup=1"
			      action language query)
		      (when encoding
			(concat "&inp-enc="
				encoding)))))

;;;###autoload
(defun diogenes-lookup-greek (query &optional encoding)
  "Look up a word in the LSJ dictionary"
  (interactive "i")
  (setf query (or query
		  (read-string "Look up greek word: "
			       (thing-at-point 'word))))
  (diogenes--server-perseus-action "lookup" query "grk" encoding))

;;;###autoload
(defun diogenes-parse-greek (query &optional encoding)
  "Parse a word and look it up in the LSJ dictionary"
  (interactive "i")
  (setf query (or query
		  (read-string "Parse and look up greek word: "
			       (thing-at-point 'word))))
  (diogenes--server-perseus-action "parse" query "grk" encoding))

;;;###autoload
(defun diogenes-lookup-latin (query &optional encoding)
  "Look up a word in the Lewis & Short dictionary"
  (interactive "i")
  (setf query (or query
		  (read-string "Look up latin word: "
			       (thing-at-point 'word))))
  (diogenes--server-perseus-action "lookup" query "lat" encoding))

;;;###autoload
(defun diogenes-parse-latin (query &optional encoding)
  "Parse a word and look it up in the LSJ dictionary"
  (interactive "i")
  (setf query (or query
		  (read-string "Parse and look up word: "
			       (thing-at-point 'word))))
  (diogenes--server-perseus-action "parse" query "lat" encoding))


;;;; --------------------------------------------------------------------
;;;; USER UTILITIES
;;;; --------------------------------------------------------------------

;;; Conversion between A.D. and Ol.
(defun diogenes--ol-to-ad (ol)
  (let ((with-0-ad (- (* ol 4) 780)))
    (if (< with-0-ad 0) with-0-ad
      (1+ with-0-ad))))

(defun diogenes--bc-and-ad (year)
  (if (< year 0)
      (format "%s B.C." (- year))
    (format "%s A.D." year)))

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

;;;Legacy post-processing functions
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
(defun diogenes-unhyphen-greek (&optional start end)
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
(defun diogenes-unhyphen (col &optional start end)
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

(provide 'diogenes)
