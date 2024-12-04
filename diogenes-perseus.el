;;; diogenes-perseus.el --- Mrophological analysis and dictionary lookup for diogenes.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Michael Neidhart
;;
;; Author: Michael Neidhart <mayhoth@gmail.com>
;; Keywords: classics, tools, philology, humanities

;;; Commentary:

;; This file contains functions that can can use the lexica and morphological analyses that come with Diogenes

;;; Code:

;;;; --------------------------------------------------------------------
;;;; LOW LEVEL INTERFACE
;;;; --------------------------------------------------------------------

;;; "Readline"
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

;;; Binary search
;; Sort functions
;; ASCII
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

;; Key function
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

;; The actual search function
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



;;; Parse whole files and load them into memory
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
		      (buffer-substring begin (1- newline)))
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

;;; Get file indices
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

;;;; --------------------------------------------------------------------
;;;; PERSEUS PARSING
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

(defun diogenes--parse-word (word lang)
  "Search the ananlyses file of lang for word using a binary search.
Returns the nearest hit to the query."
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
  (let* ((filter (or filter #'string-equal))
	 (results (cl-loop for k being the hash-keys of
			   (diogenes--get-all-analyses lang)
			   using (hash-values v)
			   when (funcall filter query k)
			   collect (cons k v))))
    (when results
      (mapcar (lambda (x) (cons (car x)
			   (diogenes--format-parse-result (cdr x) "greek")))
	      results))))

(defun diogenes--format-parse-result (encoded-str lang)
  (cl-loop with str = (decode-coding-string encoded-str 'utf-8)
	   for entry in (split-string (cadr (diogenes--split-once "\t" str))
				      "[{}]" t "\\s-")
	   for (lemma-str translation analysis) = (split-string entry "\t")
	   for (lemma-nr lemma-cat lemma) = (split-string lemma-str)
	   collect
	   (list (cond ((string= lang "greek")
			(diogenes--perseus-beta-to-utf8 lemma))
		       (t lemma))
		 lemma-nr
		 translation
		 analysis)))

(defun diogenes--insert-parse-result (entries)
  (insert ))

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
	   collect (cons (cond ((string= lang "greek")

				(diogenes--perseus-beta-to-utf8 form)
				(t form)))
			 analysis)))

(defvar diogenes-analysis-mode-map
  (let ((map (nconc (make-sparse-keymap) text-mode-map)))
    map)
  "Basic mode map for the Diogenes Analysis Mode.")

(define-derived-mode diogenes-analysis-mode text-mode "Diogenes Analysis"
  "Display analysis of search term.")


;;;; --------------------------------------------------------------------
;;;; PERSEUS DICTIONARY LOOKUP
;;;; --------------------------------------------------------------------

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

(provide 'diogenes-perseus)

;;; diogenes-perseus.el ends here

