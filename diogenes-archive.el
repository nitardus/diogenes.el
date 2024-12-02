(defun diogenes--linear-search (dict-file comp-fn key-fn word &optional start stop)
  "A linear search for finding entries in the lexicographical files.
Returns a list of all entries where comp-fn returned nil."
  (let ((start (or start 0))
	(stop  (let ((size (file-attribute-size (file-attributes dict-file))))
		 (if (and stop (< stop size))
		    stop size))))
    (cl-remove-if-not (lambda (str)
			(let ((key (car (funcall key-fn str))))
			  (when key (funcall comp-fn word key))))
		      (split-string (with-temp-buffer
				      (insert-file-contents-literally dict-file
								      nil
								      start
								      stop)
				      (buffer-string))
				    "\n"))))

(defun diogenes--analyses-linear-index-fn (word)
  "Function that transformes a word to become a valid key in the linear index"
  (and (string-match "\\([!*']\\)?.*?\\([A-Za-z]\\)" word)
       (concat (match-string 1 word)
	       (match-string 2 word))))

(let ((cache (make-hash-table :test 'equal)))
  (defun diogenes--get-analyses-linear-index (file)
    "Read an diogenes analyses file and return an index where each letter starts and ends."
    (or (gethash file cache)
	(setf
	 (gethash file cache)
	 (with-temp-buffer
	   (insert-file-contents-literally file)
	   (goto-char (point-min))
	   (let ((last-key (diogenes--analyses-linear-index-fn
			    (buffer-substring (point-min)
					      (1- (re-search-forward "\t" nil t)))))
		 (table (make-hash-table :test 'equal)))
	     (push (list 0) (gethash last-key table))
	     (cl-loop for newline = (re-search-forward "\n" nil t)
		      for tab = (re-search-forward "\t" nil t)
		      while (and newline tab)
		      for word = (buffer-substring newline (1- tab))
		      for key = (diogenes--analyses-linear-index-fn word)
		      unless key do (print word)
		      unless (equal last-key key)
		      do (progn (setcdr (car (gethash last-key table))
					(1- newline))
				(push (list newline) (gethash key table))
				(setf last-key key))
		      finally do (setcdr (car (gethash key table))
					 (file-attribute-size
					  (file-attributes file))))
	     table))))))

(defun diogenes--equal-letters-only (a b)
  (equal (downcase (diogenes--ascii-alpha-only a))
	 (downcase (diogenes--ascii-alpha-only b))))

(defun diogenes--prefix-letters-only (a b)
  (string-prefix-p (downcase (diogenes--ascii-alpha-only a))
		   (downcase (diogenes--ascii-alpha-only b))))

(defun diogenes--parse-word-letters-only (word lang comp-fn)
  "Do a linear search in the morphological database and return all the entries that match
word and comp-fn as a list."
  (let* ((analyses-file (file-name-concat (diogenes--perseus-path)
					  (concat lang "-analyses.txt")))
	 (index (diogenes--get-analyses-linear-index analyses-file))
	 (key (and (diogenes--ascii-alpha-only word)))
	 (indices (cl-loop for k being the hash-keys of index
			   when (string-prefix-p (diogenes--ascii-alpha-only k)
						 key)
			   append (gethash k index))))
    (cl-loop for (start . stop) in indices
	     append (diogenes--linear-search analyses-file
					     comp-fn
					     #'diogenes--tab-key-fn
					     word
					     start
					     stop))))


;;; Convert between legacy encodings with Diogenes::Base
(defun diogenes--utf8-to-beta-script (str)
  "Return perl script that converts an utf-8 string to greek beta code."
  (diogenes--perl-script
   "use utf8;"
   "use Diogenes::UnicodeInput;"
   (format "print Diogenes::UnicodeInput->unicode_greek_to_beta('%s')"
	   str)))

(defun diogenes--beta-to-utf8-script (str)
  "Return perl script that converts greek beta code to utf-8.
Adapted from Perseus.pm, ll. 62--80."
  (diogenes--perl-script
   "use Diogenes::Base;"
   (format "$_ = '%s';" str)
   "tr/a-z/A-Z/;"			;
   "Diogenes::Base::beta_encoding_to_external({encoding => 'UTF-8'}, \\$_ );"
   "s/([\\x80-\\xff])\\_/$1&#x304;/g; # combining macron"
   "s/_/&nbsp;&#x304;/g;"
   "s/([\\x80-\\xff])\\^/$1&#x306;/g; # combining breve"
   "s/\\^/&nbsp;&#x306;/g;"
   "print"))

(defun diogenes--transcode-string (script str)
  "Call perl to transcode string using a diogenes script."
  (with-temp-buffer
    (call-process diogenes-perl-executable
		  nil '(t nil) nil
		  "-e" (funcall script str)
		  (diogenes--include-server)
		  (diogenes--include-cpan))
    (buffer-string)))

(defun diogenes--transcode-with-perl (transcode-script str region-active minibuffer-prompt)
  "Utility function for diogenes-utf8-to-beta and diogenes-beta-to-utf8"
  (if str (diogenes--transcode-string transcode-script str)
    (let ((transformed (diogenes--transcode-string transcode-script
				(if region-active (buffer-substring-no-properties (mark) (point))
				  (read-from-minibuffer minibuffer-prompt nil nil nil nil
							(thing-at-point 'word t))))))
      (unless (string-match "[[:alpha:]]" transformed)
	(error "Conversion failed! Did the string contain non-greek characters?"))
      (cond (region-active (delete-region (point) (mark))
			   (insert transformed))
	    (t (message "#%s#" transformed))))))

;;;###autoload
(defun diogenes-utf8-to-beta-with-perl (str)
  "Call perl to convert greek beta code to utf-8. 
If a region is active, convert the contents of the region in place; 
otherwise, prompt the user for input."
  (interactive "i")
  (diogenes--transcode-with-perl #'diogenes--utf8-to-beta-script str (use-region-p)
				 "Convert to Greek Beta Code: "))

;;;###autoload
(defun diogenes-beta-to-utf8-with-perl (str)
  "Call perl to convert greek unicode to beta code.
If a region is active, convert the contents of the region in place; 
otherwise, prompt the user for input."
  (interactive "i")
  (diogenes--transcode-with-perl #'diogenes--beta-to-utf8-script str (use-region-p)
				 "Convert from Greek Beta Code: "))

;; ;;;###autoload
;; (defun diogenes-lookup-greek (query &optional encoding)
;;   "Look up a word in the LSJ dictionary"
;;   (interactive "i")
;;   (setf query (or query
;; 		  (read-string "Look up greek word: "
;; 			       (thing-at-point 'word))))
;;   (diogenes--server-perseus-action "lookup" query "grk" encoding))

;; ;;;###autoload
;; (defun diogenes-parse-greek (query &optional encoding)
;;   "Parse a word and look it up in the LSJ dictionary"
;;   (interactive "i")
;;   (setf query (or query
;; 		  (read-string "Parse and look up greek word: "
;; 			       (thing-at-point 'word))))
;;   (diogenes--server-perseus-action "parse" query "grk" encoding))

;; ;;;###autoload
;; (defun diogenes-lookup-latin (query &optional encoding)
;;   "Look up a word in the Lewis & Short dictionary"
;;   (interactive "i")
;;   (setf query (or query
;; 		  (read-string "Look up latin word: "
;; 			       (thing-at-point 'word))))
;;   (diogenes--server-perseus-action "lookup" query "lat" encoding))

;; ;;;###autoload
;; (defun diogenes-parse-latin (query &optional encoding)
;;   "Parse a word and look it up in the LSJ dictionary"
;;   (interactive "i")
;;   (setf query (or query
;; 		  (read-string "Parse and look up word: "
;; 			       (thing-at-point 'word))))
;;   (diogenes--server-perseus-action "parse" query "lat" encoding))

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
			       :command (list
					 (concat (directory-file-name (diogenes--path))
						 "/server/diogenes-server.pl"))))
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
