;;; diogenes-utils.el --- Some utilities for diogenes.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Michael Neidhart
;;
;; Author: Michael Neidhart <mayhoth@gmail.com>
;; Keywords: classics, tools, philology, humanities

;;; Commentary:

;; This file contains conversions from and to beta code, and some other minor utilities

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'diogenes-lisp-utils)
(require 'ucs-normalize)

;;; Conversion from and to greek beta code
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
    ;; dialytika + acute (in the TLG, the acute comes last. Persesus is un fortunately inconsistent)
    ("i/+".?ΐ)("υ/+".?ΰ)("i+/".?ΐ)("υ+/".?ΰ)
    ;; dialytika + tonos
    ("i/+".?ΐ)("υ/+".?ΰ)("i+/".?ΐ)("υ+/".?ΰ)
    ;; diaeresis + tonos
    ("i/+".?ΐ)("υ/+".?ΰ)("i+/".?ΐ)("υ+/".?ΰ)
    ;; dialytika + grave
    ("i\\+".?ῒ)("υ\\+".?ῢ)("i+\\".?ῒ)("υ+\\".?ῢ)
    ;; dialytika + circumflex
    ("i=+".?ῗ)("υ=+".?ῧ)("i+=".?ῗ)("υ+=".?ῧ)    
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

(defun diogenes--beta-to-utf8 (str)
  "Convert a string from greek beta code to utf-8."
  (replace-regexp-in-string
   "σ\\b\\|σ$" "ς"
   (with-temp-buffer
     (insert str)
     (translate-region (point-min) (point-max)
		       diogenes--beta-to-utf8-table)
     (buffer-string))))

(defun diogenes--perseus-beta-to-utf8 (str)
  "Convert a string from Perseus greek beta code to utf-8.
In addition to diogenes--beta-to-utf8, it handles also macron and
breve signs."
  (when str
    (diogenes--beta-to-utf8
     (diogenes--replace-regexes-in-string str
       ("\\(.\\)_\\([=/\\|+()]+\\)" "\N{COMBINING MACRON}\\1\\2")	
       ("_" "\N{COMBINING MACRON}")
       ("\\(.\\)\\^\\([=/\\|+()]+\\)" "\N{COMBINING BREVE}\\1\\2")
       ("\\^" "\N{COMBINING BREVE}")))))

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
      ;; (when (and latin greek)
      ;; 	(error "\"%s\" contains both Latin and Greek characters!" str))
      (if greek (diogenes--utf8-to-beta str)
	str))))

(defun diogenes--beta-normalize-gravis (str)
  (replace-regexp-in-string "\\\\" "/" str))

(defsubst diogenes--unicode-non-spacing-mark-p (c)
  "Test for the category non-spacing-mark"
  (= 6 (aref unicode-category-table c)))

(defun diogenes--strip-diacritics (str)
  "Remove all diacritics in a string."
  (cl-remove-if #'diogenes--unicode-non-spacing-mark-p
		(string-glyph-decompose str)))

(defsubst diogenes--sort-alphabetically-no-diacritics (a b)
  (string-lessp (diogenes--strip-diacritics a)
		(diogenes--strip-diacritics b)))

;;; Post-processing of raw output
;;;###autoload
(defun diogenes-remove-hyphenation (&optional start end)
  "Delete hyphenation in the active region, or until EOBP."
  (interactive "r")
  (save-excursion
    (save-restriction
      (when (region-active-p)
	(narrow-to-region (progn (goto-char start)
				 (beginning-of-line)
				 (point))
			  (progn (goto-char end)
				 (end-of-line)
				 (point)))
	(goto-char (point-min)))
      (cl-loop for pos-a =
	       (and (re-search-forward "\\([^ <-]+\\)-\\s-*$" (point-max) t)
		    (cons (match-beginning 1)
			  (match-end 1)))
	       while pos-a
	       for pos-b =
	       (when-let ((next-line (and (zerop (forward-line))
					  (thing-at-point 'line))))
		 (when (cl-find-if (lambda (regexp) (string-match regexp next-line))
				   '("^\\S-+\\s-\\{3,\\}\\(\\S-+\\)"
				     "^\\s-\\{3,\\}\\(\\S-+\\)"
				     "^\\(\\S-+\\)"))
		   (cons (+ (point) (match-beginning 1))
			 (+ (point) (match-end 1)))))
	       while pos-b
	       do (let ((word-rest (buffer-substring (car pos-b)
						     (cdr pos-b))))
		    (delete-region (car pos-b) (1+ (cdr pos-b)))
		    (delete-blank-lines)
		    (goto-char (cdr pos-a))
		    (delete-char 1)
		    (insert-and-inherit word-rest))))))

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

;;; Remove line-numbers

;;; Conversion between A.D. and Ol.
(defun diogenes--ol-to-ad (ol)
  (let ((with-0-ad (- (* ol 4) 780)))
    (if (< with-0-ad 0) with-0-ad
      (1+ with-0-ad))))

(defun diogenes--bc-and-ad (year)
  (if (< year 0)
      (format "%s B.C." (- year))
    (format "%s A.D." year)))

(provide 'diogenes-utils)

;;; diogenes-utils.el ends here

