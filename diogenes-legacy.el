;;; diogenes-legacy.el --- Lisp utilities for diogenes.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Michael Neidhart
;;
;; Author: Michael Neidhart <mayhoth@gmail.com>
;; Keywords: classics, tools, philology, humanities

;;; Commentary:

;; Some legacy post-processing functions

;;; Code:


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

(provide 'diogenes-legacy)

;;; diogenes-legacy.el ends here
