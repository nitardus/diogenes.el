;;; diogenes-legacy.el --- Lisp utilities for diogenes.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Michael Neidhart
;;
;; Author: Michael Neidhart <mayhoth@gmail.com>
;; Keywords: classics, tools, philology, humanities

;;; Commentary:

;; Some legacy post-processing functions

;;; Code:

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

(provide 'diogenes-legacy)

;;; diogenes-legacy.el ends here
