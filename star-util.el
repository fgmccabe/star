;;; -*- lexical-binding: t; -*-
;;; Utilities used in star emacs support
;;; Copyright (C) 2019 and beyond F.G. McCabe

;;;
;;; Utility functions for star mode
;;;

(defsubst 1st (l)
  "return the first element of a list."
  (nth 0 l))

(defsubst 2nd (l)
  "return the second element of a list."
  (nth 1 l))

(defsubst 3rd (l)
  "return the third element of a list."
  (nth 2 l))

(defsubst 4th (l)
  "return the fourth element of a list."
  (nth 3 l))

(defsubst 5th (l)
  "return the fifth element of a list."
  (nth 4 l))

(defsubst 6th (l)
  "return the sixth element of a list."
  (nth 5 l))

(defsubst 7th (l)
  "return the 7th element of a list."
  (nth 6 l))

(defsubst 8th (l)
  "return the 8th element of a list."
  (nth 7 l))

;; Regular expression utilities

(defun star-one-of (l)
  "Construct optimized regexp from a list of strings (l)."
  (regexp-opt l t))

(defun star-compose-regexps (l)
  (if (cadr l) 
      (concat (car l) "\\|"
	      (star-compose-regexps (cdr l)))
    (car l)))

(defvar star-debugging t
  "Non-nil if should log messages to *star-debug*")

(defun star-debug (msg &rest args)
  "Print a debug message to the *star-debug* buffer"
  (if star-debugging
      (let ((debug-buffer (get-buffer-create "*star-debug*")))
	(with-current-buffer debug-buffer 
	  (goto-char (point-max))
	  (insert (apply 'format (concat msg "\n") args)))))
  )

(defun star-clear-debug ()
  (if star-debugging
      (let ((debug-buffer (get-buffer-create "*star-debug*")))
	(with-current-buffer debug-buffer 
	  (erase-buffer)))))

(defun star-package ()
  (save-excursion
    (goto-char 0)
    (skip-chars-forward " \n\t")
    (let ((start (point)))
      (search-forward "{")
      (forward-char -1)
      (buffer-substring-no-properties start (point)))))

;;; Regular expression matching important star operators
(defconst star-line-comment-regexp
  "\\(--[ \t].*$\\)")

(defconst star-line-comment-regexp-bol
  (concat "^" star-line-comment-regexp))

(defconst star-body-comment-regexp
  "/\\*"
  "Star body comment start")

(provide 'star-util)
