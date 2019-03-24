;;;
;;; Utility functions for star mode
;;;


;; Some utility functions
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

(defun star-one-of (l)
  "Construct optimized regexp from a list of strings (l)."
  (regexp-opt l t))

(defun star-compose-regexps (l)
  (if (cadr l) 
      (concat (car l) "\\|"
	      (star-compose-regexps (cdr l)))
    (car l)))

(defvar star-debugging nil
  "Non-nil if should log messages to *star-debug*")

(defun star-debug (msg &rest args)
  "Print a debug message to the *star-debug* buffer"
  (if star-debugging
      (save-excursion
	(set-buffer (get-buffer-create "*star-debug*"))
	(goto-char (point-max))
	(insert (apply 'format (concat msg "\n") args)))))

(defun star-debug-reset ()
  "reset debug msg buffer"
  (if star-debugging
      (save-excursion
	(set-buffer (get-buffer-create "*star-debug*"))
	(erase-buffer))
    )
  )
  
(provide 'star-utils)
