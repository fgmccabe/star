;;; -*- lexical-binding: t; -*-
;;; Star shell
;;; Copyright (C) 2019 and beyond F.G. McCabe

(require 'comint)
(require 'star)

;; Customization point -- where is the executable for star
(defcustom star-path "/usr/local/bin/star"
  "Path name for star executable"
  :type 'file
  :group 'star)

;; Mode hook for Star shell
(defvar star-shell-mode-hook nil)

(defvar star-flags `("-g" "-dFC" "-r" "~/Projects/star/src/Star/Build/")
  "standard flags to pass to star executable")

(defvar star-prompt-regexp "\\[\\([0-9]+\\)\\]>>"
  "Regexp that matches prompt from star executable")

(defvar star-debug-run-mode-map
  (let ((map (make-sparse-keymap)))
    (progn
      (set-keymap-parent map comint-mode-map)
      (define-key map "\t" 'completion-at-point)
      map)))

(defun run-star (arg)
  "Run in a shell"
  (interactive "sRun with argument: ")
  (let* ((buffer (comint-check-proc "Star"))
	 (args (append star-flags (list (star-package) arg))))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'star-shell-mode))
	     (comint-check-proc (current-buffer)))
	 (get-buffer-create (or buffer "*Star*"))
       (current-buffer)) t)
    (unless buffer
      (star-debug "run %s" (list "Star" nil star-path args))
      (apply 'make-comint-in-buffer "Star" nil
	     star-path () args)
      (star-shell-mode))))

(defun star-shell-initialize ()
  (setq-local comint-process-echoes t)
  (setq-local comint-use-prompt-regexp t))

(defun star-debug-parse (text from)
  (if (string-match "\\(line\\|call\\|ocall\\|tail\\|otail\\|return\\): file:\\([a-z0-9A-Z/.]+\\)(\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\))" text from)
	(let* ((cmd (intern (match-string 1 text)))
	       (file (match-string 2 text))
	       (line (string-to-number (match-string 3 text)))
	       (pos (string-to-number (match-string 5 text)))
	       (len (string-to-number (match-string 6 text)))
	       )
	  (list cmd file line pos len (match-end 0)))
    nil)
  )

(defun star-shell-sentinal (proc msg)
  (star-debug "run-sentinel triggered for %s with %s" proc msg))

(defun star-shell-filter (text)
;;  (star-debug "output from shell: %s" text)
  (let ((pos 0)
	(max (length text)))
    (while (< pos max)
      (let ((pp (star-debug-parse text pos)))
	(if pp
	    (-let [(cmd file line pos len end) pp]
	      (progn
		(let* ((buffer (find-buffer-visiting file)))
		  (star-debug "file=%s, buffer=%s, line=%s, cmd=%s"
			      file buffer line cmd)
		  (if buffer
		      (star-mark-pos buffer file pos len)))
		(setq pos end)))
	  (setq pos max))))
    )
  )

(defvar-local star-overlay nil)

(defun star-mark-pos (buffer file pos len)
  (save-excursion
    (with-current-buffer buffer
      (save-restriction
	(widen)
	(goto-char pos)
	(move-to-column 0)
	(let* ((window (and buffer (or (get-buffer-window buffer)
				       (display-buffer buffer)))))
	  (setq overlay-arrow-string "=>")
	  (or overlay-arrow-position
	      (setq overlay-arrow-position (make-marker)))
	  (set-marker overlay-arrow-position (point) buffer)
	  (set-window-point window overlay-arrow-position)
	  (goto-char pos)
	  (let ((end (+ pos len)))
	    (progn
	      (if star-overlay
		  (move-overlay star-overlay pos end buffer)
		(progn
		  (setq star-overlay (make-overlay pos end buffer))
		  (overlay-put star-overlay 'face 'underline))
		)))
	  )))))

(define-derived-mode star-shell-mode comint-mode "Star"
  "Major mode for running star"
  :after-hook star-shell-mode-hook
  (setq-local comint-prompt-regexp star-prompt-regexp)
  (setq-local comint-prompt-read-only t)
  (setq-local paragraph-start star-prompt-regexp)
  (add-hook 'comint-output-filter-functions 'star-shell-filter nil t)
  (run-mode-hooks 'star-shell-mode-hook)
  )

(add-hook 'star-shell-mode-hook 'star-shell-initialize)
      
