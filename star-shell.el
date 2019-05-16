;;; -*- lexical-binding: t; -*-
;;; Star shell
;;; Copyright (C) 2019 and beyond F.G. McCabe

(require 'comint)
(require 'star-util)
(require 'star-config)
;;(require 'star-repo)

;; Mode hook for Star shell
(defvar star-shell-mode-hook nil)

(defvar star-flags `("-g" "-dFC")
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
	 (args (append star-flags (list "-r" star-build-repo (star-package) arg))))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'star-shell-mode))
	     (comint-check-proc (current-buffer)))
	 (get-buffer-create (or buffer "*Star*"))
       (current-buffer)) t)
    (unless buffer
      (star-debug "run %s" (list "Star" nil star-path args))
      
      (apply 'make-comint-in-buffer "Star" nil star-path () args)
      (star-shell-mode))))

(defun star-remote ()
  "Run in a shell"
  (interactive)
  (let* ((buffer (comint-check-proc "Star")))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'star-shell-mode))
	     (comint-check-proc (current-buffer)))
	 (get-buffer-create (or buffer "*Star*"))
       (current-buffer)) t)
    (unless buffer
      (star-debug "connect to %s" (list "localhost" 10000 ))
      (apply 'make-comint-in-buffer "Star" nil '("localhost" . 10000) ())
      (star-shell-mode))))


(defun star-shell-initialize ()
  (setq-local comint-process-echoes t)
  (setq-local comint-use-prompt-regexp t)
  (star-clear-debug))

(defconst star-debug-regexp 
  "\\(line\\|call\\|ocall\\|tail\\|otail\\|return\\): file:\\([a-z0-9A-Z/.]+\\)(\\([0-9]+\\):\\(?:[0-9]+\\)@\\([0-9]+\\),\\([0-9]+\\))"
  )

(defvar star-overlay nil)

(defun star-debug-parse (text from)
  (if (string-match star-debug-regexp text from)
      (let* ((cmd (intern (match-string 1 text)))
	     (file (match-string 2 text))
	     (pos (string-to-number (match-string 4 text)))
	     (len (string-to-number (match-string 5 text)))
	     )
	(star-debug "command: %s file %s pos: %s - %s" cmd file pos len)
	(list cmd file pos len (match-end 0)))
    nil)
  )

(defun star-shell-sentinel (proc msg)
  (star-debug "run-sentinel triggered for %s with %s" proc msg)
  (let ((status (process-status proc)))
    (when (or (eq status 'exit) (eq status 'closed))
      (if star-overlay
	  (progn
	    (delete-overlay star-overlay)
	    (setq star-overlay nil)
	    (setq overlay-arrow-position nil))))
    ))

(defun star-shell-filter (text)
  (star-debug "output from shell: %s" text)
  (if (> (length text) 0)
      (let ((pos 0)
	    (max (length text)))
	(while (< pos max)
	  (let ((pp (star-debug-parse text pos)))
	    (if pp
		(let ((cmd (1st pp))
		      (file (2nd pp))
		      (start (3rd pp))
		      (len (4th pp))
		      (end (5th pp)))
		  (progn
		    (let* ((buffer (find-buffer-visiting file)))
		      (if buffer
			  (star-mark-pos buffer start len)))
		    (setq pos end)))
	      (setq pos max))))
	)
    )
  )

(defun star-mark-pos (buffer pos len)
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
  (add-hook 'comint-exec-hook
	    (lambda ()
	      (set-process-sentinel (get-buffer-process (current-buffer))
				    'star-shell-sentinel)))
  (run-mode-hooks 'star-shell-mode-hook)
  )

(add-hook 'star-shell-mode-hook 'star-shell-initialize)
