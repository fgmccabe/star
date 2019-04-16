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

(defun run-star ()
  "Run in a shell"
  (interactive)
  (let* ((buffer (comint-check-proc "Star"))
	 (args (append star-flags (list (star-package)))))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'star-shell-mode))
	     (comint-check-proc (current-buffer)))
	 (get-buffer-create (or buffer "*Star*"))
       (current-buffer)))
    (unless buffer
      (star-debug "run %s" (list "Star" nil star-path args))
      (apply 'make-comint-in-buffer "Star" nil
	     star-path () args)
      ;; (let* ((bfr-proc (get-buffer-process buffer)))
      ;; 	(set-process-filter bfr-proc 'star-shell-filter)
      ;; 	(set-process-sentinel bfr-proc 'star-shell-sentinel))
      (star-shell-mode))))

(defun star-shell-initialize ()
  (setq-local comint-process-echoes t)
  (setq-local comint-use-prompt-regexp t))

(defun star-debug-parse (text from)
  (if (string-match "\\(line\\|call\\|ocall\\|tail\\|otail\\|return\\): file:\\([a-z0-9A-Z/.]+\\)(\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\))" text from)
	(let* ((cmd (intern (match-string 1 text)))
	       (file (match-string 2 text))
	       (line (string-to-number (match-string 3 text)))
	       (col (1- (string-to-number (match-string 4 text))))
	       (len (string-to-number (match-string 5 text)))
	       )
	  (list cmd file line col len (match-end 0)))
    nil)
  )


(defun star-shell-filter (text)
  (star-debug "output from shell: %s" text)
  (let ((pos 0)
	(max (length text)))
    (while (< pos max)
      (let ((pp (star-debug-parse text pos)))
	(if pp
	    (-let [(cmd file line col len end) pp]
	      (progn
		(let* ((buffer (find-buffer-visiting file)))
		  (star-debug "file=%s, buffer=%s, line=%s, col=%s, cmd=%s"
			      file buffer line col cmd)
		  (if buffer
		      (star-mark-pos buffer file line col len)))
		(setq pos end)))
	  (setq pos max))))
    )
  )


(defun star-mark-pos (buffer file line col len)
  (save-excursion
    (with-current-buffer buffer
      (save-restriction
	(widen)
	(goto-line line)
	(move-to-column 0)
	(let* ((window (and buffer (or (get-buffer-window buffer)
				       (display-buffer buffer)))))
	  (setq overlay-arrow-string "=>")
	  (or overlay-arrow-position
	      (setq overlay-arrow-position (make-marker)))
	  (set-marker overlay-arrow-position (point) buffer)
	  (set-window-point window overlay-arrow-position)
	  (move-to-column col)
	  (star-overlay-pos buffer line col len))))))

(defvar star-overlay nil)

(defun star-overlay-pos (buffer line col len)
  (save-excursion
    (with-current-buffer buffer
      (save-restriction
	(widen)
	(goto-line line)
	(move-to-column col)
	(let* ((start (point))
	       (end (+ start len)))
	  (progn
	    (if star-overlay
		(move-overlay star-overlay start end buffer)
	      (progn
		(setq star-overlay (make-overlay start end buffer))
		(overlay-put star-overlay 'face 'underline))
	      )))))))

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
      
