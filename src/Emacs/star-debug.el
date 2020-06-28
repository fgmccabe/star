;;; -*- lexical-binding: t; -*-
;;; Star debugging Emacs mode
;;; Copyright (C) 2019 and beyond F.G. McCabe

;;; Loosely based on gud.el, and others

(require 'comint)

(defvar star-dbg-last-frame)
(defvar star-dbg-thread-id)
(defvar star-dbg-delete-prompt-marker)
(defvar star-dbg-filter-accumulator "")
(defvar star-dbg-arrow-extent nil)

(defcustom star-dbg-key-prefix "\C-c"
  "Prefix of all star-dbg commands valid in Star buffers."
  :type 'string
  :group 'star-dbg)

(make-face 'star-dbg-arrow-face)
(or (face-differs-from-default-p 'star-dbg-arrow-face)
   ;; Usually has a better default value than highlight does
   (copy-face 'isearch 'star-dbg-arrow-face))

(defvar star-dbg-prompt-pattern "^\\[[0-9]*\\]>>$"
  "A regexp to recognize the prompt for input from the Star debugger.") 

(defvar star-dbg-mode-map
  (let ((map (make-sparse-keymap "Star Debug keymap")))
    (progn
      (set-keymap-parent map comint-mode-map)
      (define-key map "\C-l" 'star-dbg-refresh)
      (define-key map "\C-c" 'star-dbg-control-c-subjob)
      (define-key map " " 'star-dbg-break)
      (define-key map "S-v" 'star-dbg-showvar)
      (global-set-key (concat star-dbg-key-prefix " ") 'star-dbg-break)
      map))
  "Keymap for star-dbg-mode.")

(defvar star-dbg-options '("-h" "2000")
  "List of command-line options to give to star engine for debugging programs"
)

;; Define a command to be sent to the debugger

(defmacro star-dbg-cmd (func str key &optional doc)
  "Define FUNC to be a command sending STR and bound to KEY, with
optional doc string DOC."

  (list 'progn
	(list 'defun func '(arg)
	      (or doc "")
	      '(interactive "p")
	      (list 'star-dbg-call str 'arg))
	(if key
	    (list 'define-key
		  'star-dbg-mode-map
		  key
		  (list 'quote func)))
	(if key
	    (list 'global-set-key
		  (list 'concat 'star-dbg-key-prefix key)
		  (list 'quote func)))))

;; Commands understood by the debugger
(star-dbg-cmd star-dbg-next "n" "n" "Step over source line with display")

(star-dbg-cmd star-dbg-step "s" "s" "Step into")

(star-dbg-cmd star-dbg-quit "q" "q" "Terminate debugged process")

(star-dbg-cmd star-dbg-trace "t" "t" "Go into trace mode")

(star-dbg-cmd star-dbg-cont "c" "c" "Go into continue mode")

(star-dbg-cmd star-dbg-vars "v" "v" "Show all variables")

(star-dbg-cmd star-dbg-show "x" "x" "Show current call")

(star-dbg-cmd star-dbg-show-0 "0" "0" "Show program name")

(star-dbg-cmd star-dbg-show-1 "1" "1" "Show 1st argument")
(star-dbg-cmd star-dbg-show-2 "2" "2" "Show 2nd argument")
(star-dbg-cmd star-dbg-show-3 "3" "3" "Show 3rd argument")
(star-dbg-cmd star-dbg-show-4 "4" "4" "Show 4th argument")
(star-dbg-cmd star-dbg-show-5 "5" "5" "Show 5th argument")
(star-dbg-cmd star-dbg-show-6 "6" "6" "Show 6th argument")
(star-dbg-cmd star-dbg-show-7 "7" "7" "Show 7th argument")
(star-dbg-cmd star-dbg-show-8 "8" "8" "Show 8th argument")
(star-dbg-cmd star-dbg-show-9 "9" "9" "Show 9th argument")

(defun star-dbg-showvar (arg)
  "Ask the debugger to show a single variable"
  (interactive "sShow variable ")
  (star-dbg-call (concat "v " arg)))


(defvar star-dbg-display-mode nil
  "Minor mode for gdb frame display")
(or (assq 'star-dbg-display-mode minor-mode-alist)
    (setq minor-mode-alist
	  (purecopy
	   (append minor-mode-alist
		   '((star-dbg-display-mode " Frame"))))))

(defun star-dbg-display-mode (&optional arg)
  "Toggle Frame display mode
With arg, turn display mode on if and only if arg is positive.
In the display minor mode, source file are displayed in another
window for repective \\[star-dbg-display-frame] commands."
  (interactive "P")
  (setq star-dbg-display-mode (if (null arg)
			     (not star-dbg-display-mode)
			   (> (prefix-numeric-value arg) 0))))


(defun star-dbg-mode ()
  "Major mode for interacting with an inferior go debugging process.
The following commands are available:

\\{star-dbg-mode-map}

\\[star-dbg-display-frame] displays in the other window
the last line referred to in the gdb buffer. See also
\\[star-dbg-display-mode].

\\[star-dbg-step] and \\[star-dbg-next]
call star-dbg to step into or over and then update the other window
with the current file and position.

If you are in a source file, you may select a point to break
at, by doing \\[star-dbg-break].

Commands:
Many commands are inherited from comint mode. 
Additionally we have:

\\[star-dbg-display-frame] display frames file in other window
\\[star-dbg-step] advance one line in program

C-x SPACE sets break point at current line."
  (interactive)
  (comint-mode)
  (use-local-map star-dbg-mode-map)
  (make-local-variable 'star-dbg-last-frame)
  (make-local-variable 'star-dbg-thread-id)
  (make-local-variable 'star-dbg-delete-prompt-marker)
  (make-local-variable 'star-dbg-display-mode)
  (make-local-variable' star-dbg-filter-accumulator)
  (setq star-dbg-last-frame nil
        star-dbg-delete-prompt-marker nil
        star-dbg-filter-accumulator ""
	star-dbg-display-mode t
        major-mode 'star-dbg-mode
        mode-name "Star Debug"
        comint-prompt-regexp star-dbg-prompt-pattern)
  (setq star-dbg-arrow-extent nil)
  (run-hooks 'star-dbg-mode-hook))

(defvar current-star-dbg-buffer nil)

;;;###autoload
(defvar star-dbg-command-name "/usr/local/bin/star"
  "Pathname for executing star in debug mode")

(defun tokenize (arg)
  "Split a string into a list of arguments, separated by spaces"
  (let ((output nil))
    (progn
      (while (string-match 
	      " *\\([^ ]+\\)" arg)
	(setq output (cons (substring arg (match-beginning 1)
				      (match-end 1))
			   output)
	      arg (substring arg (match-end 0)))
	)
      (nreverse output)
      )
    )
  )


;;;###autoload
(defun star-dbg (path pkg &optional args)
  "Run star-dbg on PKG in buffer *star-dbg-PKG*.
If PATH is present then add path to the class path"
  (interactive "FRun star-dbg on package:
sOptional arguments: ")
  (setq path (file-truename (expand-file-name path)))
  (let* ((dbg-buffer-name (concat "star-dbg-" pkg)))
    (switch-to-buffer )
    (or (bolp) (newline))
    (star-debug "... debugging package %s" pkg)
    (star-debug "Current directory is %s" path)
    (let ((bfr-proc
	   (get-buffer-process 
	    (eval (append 
		   (list 'make-comint
			 dbg-buffer-name
			 star-dbg-command-name
			 nil
			 "-dg"
			 "-r" star-repo-dir
			 "-h" "2000"
			 pkg)
		   args)
		  ))))
	  (set-process-filter bfr-proc 'star-dbg-filter)
	  (set-process-sentinel bfr-proc 'star-dbg-sentinel)
    (star-dbg-mode)
    (star-dbg-set-buffer))))

(defun star-dbg-set-buffer ()
  (cond ((eq major-mode 'star-dbg-mode)
	 (setq current-star-dbg-buffer (current-buffer)))))

;; This function is responsible for collecting output from the 
;; debugger process and inserting it into the console buffer.
;; Aside from inserting the text, it notices and deletes
;; each filename-and-line-number;
;; It records the filename and line number, and maybe displays that file.

(defvar star-dbg-defer-flag nil
  "Non-nil means don't process anything from the debugger right now.
It is saved for when this flag is not set.")

(defvar star-dbg-pending-text nil
  "Non-nil means this is text that has been saved for later")

(defun star-prompt-parse (buffer)
  (with-current-buffer buffer
    (if (search-forward-regexp "^.*\\(line\\|call\\|ocall\\|tail\\|otail\\|return\\):.*file:\\([^\(]+\\)(\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\)) *\\(.*\\)$" nil t)
	(let* ((cmd (intern (match-string 1)))
	       (file (match-string 2))
	       (line (string-to-number (match-string 3)))
	       (col (string-to-number (match-string 4)))
	       (len (string-to-number (match-string 5)))
	       (msg (match-string 6))
	       )
	  (list cmd file line col len msg))
    nil)
    )
  )
  

(defun star-dbg-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (let (output process-window)
    (if (buffer-name (process-buffer proc))
	(if star-dbg-defer-flag
	    ;; If we can't process any text now,
	    ;; save it for later.
	    (setq star-dbg-pending-text
		  (concat (or star-dbg-pending-text "") string))

	  ;; If we have to ask a question during the processing,
	  ;; defer any additional text that comes from the debugger
	  ;; during that time.
	  (let ((star-dbg-defer-flag t))
	    ;; Process now any text we previously saved up.
	    (if star-dbg-pending-text
		(setq string (concat star-dbg-pending-text string)
		      star-dbg-pending-text nil))
	    (save-excursion
	      (set-buffer (process-buffer proc))
	      ;; Save the process output, checking for source file markers.
	      (setq output (star-dbg-marker-filter string))
	      ;; Let the comint filter do the actual insertion.
	      ;; That lets us inherit various comint features.
	      (comint-output-filter proc output))

	    ;; Put the arrow on the source line.
	    ;; This must be outside of the save-excursion
	    ;; in case the source file is our current buffer.
	    (if process-window
		(save-selected-window
		  (select-window process-window)
		  (star-dbg-display-frame))
	      ;; We have to be in the proper buffer, (process-buffer proc),
	      ;; but not in a save-excursion, because that would restore point.
	      (let ((old-buf (current-buffer)))
		(set-buffer (process-buffer proc))
		(unwind-protect
		    (star-dbg-display-frame)
		  (set-buffer old-buf)))))

	  ;; If we deferred text that arrived during this processing,
	  ;; handle it now.
	  (if star-dbg-pending-text
	      (star-dbg-filter proc star-dbg-pending-text))))))

;; Process output from the debugger, stripping out the stuff we handle
;; automatically
(defun star-dbg-marker-filter (string)
  (let ((output "") file-found)
    (setq star-dbg-filter-accumulator
	  (if star-dbg-filter-accumulator
	      (concat star-dbg-filter-accumulator string)))
      
    (while 
	(string-match
	 ;; The debugger prompts with 
	 ;; [<thread>] line <file>@<number>
	 "^\\(\\[[^]]+\\]\\) +line +file:\\([^:]*\\):\\([0-9]+$\\)"
	 star-dbg-filter-accumulator)
      (setq star-dbg-thread-id (substring star-dbg-filter-accumulator 
					 (match-beginning 1)
					 (match-end 1)))
      (setq output (concat output 
			   (substring star-dbg-filter-accumulator 0
				      (match-beginning 0))))
      (setq file-found 
	    (substring star-dbg-filter-accumulator 
		       (match-beginning 2)
		       (match-end 2)))
      (setq star-dbg-last-frame
	    (cons file-found
		  (string-to-number
		   (substring star-dbg-filter-accumulator 
			      (match-beginning 3)
			      (match-end 3)))))
;;      (message (concat "thread is " star-dbg-thread-id))
;;      (message (concat "file is " file-found))
;;      (message (concat "line number is " (substring star-dbg-filter-accumulator 
;;						    (match-beginning 3)
;;						    (match-end 3))))
      (setq star-dbg-filter-accumulator 
	    (substring star-dbg-filter-accumulator (match-end 0))))
    (if (string-match "^\\(\\[[^]]+\\]\\) +line" star-dbg-filter-accumulator) 
	(setq output (concat output 
			     (substring star-dbg-filter-accumulator
					0
					(match-beginning 0)))
	      star-dbg-filter-accumulator 
	      (substring star-dbg-filter-accumulator
			 (match-beginning 0)))
      (setq output (concat output star-dbg-filter-accumulator)
	    star-dbg-filter-accumulator ""))
;;    (message (concat "remaining input is " star-dbg-filter-accumulator))
    output)
  )

(defun star-dbg-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; (debug)
	 ;; Stop displaying an arrow in source file.
	 (if overlay-arrow-position
	     (progn
	       (set-marker overlay-arrow-position nil)
	       (setq overlay-arrow-position nil)))
	 ;; Fix the mode line.
	 (assq-delete-all 'star-dbg-display-mode minor-mode-alist)
;;	 (setq modeline-process
;;	       (concat ": go " (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the gdb buffer.
	     (set-buffer obuf))))))

(defun star-dbg-refresh (&optional arg)
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive "P")
  (recenter arg)
  (star-dbg-display-frame))

(defun star-dbg-display-frame (&optional nodisplay noauto)
  "Find, obey and delete the last filename-and-line marker 
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (star-dbg-set-buffer)
  (and star-dbg-last-frame (not nodisplay)
       star-dbg-display-mode
       (star-dbg-display-line (car star-dbg-last-frame) (cdr star-dbg-last-frame))))

(defun star-dbg-display-line (true-file line)
  (let* ((last-nonmenu-event t)	 ; Prevent use of dialog box for questions.
	 (use-dialog-box nil) ; XEmacs
	 (buffer
	  (save-excursion
	    (or (eq (current-buffer) current-star-dbg-buffer)
		(set-buffer current-star-dbg-buffer))
	    (star-dbg-find-file true-file)))
	 (window (and buffer (or (get-buffer-window buffer)
				 (display-buffer buffer))))
	 (pos))
    (if buffer
	(progn
	  (save-excursion
	    (set-buffer buffer)
	    (save-restriction
	      (widen)
	      (goto-line line)
	      (setq pos (point))
	      (setq overlay-arrow-string "=>")
	      (or overlay-arrow-position
		  (setq overlay-arrow-position (make-marker)))
	      (set-marker overlay-arrow-position (point) (current-buffer)))
	    (cond ((or (< pos (point-min)) (> pos (point-max)))
		   (widen)
		   (goto-char pos))))
	  (set-window-point window overlay-arrow-position)))))

(defun star-dbg-find-file (file)
  (save-excursion
    (while (string-match "//+" file)
    (setq file (replace-match "/" t t file)))

    (let ((buf (find-file-noselect file)))
      (set-buffer buf)
      buf)))

(defun star-dbg-call (command &optional arg)
  "Invoke star-dbg COMMAND displaying source in other window."
  (interactive)
  (star-dbg-set-buffer)

  (goto-char (point-max))
  ;; Record info on the last prompt in the buffer and its position.
  ;; This is used in  star-dbg-maybe-delete-prompt
  ;; to prevent multiple prompts from accumulating.
  (save-excursion
    (goto-char (process-mark (get-buffer-process current-star-dbg-buffer)))
    (let ((pt (point)))
      ;(beginning-of-line)
      (forward-line 0)
      (setq star-dbg-delete-prompt-marker
	    (if (= (point) pt)
		nil
	      (list (point-marker) (- pt (point))
		    (buffer-substring (point) pt)))))
    (star-dbg-maybe-delete-prompt))
  (process-send-string (get-buffer-process current-star-dbg-buffer)
		       (concat command "\n")))

(defun star-dbg-maybe-delete-prompt ()
  (if star-dbg-delete-prompt-marker
      ;; Get the string that we used as the prompt before.
      (let ((prompt (nth 2 star-dbg-delete-prompt-marker))
	    (length (nth 1 star-dbg-delete-prompt-marker)))
	;; Position after it.
	(goto-char (+ (car star-dbg-delete-prompt-marker) length))
	;; Delete any duplicates of it which follow right after.
	(while (and (<= (+ (point) length) (point-max))
		    (string= prompt
			     (buffer-substring (point) (+ (point) length))))
	  (delete-region (point) (+ (point) length)))
	;; If that didn't take us to where output is arriving,
	;; we have encountered something other than a prompt,
	;; so stop trying to delete any more prompts.
	(if (not (= (point)
		    (process-mark (get-buffer-process current-star-dbg-buffer))))
	    (progn
	      (set-marker (car star-dbg-delete-prompt-marker) nil)
	      (setq star-dbg-delete-prompt-marker nil))))))

(defun star-dbg-break (temp)
  "Set breakpoint at this source line."
  (interactive "P")
  (let* ((line (save-restriction
		 (widen)
		 (beginning-of-line)
		 (1+ (count-lines 1 (point)))))
	 (cmd (concat "b \"" (buffer-file-name) "\":" (int-to-string line))))
    (star-dbg-call cmd)
    )
  )

(defun star-dbg-clear ()
  "Clear breakpoint at this source line."
  (interactive "P")
  (let* ((line (save-restriction
		 (widen)
		 (beginning-of-line)
		 (1+ (count-lines 1 (point)))))
	 (cmd (concat "B \"" (buffer-file-name) "\":" (int-to-string line))))
    (star-dbg-call cmd)
    )
  )

(fset 'star-dbg-control-c-subjob 'comint-interrupt-subjob)

;(defun star-dbg-control-c-subjob ()
;  "Send a Control-C to the subprocess."
;  (interactive)
;  (process-send-string (get-buffer-process (current-buffer))
;		       "\C-c"))

(provide 'star-dbg)
