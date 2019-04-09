;;; star-flymake.el --- A star Flymake backend  -*- lexical-binding: t; -*-
(defvar-local star--flymake-proc nil)

(defvar star-compiler "/Users/fgm/Projects/star/src/BootCompiler/sbc"
  "Exec path to the star compiler")

(defvar star-build-repo "../Star/Build/"
  "Name of the build repository for the compiler")

(defun star-package ()
  (save-excursion
    (goto-char 0)
    (skip-chars-forward " \n\t")
    (let ((start (point)))
      (search-forward "{")
      (forward-char -1)
      (buffer-substring-no-properties start (point)))))

(defun star-compile (source repo pkg dir report-fn)
  (let* ((compile-buffer (generate-new-buffer "*star-compiler-output*")))
    (star-debug "starting star compile on %s into buffer %s" pkg compile-buffer)
    (make-process
     :name "star-compile" :noquery t :connection-type 'pipe
     :buffer compile-buffer
     :command `(,star-compiler "--stdin" "-r" ,repo "-w" ,dir "--" ,pkg)
     :sentinel
     (lambda (proc event)
       (star-debug "event %s from %s" event proc)
       (when (eq 'exit (process-status proc))
         (unwind-protect
	     (with-current-buffer compile-buffer 
	       (star-debug "report: %s" (buffer-string)))
	     (star-debug "start parsing result of compilation of %s" pkg)
             ;; Only proceed if `proc' is the same as
             ;; `star--flymake-proc', which indicates that
             ;; `proc' is not an obsolete process.
             ;;
             (if (with-current-buffer source (eq proc star--flymake-proc))
                 (with-current-buffer (process-buffer proc)
		   (let ((error-report
			  (star-parse-errors source
					     (process-buffer proc))))
		     (star-debug "error report from %s is %s" pkg error-report)
		     (funcall report-fn error-report)))
               (flymake-log :warning "Canceling obsolete check %s"
                            proc))
           ;; Cleanup the temporary buffer used to hold the compiler's output
           (kill-buffer (process-buffer proc))
	   ))))
    )
  )

(defun star-parse-errors (source buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (cl-loop
     while (search-forward-regexp
            "^\\(Error\\|Warning\\) \\(?:[0-9]+\\) - \\(.*?\\)\\[\\([0-9]+\\):\\([0-9]+\\)]\\(?:[0-9]+:[0-9]+\\)\n\\(.*\\)$"
            nil t)
     for beg = (string-to-number (match-string 3))
     for len = (string-to-number (match-string 4))
     for end = (+ beg len)
     for msg = (match-string 5)
     collect (flymake-make-diagnostic source
                                      beg
                                      end
                                      :error
                                      msg)
     )))

(defun star-flymake (report-fn &rest _args)
  ;; check for the star compiler
  (unless (executable-find star-compiler)
    (error "Cannot find a suitable star compiler"))

  (when (process-live-p star--flymake-proc)
    (kill-process star--flymake-proc))
     
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      ;; Reset the `star--flymake-proc' process to a new process
      (setq star-pkg (star-package))
      (setq star--flymake-proc
	    (star-compile source
			  star-build-repo
			  star-pkg
			  (file-name-directory (buffer-file-name source))
			  report-fn))
      (process-send-region star--flymake-proc (point-min) (point-max))
      (process-send-eof star--flymake-proc)
      )
    )
  )
     
(defun star-setup-flymake-backend ()
  (add-hook 'flymake-diagnostic-functions 'star-flymake nil t))
     
(add-hook 'star-mode-hook 'star-setup-flymake-backend)

(provide 'star-flymake)
