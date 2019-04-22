;;; star-repo.el --- Manage a Star repo  -*- lexical-binding: t; -*-

;; use flymake to report errors

(defvar-local star-build-repo "../Star/Build/"
  "Where the repository for your project lives.")

(defcustom star-repo-name ".repo"
  "Name of the star repository"
  :type 'directory
  :group 'star)

(defvar-local star--flymake-proc nil)

(defun enable-star-flymake ()
  (interactive)
  (add-hook 'flymake-diagnostic-functions 'star-flymake nil t)
  (setq-local star-build-repo
	      (file-name-as-directory
	       (expand-file-name star-repo-name
				 (star-find-project-root
				  (file-name-directory (buffer-file-name))
				  star-repo-name))))
  (flymake-mode t))

(defun star-package ()
  (save-excursion
    (goto-char 0)
    (skip-chars-forward " \n\t")
    (let ((start (point)))
      (search-forward "{")
      (forward-char -1)
      (buffer-substring-no-properties start (point)))))

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
      (setq star--flymake-proc
	    (star-compile source
			  star-build-repo
			  (star-package)
			  (file-name-directory (buffer-file-name source))
			  report-fn))
      (process-send-region star--flymake-proc (point-min) (point-max))
      (process-send-eof star--flymake-proc)
      )
    )
  )

(defvar star-compiler "/Users/fgm/Projects/star/src/BootCompiler/sbc"
  "Exec path to the star compiler")

(defun star-compile (source repo pkg dir report-fn)
  (let* ((compile-buffer (generate-new-buffer "*star-compiler-output*")))
    (star-debug "starting star compile %s" `(,star-compiler "--stdin" "-r" ,repo "-w" ,dir "--" ,pkg))
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

(defconst star-loc-regexp
  "\\(Error\\|Warning\\) [0-9]+ - \\(.*?\\)\\[\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)]")

(defun star-parse-errors (source buffer)
  (with-current-buffer buffer
    (star-debug "report from compiling: %s" buffer)
    (let* ((errRe (concat "^" star-loc-regexp "\n\\(.*\\)$")))
      (progn
	(goto-char (point-min))
	(cl-loop
	 while (search-forward-regexp errRe nil t)
	 for line = (string-to-number (match-string 3))
	 for col = (1- (string-to-number (match-string 4)))
	 for len = (string-to-number (match-string 5))
	 for beg = (star-line-to-pos source line col)
	 for end = (+ beg len)
	 for msg = (match-string 6)
	 collect (flymake-make-diagnostic source beg end :error msg)
	 )))))

(defun star-line-to-pos (buffer line col)
  (save-excursion
    (with-current-buffer buffer
      (save-restriction
	(widen)
	(goto-char (point-min))
	(forward-line (- line (line-number-at-pos)))
	(move-to-column col)
	(point)))))

(defun star-find-project-root (fn sentinel)
  (let ((dr (file-name-as-directory fn))
	(home (file-name-as-directory (expand-file-name "~"))))
    (catch 'find-root
      (while (not (equal dr home))
	(let ((tgt (expand-file-name sentinel dr)))
	  (if (file-exists-p tgt)
	      (throw 'find-root dr)
	    (setq dr (file-name-directory (directory-file-name dr))))))
      nil)))

(provide 'star-repo)
