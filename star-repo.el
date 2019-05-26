;;; star-repo.el --- Manage a Star repo  -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'star-util)
(require 'star-config)

(defun star-find-project-root (fn sentinel)
  (star-debug "looking for project root starting from %s" fn)
  (let ((dr (file-name-as-directory fn))
	(home (file-name-as-directory (expand-file-name "~"))))
    (catch 'find-root
      (while (not (equal dr home))
	(let ((tgt (expand-file-name sentinel dr)))
	  (if (file-exists-p tgt)
	      (throw 'find-root dr)
	    (setq dr (file-name-directory (directory-file-name dr))))))
      nil)))

(defvar-local star-build-repo
  (file-name-as-directory
   (expand-file-name star-repo-name
		     (star-find-project-root
		      (file-name-directory (buffer-file-name))
		      star-repo-name)))
  "Where the repository for this project lives.")

(defun enable-star-flymake ()
  (interactive)
  (flymake-mode `toggle))

(defconst star-errormsg-regexp
  "^\\(Error\\|Warning\\) [0-9]+ - \\(.*?\\)\\[\\([0-9]+\\):\\([0-9]+\\)@\\([0-9]+\\)-\\([0-9]+\\)\\]\n<<\\(\\(?:.\\|\n\\)*?\\)>>")

(defun star-parse-errors (source buffer)
  (save-excursion
    (with-current-buffer buffer
      (star-debug "report from compiling: %s" buffer)
      (progn
	(goto-char (point-min))
	(cl-loop
	 while (search-forward-regexp star-errormsg-regexp nil t)
	 for pos = (string-to-number (match-string 5))
	 for len = (string-to-number (match-string 6))
	 for end = (+ pos len)
	 for msg = (match-string 7)
	 collect (flymake-make-diagnostic source pos end :error msg)
	 ))
      )
    )
  )

(defvar-local star--flymake-proc nil)

(defun star-flymake (report-fn &rest _args)
  ;; check for the star compiler
  (unless (executable-find star-compiler)
    (error "Cannot find a suitable star compiler"))

  (when (process-live-p star--flymake-proc)
    (kill-buffer (process-buffer star--flymake-proc))
    (kill-process star--flymake-proc))
     
  (let ((source (current-buffer)))
    (save-excursion
      (save-restriction
	(widen)
	;; Reset the `star--flymake-proc' process to a new process
	(setq star--flymake-proc
	      (star-fly-compile source
				star-build-repo
				(star-package)
				(file-name-directory (buffer-file-name source))
				report-fn))
	(process-send-region star--flymake-proc (point-min) (point-max))
	(process-send-eof star--flymake-proc)
	)
      )
    )
  )

(defun star-fly-compile (source repo pkg dir report-fn)
  (let* ((compile-buffer (generate-new-buffer "*star-compiler-output*")))
    (star-debug "starting star compile %s" `(,star-compiler "--stdin" "-r" ,repo "-w" ,dir "--" ,pkg))
    (make-process
     :name "star-compile" :noquery t :connection-type 'pipe
     :buffer compile-buffer
     :command `(,star-compiler "--stdin" "-r" ,repo "-w" ,dir "--" ,pkg)
     :sentinel
     (lambda (proc event)
       (let ((proc-status (process-status proc))
	     (proc-buffer (process-buffer proc)))
	 (star-debug "process status %s from %s" proc-status proc)
	 (when (or (eq 'signal proc-status) (eq 'exit proc-status))
           (unwind-protect
               ;; Only proceed if `proc' is the same as
               ;; `star--flymake-proc', which indicates that
               ;; `proc' is not an obsolete process.
               ;;
               (if (eq proc star--flymake-proc)
                   (with-current-buffer proc-buffer
		     (let ((error-report
			    (star-parse-errors source proc-buffer)))
		       (star-debug "error report from %s is %s" pkg error-report)
		       (funcall report-fn error-report))
		     )
		 )
	     ;; Cleanup the temporary buffer used to hold the compiler's output
	     (kill-buffer proc-buffer)
	     ))))
     )
    )
  )

;; Normal compilation
(defun star-compile (repo pkg dir)
  (let* ((compile-buffer (generate-new-buffer "*star-compile-on-save*")))
    (star-debug "starting star compile %s"
		`(,star-compiler "-r" ,repo "-w" ,dir ,@star-compiler-flags "--" ,pkg))
    (make-process
     :name "star-compile-on-save" :noquery t :connection-type 'pipe
     :buffer compile-buffer
     :command `(,star-compiler "-r" ,repo "-w" ,dir ,@star-compiler-flags "--" ,pkg)
     :sentinel
     (lambda (proc event)
       (let ((proc-status (process-status proc)))
	 (star-debug "status %s from compiling %s" proc-status pkg)
	 (when (or (eq 'exit proc-status) (eq 'signal proc-status))
           (kill-buffer (process-buffer proc))
	   (message "%s compiling %s" (string-trim event) pkg)))
       )
     )
    )
  )

(defun star-compile-maybe ()
  (let ((source (current-buffer)))
    (with-current-buffer source
      (if (equal major-mode 'star-mode)
	  (star-compile star-build-repo (star-package)
			(file-name-directory (buffer-file-name source))))
      )
    )
  )

(provide 'star-repo)
