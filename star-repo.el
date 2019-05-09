;;; star-repo.el --- Manage a Star repo  -*- lexical-binding: t; -*-

(defcustom star-repo-name ".star-repo"
  "Name of the star repository directory"
  :type 'directory
  :group 'star)

(defcustom star-compiler (executable-find "sbc")
  "Exec path to the star compiler"
  :type 'file
  :group 'star)

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

(defcustom star-build-repo
  (file-name-as-directory
   (expand-file-name star-repo-name
		     (star-find-project-root
		      (file-name-directory (buffer-file-name))
		      star-repo-name)))
  "Where the repository for this project lives."
  :group 'star
  :type 'directory)

(defun enable-star-flymake ()
  (interactive)
  (flymake-mode `toggle))

(defun star-package ()
  (save-excursion
    (goto-char 0)
    (skip-chars-forward " \n\t")
    (let ((start (point)))
      (search-forward "{")
      (forward-char -1)
      (buffer-substring-no-properties start (point)))))

(defconst star-errormsg-regexp
  "^\\(Error\\|Warning\\) [0-9]+ - \\(.*?\\)\\[\\([0-9]+\\):\\([0-9]+\\)@\\([0-9]+\\)-\\([0-9]+\\)\\]\n<<\\(.*?\\)>>")

(defun star-parse-errors (source buffer)
  (save-excursion
    (with-current-buffer buffer
      (star-debug "report from compiling: %s" buffer)
      (progn
	(goto-char (point-min))
	(cl-loop
	 while (search-forward-regexp star-errormsg-regexp nil t)
	 for line = (string-to-number (match-string 3))
	 for col = (1- (string-to-number (match-string 4)))
	 for pos = (string-to-number (match-string 5))
	 for len = (string-to-number (match-string 6))
	 for end = (+ pos len)
	 for msg = (match-string 7)
	 collect (flymake-make-diagnostic source pos end :error msg)
	 ))
      )
    )
  )

(defun star-line-to-pos (buffer line col)
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(forward-line (- line (line-number-at-pos)))
	(move-to-column col)
	(point)))))

(defvar-local star--flymake-proc nil)

(defun star-flymake (report-fn &rest _args)
  ;; check for the star compiler
  (unless (executable-find star-compiler)
    (error "Cannot find a suitable star compiler"))

  (when (process-live-p star--flymake-proc)
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
       (star-debug "event %s from %s" event proc)
       (when (eq 'exit (process-status proc))
         (unwind-protect
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

;; Normal compilation
(defun star-compile (source repo pkg dir)
  (let* ((compile-buffer (generate-new-buffer "*star-compiler-output*")))
    (star-debug "starting star compile %s" `(,star-compiler "-r" ,repo "-w" ,dir "--" ,pkg))
    (make-process
     :name "star-compile-on-save" :noquery t :connection-type 'pipe
     :buffer compile-buffer
     :command `(,star-compiler "-r" ,repo "-w" ,dir "--" ,pkg)
     :sentinel
     (lambda (proc event)
       (star-debug "event %s from compiling %s" event pkg)
       (when (eq 'exit (process-status proc))
         (kill-buffer (process-buffer proc))
	 (message "%s compiling %s" (string-trim event) pkg)))
    )
    )
  )

(defun star-compile-maybe ()
  (let ((source (current-buffer)))
    (with-current-buffer source
      (if (equal major-mode 'star-mode)
	  (star-compile source star-build-repo (star-package)
			(file-name-directory (buffer-file-name source))))
      )
    )
  )


(provide 'star-repo)
