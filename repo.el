;;; star-repo.el --- Manage a Star repo  -*- lexical-binding: t; -*-

(defun home-search (dir tgt)
  (process-dir dir (is-lit tgt)))

(defun mkpath (path fn)
  (concat path "/" fn))

(defun is-member (el lst)
  (mem-test (lambda (x) (equal x el)) lst))

(defun mem-test (tst lst)
  (catch 'mem-test
    (while lst
      (let* ((hd (car lst)))
	(if (funcall tst hd)
	    (throw 'mem-test t)
	  (setq lst (cdr lst))
	  )
	))
    nil
    )
  )

(defun is-lit (str)
  (lambda (x) (equal x str)))

(defun never (nm)
  nil)

(defun xcluder (old nm)
  (lambda (n)
    (or (funcall old n)
	(equal n nm))))

(defun process-dir (dir tgt)
  (catch 'process-dir
    (proc-dir dir (is-lit ".git") 'never tgt)))

(defun find-project-root (fn sentinel)
  (let ((dr (file-name-as-directory fn))
	(home (file-name-as-directory (expand-file-name "~"))))
    (catch 'find-root
      (while (not (equal dr home))
	(let ((tgt (expand-file-name sentinel dr)))
	  (if (file-exists-p tgt)
	      (throw 'find-root dr)
	    (setq dr (file-name-directory (directory-file-name dr))))))
      nil)))

(defun proc-dir (dir stop exclude tgt)
  (let ((stopping nil)
	(files (directory-files-and-attributes dir)))
    (while files
      (let* ((fl (car files))
	     (fn (1st fl))
	     (full-name (mkpath dir fn))
	     (isdir (2nd fl)))
	(cond ((funcall tgt fn)
	       (throw 'process-dir full-name))
	      ((funcall stop fn)
	       (setq stopping t))
	      ((and isdir
		    (not (or (equal fn "..") (equal fn ".")))
		    (not (funcall exclude fn)))
	       (process-dir (proc-dir
			     full-name
			     stop
			     (xcluder exclude
				      (lambda (f)
					(equal (file-name-nondirectory f) fn)))
			     tgt))))
	)
      (setq files (cdr files))
      )
    (unless stopping
      (let ((upper (file-name-directory dir))
	    (this (file-name-nondirectory dir)))
	(unless (equal upper "")
	  (proc-dir upper stop (is-lit this) tgt))
	)
      )
    nil)
)


(mem-test (is-lit "fred") '("alpha" "fred" "gamma"))
(home-search "/Users/fgm/Projects/star/src/Compiler/" "Repo")

(find-project-root "/Users/fgm/Projects/star/src/Compiler/" ".git")
