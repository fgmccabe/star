;;; star-repo.el --- Manage a Star repo  -*- lexical-binding: t; -*-

(defun home-search (dir tgt)
  (let* ((this-dir (directory-files-and-attributes dir)))
    (process-dir this-dir dir 'nogit (lambda (fn) (equal tgt fn)))))

(defun mkpath (path fn)
  (concat path "/" fn))

(defun is_member (el lst)
  (member_test (lambda (x) (equal x el)) lst))

(defun member_test (tst lst)
  (let* ((reslt nil))
    (while lst
      (let* ((hd (car lst)))
	(if (funcall tst hd)
	    (progn
	      (setq reslt t)
	      (setq lst nil))
	  (setq lst (cdr lst))
	  )))
    reslt))

(defun isgit (fn)
  (is_member fn '(".git")))

(defun never (nm)
  nil)

(defun xcluder (old nm)
  (lambda (n)
    (or (funcall old n)
	(equal n nm))))

(defun process-dir (files dir exclude tgt)
  (let* ((reslt ()))
    (while files
      (let* ((fl (car files))
	     (fn (1st fl))
	     (full-name (mkpath dir fn))
	     (isdir (2nd fl)))
	(setq files (cdr files))
	(cond ((funcall tgt fn)
	       (setq reslt (cons full-name reslt)))
	      ((and isdir
		    (not (funcall exclude fn))
		    (not (or (equal fn "..") (equal fn "."))))
	       (setq reslt (append
			    (process-dir
			     (directory-files-and-attributes full-name)
			     full-name
			     exclude
			     tgt)
			    reslt)))
	      )
	)
      )
    reslt)
  )

