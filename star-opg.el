;;; star-opg.el -*- lexical-binding: t; -*-

(defconst star-num-regexp
  "\\([0-9]+\\(?:[.][0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\)?\\)"
  "regular expression that matches numeric literals")

(defconst star-id-regexp
  "\\(\\(?:[a-zA-Z_][a-zA-Z_0-9]*\\)\\|'\\(?:.*\\)*'\\)"
  "regular expression that matches identifiers")

(defconst star-string-regexp
  "\\(\\(?:\"\"\"\\(?:.\\|\n\\)*\"\"\"\\)\\|\\(?:\"\\(?:\\\\.\\|[^\"\n]\\)*\"\\)\\)"
  "regular expression that matches basic strings")

(defconst star-term-regexp
  "\\(\\.\\(?:[ \t\n]\\|$\\)\\)"
  "regular expression that matches a statement terminator")


(defun star-skip-whitespace (buffer pos limit)
  (with-current-buffer buffer
    (goto-char pos)
    (let ((done nil))
      (while (and (not done) (< (point) limit))
	(cond ((looking-at star-line-comment-regexp)
	       (star-skip-line-comment))
	      ((looking-at "[ \n\t]")
	       (skip-chars-forward " \n\t"))
	      ((looking-at star-body-comment-regexp)
	       (star-skip-block-comment))
	      (t (setq done t))))
      (point))))

(defun star-string ()
  (interactive)
  (search-forward-regexp star-string-regexp))

(defun star-term ()
  (interactive)
  (search-forward-regexp star-term-regexp))

(defun star-skip ()
  (interactive)
  (goto-char (star-skip-whitespace (current-buffer) (point) (buffer-end 1))))

(defun star-next-tok (buffer pos limit)
  (with-current-buffer buffer
    (goto-char pos)
    (let ((done nil))
      (while (and (not done) (< (point) limit))
	(let ((nextpt (star-skip-whitespace buffer (point) limit)))
	  (if (< nextpt limit)
	      (cond
	       ((looking-at star-term-regexp)
		(progn
		  (let* ((op (match-string 0))
			 (start (point))
			 (end (+ (point) (length op))))
		    (setq done (list 'term ". " start end))
		    )))
	       
	       ((looking-at star-op-regexp)
		(progn
		  (let* ((op (match-string 0))
			 (start (point))
			 (end (+ (point) (length op))))
		    (setq done (list 'operator op start end))
		    )))
	       ((looking-at star-bkt-regexp)
		(progn
		  (let* ((op (match-string 0))
			 (start (point))
			 (end (+ (point) (length op))))
		    (setq done (list 'bracket op start end))
		    )))
	       ((looking-at star-num-regexp)
		(progn
		  (let* ((op (match-string 0))
			 (start (point))
			 (end (+ (point) (length op))))
		    (setq done (list 'prim op start end))
		    )))
	       ((looking-at star-id-regexp)
		(progn
		  (let* ((op (match-string 0))
			 (start (point))
			 (end (+ (point) (length op))))
		    (setq done (list 'prim op start end))
		    )))
	       ((looking-at star-string-regexp)
		(progn
		  (let* ((op (match-string 0))
			 (start (point))
			 (end (+ (point) (length op))))
		    (setq done (list 'prim op start end))
		    )))
	       (t
		(forward-char 1)))))
	)
      done
      )
    )
  )

(defun star-scan ()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (star-clear-debug)
      (let ((pt (point))
	    (limit (point-max))
	    (buffer (current-buffer))
	    (toks ()))
	(while (< pt limit)
	  (let ((tok (star-next-tok buffer pt limit)))
	    (if tok
		(progn
		  (setq toks (cons tok toks))
		  (star-debug "tokens: %s" tok)
		  (setq pt (4th tok)))
	      (setq pt limit)
	      )
	    ))
	(reverse toks)
	)))
  )

(defun star-tok-type (tok)
  (1st tok))

(defun star-tok-name (tok)
  (2nd tok))

(defun star-tok-loc (tok)
  (cddr tok))

(defun star-ast-loc (ast)
  (1st ast))

(defun star-merge-loc (l1 l2)
  (list (1st l1) (2nd l2)))

(defun star-term (toks prior)
  (let* ((lt (star-left toks prior))
	 (left (1st lt))
	 (lprior (2nd lt))
	 (rtoks (3rd lt))
	 (llast (4th lt)))
    (star-term-right rtoks prior lprior left llast)))

(defun star-left (toks prior)
  (let* ((tok (car toks))
	 (id (star-tok-name tok)))
    (cond ((and (star-is-prefixop id)
		(not (star-is-right-bracket-token (star-lookahead toks)))
		(<= (star-prefix-priority id) prior))
	   (let* ((rprior (star-prefix-arg-priority id))
		  (rgt (star-term (cdr toks) rprior))
		  (right (1st rgt))
		  (rtoks (2nd rgt))
		  (rlast (3rd rgt))
		  (rloc (star-ast-loc right)))
	     ((star-unary-ast (star-merge-loc (star-tok-loc tok) rloc)
			 id
			 right)
	      (star-prefix-priority id)
	      rtoks
	      rlast)))
	  (t (let*
		 ((t0 (star-term0 toks))
		  (term (1st t0))
		  (rtoks (2nd t0))
		  (rlast (3rd t0)))
	       (list term 0 rtoks rlast)))
	  )))

(defun star-term00 (toks)
  (let* ((tok (car toks))
	 (tp (star-tok-type tok))
	 (name (star-tok-name tok)))
    (cond ((eq tp 'prim)
	   (list (star-prim-ast tok) (cdr toks) 'id))
	  ((and (eq tp 'bracket) (star-is-left name))
	   (let* ((spec (star-bracket-spec name))
		  (inner (4th spec))
		  (t0 (star-term (cdr toks) inner))
		  (term (1st t0))
		  (rtoks (2nd t0))
		  (nxtok (car rtoks))
		  )
	     (if (equal (star-tok-name nxtok) (2nd spec))
		 (list (star-tuple-ast (merge-loc (star-tok-loc tok)
						  (star-tok-loc nxtok))
				       name term)
		       (cdr rtoks)
		       (star-tok-name nxtok))
	       )))
	  )
    )
  )

(defun star-term0 (toks)
  (-let [(head itoks lst) (star-term00 toks)]
    (star-term-args itoks head lst)))

(defun star-term-args (toks head lst)
  (if (toks)
      (let* ((nxtk (car toks)))
	(if (eq (star-tok-type nxtk) 'bracket)
	    (-let [(arg itoks llst) (star-term00 toks)]
	      (star-term-args
	       itoks 
	       (star-apply (star-merge-loc (star-ast-loc head) (star-ast-loc arg))
			   head arg)
	       llst))
	  (list head toks lst)))
    (list head toks lst)))
				  
(defun star-right (toks prior lprior left last)
  (let* ((tok (car toks))
	 (id (star-tok-name tok))
	 (inf (star-is-infixop id))
	 (post (star-is-postfixop id)))
    ))
