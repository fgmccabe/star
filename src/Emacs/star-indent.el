;;; star-indent.el -*- lexical-binding: t; -*-

(require 'star-util)
(require 'star-ops)


;; star-indent-cache holds the parse state 
;; at particular points in the buffer.
;; It is a sorted list (largest points first)
;; of elements (POINT . PARSE-STATE)
;; PARSE-STATE are cells (STATE . STACK)

(defvar-local star-indent-cache nil
  "Incremental parse state cache.")

(defun star-indent-pos (cache)
  (1st (car cache)))

(defun star-clear-indent-cache (pos)
  "clear indent cache to pos"
;;  (star-debug "clearing indent cache %s to %from" star-indent-cache pos)
  (while (and star-indent-cache
	      (>= (star-indent-pos star-indent-cache) pos))
    (setq star-indent-cache (cdr star-indent-cache)))
;;  (star-debug "indent cache now %s" star-indent-cache)
)  

(defun star-after-change-function (from to &rest rest)
  ;; The buffer has changed, we need to
  ;; remove any parse states that have been saved
  ;; past point 'from' in the buffer.

  (star-clear-indent-cache from)
  )

(defconst star-num-regexp
  "\\(0x[0-9a-fA-F]+\\|0c\\(?:\\\\u[^;]*;\\|.\\)\\|[0-9]+\\(?:[.][0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\)?\\)"
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

(defsubst star-skip-block-comment ()
  (forward-comment 1))

(defsubst star-skip-line-comment ()
  (search-forward "\n"))

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

(defun star-skip ()
  (interactive)
  (goto-char (star-skip-whitespace (current-buffer) (point) (buffer-end 1))))

(defun star-next-tok (limit)
  (let ((done nil)
	(buffer (current-buffer)))
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
	     ((looking-at star-bkt-regexp)
	      (progn
		(let* ((op (match-string 0))
		       (start (point))
		       (end (+ (point) (length op))))
		  (setq done (list 'bracket op start end))
		  )))
	     ((looking-at star-op-regexp)
	      (progn
		(let* ((op (match-string 0))
		       (start (point))
		       (end (+ (point) (length op))))
		  (setq done (list 'operator op start end))
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

;; Indentation table

(defconst star-indentation
  ;; Name Align Hanging Delta
  (let ((l 
	 '((";"      nil     0)
	   ("catch"  nil     0)
	   ("then"   nil     star-query-indent)
	   ("else"   nil     0)
	   (":="     nil     star-arrow-indent)
	   ("::="    nil     star-arrow-indent)
	   ("="      nil     star-arrow-indent)
	   ("=>"     nil     star-arrow-indent)
	   (">>="    nil     star-arrow-indent)
	   ("<~"     nil     star-arrow-indent)
	   ("~>"     nil     star-arrow-indent)
	   ("|:"     nil     star-arrow-indent)
	   ("where"  nil     (* star-arrow-indent 2))
	   ("|"      nil     star-arrow-indent)
	   ("||"     t	     0)
	   ("?"      nil     star-query-indent)
	   (","      nil     0)
	   ("&&"     nil     (* star-pred-indent 2))
	   ("->"     nil     star-arrow-indent)
	   ("=="     nil     star-pred-indent)
	   (".="     nil     star-pred-indent)
	   ("^="     nil     star-pred-indent)
	   ("assert" nil     0)
	   ))
	(i (make-hash-table :test 'equal)))
    (while l
      (let* ((o (car l))
	     (op (1st o))
	     (hanging (2nd o))
	     (delta (3rd o)))
	(puthash op (list delta hanging) i)
	(setq l (cdr l))))
    i)
  "Star operators indents ")

(defun star-operator-delta (op)
  "Get delta for operator"
  (let ((op (gethash op star-indentation)))
    (if op
	(eval (1st op))
      0)))

(defun star-operator-hanging (op)
  "Does this operator hang to left"
  (let ((op (gethash op star-indentation)))
    (if op
	(eval (2nd op))
      nil)))

(defun star-operator-indent (state op)
  (let ((delta (star-operator-delta op)))
    (+ (star-state-indent state) delta)))

(defun star-state-type (state)
  (1st state))

(defun star-state-indent (state)
  (2nd state))

(defun state-priority (state)
  (3rd state))

(defun star-state-matching (state op)
  (equal (4th state) op))

(defun star-create-state (type indent priority match)
  (list type indent priority match))

(defun star-deflt-state ()
  (list (list 'outer 0 3000 nil)))

(defun star-is-at-eol (pos)
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t")
    (cond ((looking-at star-line-comment-regexp)
	   t)
	  ((looking-at "[ \t]*$")
	   t)
	  (t nil))))

(defun star-is-at-bol (pos)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (>= (point) pos)))

(defun star-scan-until (pos)
  (star-clear-debug)
;;  (star-debug "parse until %s" pos)
;;  (star-debug "indent-cache: %s\n" star-indent-cache)

  (let ((parse-state (star-deflt-state)) ; The parse-state just before POS
	(parse-pos   1)			; The position of the above parse-state
	(before      star-indent-cache)   ; All the parse-states before POS
	(after       nil))		; All the parse-states after POS
    (while (and before
		(> (caar before) pos))
      (setq after (cons (car before) after)
	    before (cdr before)))
    ;; Load the parse state
    (if before
	(setq parse-pos (caar before)
	      parse-state (cdar before)
	      before (cdr before)))
    (if
	(= parse-pos pos) ;; Have we already parsed up till here?
	parse-state

      ;; if there is an error parsing (eg. due to end-of-buffer)
      ;; just return 0
      (condition-case nil
	  (let ((new-parse-state (star-term parse-pos pos parse-state)))
;;	    (star-debug "state from parse: %s" new-parse-state)
	    ;; Insert the new-parse-state into the indent-cache
	    ;; Cache is sorted, largest first.
	    ;; cache = (reverse after) <> [new-parse-state,parse-state,..before]

;;	    (star-debug "indent-cache (before): %s\n" star-indent-cache)
		
	    (setq star-indent-cache
		  (cons (cons parse-pos parse-state) 
			before))
	    (setq star-indent-cache
		  (cons (cons pos new-parse-state)
			star-indent-cache))
	    (while after
	      (setq star-indent-cache (cons (car after) star-indent-cache)
		    after (cdr after)))

;;	    (star-debug "indent-cache (after): %s\n" star-indent-cache)
			  
	    new-parse-state)
	(t ;; Some error occurred
	 parse-state)))
    )
  )

;; 
;; state is a tuple consisting of
;; (type indent  priority match-bracket)
;; where type is 'operator or 'bracket
;; stack is stack of same

(defun star-term (pos limit parse-state)
  "An abstracted form of opg parser that is used to compute indentation"
  (save-excursion
    (goto-char limit)
    (skip-chars-forward " \t")
    (if (looking-at star-op-regexp) ;; parse past operator if first on line
	(let* ((op (match-string 0))
	       (end (+ (point) (length op))))
	  (setq limit end)))
    
    (goto-char pos)

    (let ((state (car parse-state))
	  (stack (cdr parse-state)))
      (while (< (point) limit)
;;	(star-debug "state: %s" state)
;;	(star-debug "stack: %s" stack)
	(let ((nxtok (star-next-tok limit)))
	  (if nxtok
	      (let ((tktype (1st nxtok))
		    (op (2nd nxtok))
		    (start (3rd nxtok))
		    (end (4th nxtok)))
;;		(star-debug "token: %s %s %s %s" tktype op start end)
		(cond ((eq tktype 'term)
		       (while (and stack
				   (not (and
					 (eq (star-state-type state) 'bracket)
					 (or (star-state-matching state "}")
					     (star-state-matching state ".}")))))
			 (setq state (car stack)
			       stack (cdr stack))))
		      ((eq tktype 'operator)
		       (cond ((star-is-prefixop op)
			      (if (star-is-at-bol start)
				  (let* ((spec (star-is-prefixop op))
					 (rprior (2nd spec)))
				    (setq stack (cons state stack)
					  state (star-create-state
						 'operator
						 (star-operator-indent state op)
						 rprior nil)
					  ))))
			     ((star-is-infixop op)
			      (if (or (star-is-at-bol start) (star-is-at-eol end))
				  (let* ((spec (star-is-infixop op))
					 (lprior (1st spec))
					 (rprior (3rd spec)))
				    (while (and stack
						(<= (state-priority state) lprior))
				      (setq state (car stack)
					    stack (cdr stack)))
				    (setq stack (cons state stack)
					  state (star-create-state
						 'operator
						 (star-operator-indent state op)
						 rprior nil)
					  ))))
			     ((star-is-postfixop op)
			      (if (star-is-at-eol end)
				  (let* ((spec (star-is-postfixop op))
					 (lprior (1st spec)))
				    (while (and stack
						(<= (state-priority state) lprior))
				      (setq state (car stack)
					    stack (cdr stack)))
				    ))
			      )
			 ))
		      ((eq tktype 'bracket)
		       (if (star-is-left op)
			   (progn
			     (let* ((spec (star-is-left op))
				    (right (1st spec))
				    (inner (2nd spec)))
			       (setq stack (cons state stack)
				     state (star-create-state
					    'bracket (+ (star-state-indent state) 2)
					    inner
					    right))))
			 (progn
			   (while (and stack
				       (not (and
					     (eq (star-state-type state) 'bracket)
					     (star-state-matching state op))))
			     (setq state (car stack)
				   stack (cdr stack)))
			   (if stack
			       (setq state (car stack)
				     stack (cdr stack))))))
		      )
		(goto-char end))
	    )))
      (cons state stack))))

;; Manage indentation
(defconst star-close-par "[])}]"
  "Star close parentheses")

(defun star-calculate-indent (pos)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")

    (cond
     ;; Keep comments at beginning of line at the beginning
     ((looking-at star-line-comment-regexp-bol) 0)

     ;; Otherwise indent to standard indent position
     ((looking-at star-line-comment-regexp)
      (star-calculate-brace-indent pos))

     ;; If it's a close brace then we can short-cut (a bit)
     ((looking-at star-close-par)
      (star-calculate-outer-indent (point)))

     ((looking-at "\\.}")
      (star-calculate-outer-indent (1+ (point))))

     ;; Otherwise standard indent position
     (t 
      (star-calculate-brace-indent pos))
     )
    )
  )

(defun star-parse-state-indent (parse-state)
  (star-state-indent (car parse-state)))

(defun star-calculate-brace-indent (pos)
  (star-parse-state-indent (star-scan-until pos)))

(defun star-calculate-outer-indent (pos)
  (save-excursion
    (condition-case nil
	(progn (goto-char pos)
	       (goto-char (scan-lists pos -1 1))
	       (star-calculate-indent (point)))
      (error 0))))

;;; look for a the first non-whitespace
(defun star-indentation-level (pos)
  "returns the indentation level of the current line"
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-column)))

;;; Readjust a -- comment on the same line
(defun star-readjust-comment (pos)
  "readjust a line comment if there is one on the current line"
  (save-excursion
    (let
	((bol (progn (goto-char pos)(beginning-of-line)(point)))
	 (eol (progn (goto-char pos)(end-of-line)(point))))
      (goto-char bol)
      (cond ((search-forward-regexp comment-start-skip eol t)
	     (indent-for-comment))))))

;;; Hook called when the tab key is pressed
(defun star-indent-line ()
  (save-excursion
    (let* ((bol         (progn (beginning-of-line) (point)))
	   (cur-level   (star-indentation-level bol))
	   (level       (star-calculate-indent bol)))
      (if (= cur-level level)
	  nil
	(progn
	  (delete-horizontal-space)
	  (indent-to level)
	  (star-readjust-comment bol)
	  ))))
  (star-goto-first-non-whitespace-maybe))

(defun star-goto-first-non-whitespace-maybe ()
  (let ((dest (save-excursion
		(beginning-of-line)
		(skip-chars-forward " \t")
		(point))))
    (if (< (point) dest)
	(goto-char dest))))


(defun star-indent ()
  (interactive)
  (save-excursion
    (let ((stop   (condition-case nil
		      (save-excursion (forward-sexp 1) (point))
		    (error (point)))))
      (while (and (< (point) stop)
		  (progn (star-indent-line) t)
		  (eq (forward-line) 0)))
      (star-indent-line))))

(provide 'star-indent)
