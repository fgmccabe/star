;;; 
;;; Star Emacs mode
;;; Copyright (C) 2019 and beyond F.G. McCabe

(require 'cl)
(require 'font-lock)

(defvar star-mode-hook nil)

;; Customization parameters

(defgroup star nil
  "Major mode for editing and running Star under Emacs"
  :group 'languages)

(defcustom star-block-indent 2
  "* Amount by which to indent blocks of code in Star mode"
  :type 'integer
  :group 'star)

(defcustom star-paren-indent 1
  "* Amount by which to indent after a left paren in Star mode"
  :type 'integer
  :group 'star)

(defcustom star-brace-indent 2
  "* Amount by which to indent after a left brace in Star mode"
  :type 'integer
  :group 'star)

(defcustom star-bracket-indent 5
  "* Amount by which to indent after a left bracket in Star mode"
  :type 'integer
  :group 'star)

(defcustom star-arrow-indent 4
  "* Amount by which to indent after an arrow in Star mode"
  :type 'integer
  :group 'star)

(defcustom star-query-indent 2
  "* Amount by which to indent after an query in Star mode"
  :type 'integer
  :group 'star)

(defcustom comment-column 40
  "* The column where -- comments are placed"
  :type 'integer
  :group 'star)

;;; Initialise the syntax table

(defconst star-mode-syntax-table
  (let ((table (make-syntax-table)))
       ;; ' is a quoted identifier delim, looks like string
       (modify-syntax-entry ?' "\"" table)
       ;; " is an actual string delimiter
       (modify-syntax-entry ?\" "\"" table)
       ;; \n ends line comments
       (modify-syntax-entry ?\n ">" table)
       ;; - is an operator, -- is a comment starter
       (modify-syntax-entry ?- ". 12" table)
       table))


(defvar star-debugging nil
  "Non-nil if should log messages to *star-debug*")

;;; Initialise the abbrev table
(defvar star-mode-abbrev-table nil
  "Abbrev table used while in Star mode.")
(define-abbrev-table 'star-mode-abbrev-table ())

;;; Initialise the key map
(defvar star-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'indent-for-tab-command)
    (define-key map "C-M-q" 'star-indent-sexp)
    (define-key map "C-c C-c" 'comment-region)
    (define-key map "C-c C-d" 'stardebug-buffer)
    map)
  "Keymap for Star major mode")

(defun star-self-insert-and-indent-command (n)
  "Self insert and indent appropriately"
  (interactive "*P")
  (self-insert-command (prefix-numeric-value n))
  (indent-for-tab-command))

(defvar star-indent-cache nil
  "Incremental parse state cache")

;;; Provide `star-mode' user callable function
(define-derived-mode star-mode prog-mode "Star code"
  "Major mode for editing Star programs"
  (setq font-lock-defaults star-mode-font-lock-defaults)
  

  (use-local-map star-mode-map)



  (setq local-abbrev-table star-mode-abbrev-table)
  (set-syntax-table star-mode-syntax-table)

  (setq comment-start "-- ")

  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)

  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "^-- \\|[^:]-- ")

  ;; Local variables (indentation)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'star-indent-line)

  ;; very important that case-fold-search is nil
  ;; since star! is a case-sensitive language
  (setq case-fold-search nil)

  ;; star-indent-cache holds the parse state 
  ;; at particular points in the buffer.
  ;; It is a sorted list (largest points first)
  ;; of elements (POINT . PARSE-STATE)
  ;; PARSE-STATE are cells (STATE . STACK)
  (make-local-variable 'star-indent-cache)
  (setq star-indent-cache nil)

  ;; After a buffer change, we need
  ;; to ensure that the cache is consistent.
  (make-local-variable 'before-change-functions)
  (add-to-list 'before-change-functions 'star-before-change-function)

  ;; Initialise font-lock support

  (star-init-font-lock)
  (run-hooks 'star-mode-hook))

(defun star-before-change-function (from to &rest rest)
  ;; The buffer has changed, we need to
  ;; remove any parse states that have been saved
  ;; past point 'from' in the buffer.
  (while (and star-indent-cache
	      (>= (caar star-indent-cache) from))
    (setq star-indent-cache (cdr star-indent-cache))))

;;; Indentation and syntax
(defsubst star-skip-block-comment ()
  (forward-comment 1))

(defsubst star-skip-line-comment ()
  (search-forward "\n"))

(defsubst star-skip-string ()
  (goto-char (or (scan-sexps (point) 1) (buffer-end 1))))

(defsubst star-skip-symbol ()
  (goto-char (or (scan-sexps (point) 1) (buffer-end 1))))

(defsubst star-skip-char ()
  (cond ((looking-at "`\\+[0-9a-fA-F]+;")
	 (goto-char (match-end 0)))
	((looking-at "`\\\\")
	 (forward-char 2))
	(t (forward-char 2))))

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

(defun star-line-get-pos-after (pos what)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at what)
	(match-end 0)
      nil)))

(defun star-one-of (l)
  (regexp-opt l t))

(defvar star-close-par "[])}]"
  "Star close parentheses")

(defvar star-line-comment "-- "
  "Star line comment")

(defvar star-body-comment "/\\*"
  "Star body comment")

(defvar star-comment (star-one-of star-line-comment star-body-comment)
  "Star comment")

(defvar star-comment-bol (concat "^" star-comment)
  "Star comment at beginning of line")

(defun star-up-level (pos)
  (or (condition-case nil (scan-lists pos -1 1) (error nil)) 
      0))

;;; Parse tables
(defconst star-operators
  ;; Prec Text  Regex  Push Pop Hanging IndentOption      Delta
  '((5000 "{"   "{"    t    nil  nil    nil               star-brace-indent)
    (5000 "["   "\\["  t    nil  t	nil		  star-bracket-indent)
    (5000 "("   "("    t    nil  t	nil		  star-paren-indent)
    (5000 ")"   ")"    nil  same nil	nil		  0)
    (5000 "]"   "\\]"  nil  same nil	nil		  0)
    (5000 "}"   "}"    nil  same nil	nil		  0)
    (1200 "=>"  "=>"   t    t    nil	nil		  star-arrow-indent)
    (900  ":="  ":="   t    t    nil    nil		  star-arrow-indent)
    (820  ".."  "\\.\\." t  t    nil	nil		  0)
    (1460 "::=" "::="  t    t    t	nil		  (* star-arrow-indent 2))
    (1200 "->"  "->"   t    t    nil	nil		  star-arrow-indent)
    (1199 "=>"  "=>"   t    t    t	nil		  star-arrow-indent)
    (1199 "<="  "<="   t    t    nil	nil		  star-arrow-indent)
    (1199 "<~"  "<~"   t    t    nil	nil		  star-arrow-indent)
    (1199 "~>"  "~>"   t    t    nil	nil		  star-arrow-indent)
    (1250 "|"   "|"    t    t    nil	nil		  0)
    (1060 "||"  "||"   t    t    nil	nil		  0)
    (1010  "::"  "::"   t    t    nil	nil		  (* star-arrow-indent 2))
    (1000 ","   ","    t    t    nil	nil		  0)
    (1000 ";"   ";"    t    t    nil	nil		  0)
    (900  ".="  "\\.=" t    t    nil    nil		  0)
    ;; "Terminating dot must be followed by whitespace"
    (1900 ". "  "\\. " t    t    nil    nil               0)
    (1900 ".\t"  "\\.\t" t  t    nil	nil		  0)
    (1900 ".\n"  "\\.\n" t  t    nil	nil		  0)
    (1040 "?"   "\\?"  t    t    nil	nil		  star-query-indent)
    (750  "private"  
          "private\\b" t    t    nil    nil		  0)
    (750  "public"
          "public\\b" t    t    nil    nil		  0)
    (750  "import"  
          "import\\b" t    t    nil    nil		  0)
    (750  "action" 
	  "action\\b"  t    t    nil    nil		  0)
    (750  "sync" 
	  "sync\\b"  t    t    nil    nil		  0)
    )
  "Star operators and precedences")

;;; Speed up table lookups by encoding
;;; information in operator symbols.
(defun star-setup-operators-hash ()
  (let ((l star-operators))
    (while l 
      (let* ((o (car l))
	     (precedence (first o))
	     (text (second o))
	     (regex (third o))
	     (push (fourth o))
	     (pop  (fifth o))
	     (hanging (sixth o))
	     (option (seventh o))
	     (delta (eighth o))
	     (symbol (intern text)))
	(put symbol 'precedence precedence)
	(put symbol 'text text)
	(put symbol 'regex regex)
	(put symbol 'push push)
	(put symbol 'pop (if (eq pop 'same) nil pop))
	(put symbol 'pop-until-same (eq pop 'same))
	(put symbol 'hanging hanging)
	(put symbol 'delta delta)
	(put symbol 'length (length text)))
      (setq l (cdr l)))))
(star-setup-operators-hash)

;;; Regular expression matching important star operators
(defconst star-operators-regex
  (apply 'star-one-of
	 (mapcar 'caddr star-operators))
  "Regular expression matching important star operators")

(defconst star-escaped-string-regex "\\\\['\"]"
  "Regular expression matching the start of an escape")

(defconst star-next-token-regex
  (star-one-of star-operators-regex 
	     star-escaped-string-regex
	     "\""
	     "\'"
	     star-body-comment
	     star-line-comment))

;; The PARSE-STATE is a stack with at least one element.
;; Each element is a list with format (PRECEDENCE OP INDENT)
;; PREC: operator precedence
;; OP: symbol corresponsing to that operator
;; INDENT: indent calculated so far.
(defsubst star-init-parse-state ()
  (list 
   (list 9999 'none 0 nil)))

;; Accessor functions for a PARSE-STATE ((PREC OP INDENT) . STACK)
(defsubst star-parse-state-indent (parse-state)
  (third (car parse-state)))

(defsubst star-parse-state-op (parse-state)
  (second (car parse-state)))

(defsubst star-parse-state-in-comment (parse-state)
  (fourth (car parse-state)))

(defun star-parse-until (pos)
  ;; Find the most recent parse state in the cache 
  ;; that is <= pos
  (let ((parse-state (star-init-parse-state)) ; The parse-state just before POS
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
    (cond 
     ;; Have we already parsed up till here?
     ((= parse-pos pos)		
      parse-state)
     ;; Nope
     (t 
      ;; if there is an error parsing (eg. due to end-of-buffer)
      ;; just return 0
      (condition-case nil
	  (let ((new-parse-state (star-parse parse-pos pos parse-state)))
	    ;; If we parsed into a comment
	    ;; don't bother saving the parse state.
	    (if (star-parse-state-in-comment new-parse-state)
		new-parse-state
	      (progn
		;; Insert the new-parse-state into the indent-cache
		;; Cache is sorted, largest first.
		;; cache = (reverse after) <> [new-parse-state,parse-state,..before]	
		(setq star-indent-cache
		      (cons (cons parse-pos parse-state) 
			    before))
		(setq star-indent-cache
		      (cons (cons pos new-parse-state)
			    star-indent-cache))
		(while after
		  (setq star-indent-cache (cons (car after) star-indent-cache)
			after (cdr after)))
		new-parse-state)))
	(t ;; Some error occurred
	 parse-state)))
     )))

(defsubst star-calculate-brace-indent (pos)
  (star-parse-state-indent (star-parse-until pos)))

;;; Parse from POS to TO given initial PARSE-STATE
;;; Return final PARSE-STATE at TO.
(defun star-parse (pos to parse-state)
  (let* ((case-fold-search nil)
	 (state (car parse-state))
	 (stack (cdr parse-state))
	 (tos-prec   (first  state))
	 (tos-op     (second state))
	 (tos-indent (third  state))
	 (tos-in-comment (fourth state)))
    (save-excursion
      (goto-char pos)
      ;; We assume that the parsing does not
      ;; resume from within a (block) comment.
      ;; To implement that we would need
      ;; to check tos-in-comment and scan for
      ;; end-of-comment (*/) to escape it first.
      (progn 
	(while (< (point) to)
	  (cond 
	   ;; An important Star! operator
	   ((looking-at star-operators-regex)

	    (let* ((symbol (intern (match-string 0)))
		   (symbol-prec (get symbol 'precedence)))

	      ;; Check to see if we should pop any operators off the stack
	      (if (get symbol 'pop)
		  ;; Yes, pop off any lower precedence operators
		  (while (<= tos-prec symbol-prec)
		    (setq state (car stack)
			  stack (cdr stack)
			  tos-prec   (first state)
			  tos-op     (second state)
			  tos-indent (third state))))
	    
	      (if (get symbol 'pop-until-same)
		  ;; Yes, pop of all operators until
		  ;; we meet an operator with the same
		  ;; precedence (for brackets)
		  (progn
		    (while (and (/= tos-prec symbol-prec) (cdr stack))
		      (setq state (car stack)
			    stack (cdr stack)
			    tos-prec   (first state)
			    tos-op     (second state)
			    tos-indent (third state)))
		    (setq state (car stack)
			  stack (cdr stack)
			  tos-prec   (first state)
			  tos-op     (second state)
			  tos-indent (third state))))

	      ;; Push the symbol onto the stack, if allowed
	      (if (get symbol 'push)
		  (progn
		    (setq 
		     ;; Save the old state
		     state (list tos-prec 
				 tos-op 
				 tos-indent)
		     ;; Push it onto the stack
		     stack (cons state stack) 
		     ;; New top-of-stack (indentation carries on
		     ;; from before)
		     tos-prec   symbol-prec
		     tos-op     symbol)))
	    
	      ;; Advance the pointer 
	      (forward-char (get symbol 'length))

	      ;; Adjust the indentation for hanging
	      (if (and (get symbol 'hanging)
		       (not (looking-at "[ \t]*\\(--[ \t]?\\)?$")))
		  ;; Hanging
		  (progn 
		    (skip-chars-forward " \t")
		    (setq tos-indent 
			  (+ tos-indent
			     (- (current-column)
				(max (star-indentation-level (point))
				     (third (car stack)))))))

		;; Not Hanging
		(setq tos-indent (+ tos-indent 
				    (eval (get symbol 'delta)))))
	      ))
	 
	   ;; Skip escaped characters
	   ((looking-at "`\\\\.") (forward-char 3))

	   ;; Skip syntax
	   ((looking-at star-line-comment)
	    (star-skip-line-comment))
	   ((looking-at star-body-comment)
	    (let ((co-col (current-column)))
	      (star-skip-block-comment)
	      (if (>= (point) to)
		  (setq tos-indent (1+ co-col)
			tos-in-comment t))))
	   ((looking-at star-escaped-string-regex)
	    (forward-char 2))
	   ((looking-at "\"")
	    (star-skip-string))
	   ((looking-at "\'")
	    (star-skip-symbol))
	   ((looking-at "`")
	    (star-skip-char))
	   (t 
	    ;; It might be better to forward char first and then scan
	    ;; for the next token to eliminate any possibility of
	    ;; an un-handled token.
	    (or (and (search-forward-regexp star-next-token-regex to t)
		     (goto-char (match-beginning 0)))
		(forward-char)
		)))
	  (skip-chars-forward " \t\n"))

	;; Save the state for future runs
	(setq state (list tos-prec 
			  tos-op 
			  tos-indent
			  tos-in-comment))
	(star-debug "stack: %s %s\n" state stack)
	(cons state stack)))))

(defun star-vertical-bar-adjust (pos bar)
  "Returns the number of columns occupied by the | and following spaces"
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at (concat bar "[ \t]*[^ \t\n\r]"))
	(progn
	  (forward-char)
	  (1+ (skip-chars-forward " \t")))
      1)))

(defun star-calculate-indent (pos)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")
    
    (cond
     ;; Keep comments at beginning of line at the beginning
     ((looking-at star-comment-bol) 0)

     ;; Otherwise indent to standard indent position
     ((looking-at star-comment)
      (star-calculate-brace-indent pos))

     ;; If it's a close brace then we can short-cut (a bit)
     ((looking-at star-close-par)
      (star-calculate-outer-indent (point)))

     ((looking-at "\\.}")
      (star-calculate-outer-indent (1+ (point))))

     ;; If it's a | we need to parse past it to get the
     ;; real indentation level 
     ;; (this method would work fine for close braces as well)
     ((looking-at "[|?]")
      (- (star-calculate-brace-indent 
	  (star-line-get-pos-after pos "[|?]"))
	 (star-vertical-bar-adjust pos "[|?]")))
     
     ;; Otherwise standard indent position
     (t 
      (star-calculate-brace-indent pos)))))

(defun star-goto-first-non-whitespace-maybe ()
  (let ((dest (save-excursion
		(beginning-of-line)
		(skip-chars-forward " \t")
		(point))))
    (if (< (point) dest)
	(goto-char dest))))

(defun star-debug (msg &rest args)
  "Print a debug message to the *star-debug* buffer"
  (if star-debugging
      (save-excursion
	(set-buffer (get-buffer-create "*star-debug*"))
	(insert (apply 'format msg args)))))
  
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
	  ;; (star-readjust-comment bol)
	  ))))
  (star-goto-first-non-whitespace-maybe))

;;; Readjust a -- comment on the same line
;;; (not used for now)
(defun star-readjust-comment (pos)
  "readjust a line comment if there is one on the current line"
  (save-excursion
    (let
	((bol (progn (goto-char pos)(beginning-of-line)(point)))
	 (eol (progn (goto-char pos)(end-of-line)(point))))
      (goto-char bol)
      (cond ((search-forward-regexp comment-start-skip eol t)
	     (indent-for-comment))))))

(defun star-indent-sexp ()
  (interactive)
  (save-excursion
    (let (;(start  (point))
	  (stop   (condition-case nil
		      (save-excursion (forward-sexp 1) (point))
		    (error (point)))))
      (while (and (< (point) stop)
		  (progn (star-indent-line) t)
		  (eq (forward-line) 0)))
      (star-indent-line))))

;;; Font-lock support

(defvar star-font-lock-keyword-regexp
  (concat "\\<"
	  (star-one-of
	   '(
	     "import"		; package
	     "private"		; non-exported element of package
	     "public"		; exported element of package
	     
	     "boolean"		; type
	     "void"			; type
	     "float"		; type 
	     "integer"		; type 
	     
	     "thread"		; type
	     
	     "true"			; standard enumeration symbol
	     "false"		; standard enumeration symbol
	     
	     "this"			; this object
	     
	     "string"		; type
	     "sync"			; control
	     "spawn"		; control
	     "onerror"		; control
	     "in"			; control
	     "case"			; control
	     "valof"		; control
	     
	     "raise"		; control
	     "error"		; standard constructor
	     )) "\\>")
  "Regular expression matching the keywords to highlight in Star mode")

;;; I think that there is too much highlighting
;;; perhaps just highlight arrows => --> -> :- :-- ?

(defvar star-font-lock-symbol-regexp
  (star-one-of '(
		 "::="
		 "=>"
		 "->"
		 "<=>"
		 "{\\."
		 "\\.}"
		 "\\.\\."
		 ":="
		 "\\.="
		 ">="
		 "=="
		 "=<"
		 "="
		 "<\\~"
		 "\\*>"
		 "::="
		 "::"
		 ":"
		 "%%"
		 "~~"
		 "@="
		 "@>"
		 "@@"
		 "@"
		 "#"
		 "\\^"
		 "\\^\\^"
		 "\\\\\\+"
		 "\\\\="
		 ",\\.\\."
		 "!\\."
		 "\\."
		 "!"
		 "+"
		 "-")
	       "\\)")
  "Regular expression matching the symbols to highlight in Star mode")

(defvar star-font-lock-function-regexp
  "^[ \t]*\\(\\sw+\\)([][0-9_a-zA-Z?,.:`'\\ ]*)[ \t]*\\([=-]+>\\|:-\\)"
  "Regular expression matching the function declarations to highlight in Star mode")

(defvar star-font-lock-include-regexp
  "import[ \t]+"
  "Regular expression matching the compiler import package statement")

(defvar star-font-lock-comment-regexp-bol
  "^\\(--[ \t].*$\\)")

(defvar star-font-lock-comment-regexp
  "[^:]\\(--[ \t].*$\\)")

;; Match a line comment, not inside a string.
(defun star-match-line-comment (limit)
  (let ((from (save-excursion (beginning-of-line) (point))))
    (if (search-forward-regexp star-font-lock-comment-regexp limit t)
	(let ((state (parse-partial-sexp from (match-beginning 1))))
	  (if state
	      (if (nth 3 state)
		  (star-match-line-comment limit)
		t)
	    t)))))

(defconst star-dot-space (intern ". "))
(defconst star-dot-newline (intern ".\n"))
(defconst star-dot-tab (intern ".\t"))

(defun star-match-function (limit)
  (if (search-forward-regexp "^[ \t]*\\(\\sw+\\)[ \t]*" limit t)
      (let* ((s (save-excursion 
		  (save-match-data 
		    (star-parse-until (progn (beginning-of-line) (point))))))
	     (op (star-parse-state-op s)))
	(cond
	 ((and (eq op '\{) (cdr s)
	       (not (eq (star-parse-state-op (cdr s)) 'action)))
	  t)
	 ((or (eq op star-dot-space) 
	      (eq op star-dot-newline)
	      (eq op star-dot-tab))
	  t)
	 (t
	  (star-match-function limit))))))

(defconst star-font-lock-keywords-1
  `((,star-font-lock-comment-regexp-bol (1 font-lock-comment-face))
    (,star-font-lock-comment-regexp     (1 font-lock-comment-face))
;;    (star-match-line-comment (1 font-lock-comment-face))
    (,star-font-lock-keyword-regexp     (1 font-lock-keyword-face))
    (,star-font-lock-symbol-regexp      (1 font-lock-reference-face))
;;;    (,star-font-lock-include-regexp     (1 font-lock-doc-string-face))
    (,star-font-lock-function-regexp    (1 font-lock-function-name-face))
    (star-match-function     (1 font-lock-function-name-face t))
    ))

(defvar star-font-lock-keywords star-font-lock-keywords-1
  "Keywords to syntax highlight with font-lock-mode")

(defun star-init-font-lock ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(star-font-lock-keywords nil nil nil nil)))

(provide 'star)
