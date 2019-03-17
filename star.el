;;; star.el --- Emacs mode for Star
;;; Copyright (C) 2019 and beyond F.G. McCabe


;;; Commentary:
;; 

(require 'font-lock)
(require 'smie)

;; Mode hook for Star
;;; Code:

(defvar star-mode-hook nil)


;; Customization parameters

(defgroup star nil
  "Major mode for editing and running Star under Emacs"
  :group 'languages)

(defcustom star-block-indent 2
  "* Amount by which to indent blocks of code in Star mode."
  :type 'integer
  :group 'star)

(defcustom star-paren-indent 1
  "* Amount by which to indent after a left paren in Star mode."
  :type 'integer
  :group 'star)

(defcustom star-brace-indent 2
  "* Amount by which to indent after a left brace in Star mode."
  :type 'integer
  :group 'star)

(defcustom star-bracket-indent 5
  "* Amount by which to indent after a left bracket in Star mode."
  :type 'integer
  :group 'star)

(defcustom star-arrow-indent 4
  "* Amount by which to indent after an arrow in Star mode."
  :type 'integer
  :group 'star)

(defcustom star-query-indent 2
  "* Amount by which to indent after an query in Star mode."
  :type 'integer
  :group 'star)

(defcustom comment-column 40
  "* The column where -- comments are placed."
  :type 'integer
  :group 'star)

;; Some utility functions
(defsubst 1st (l)
  "return the first element of a list."
  (nth 0 l))

(defsubst 2nd (l)
  "return the second element of a list."
  (nth 1 l))

(defsubst 3rd (l)
  "return the third element of a list."
  (nth 2 l))

(defsubst 4th (l)
  "return the fourth element of a list."
  (nth 3 l))

(defsubst 5th (l)
  "return the fifth element of a list."
  (nth 4 l))

(defsubst 6th (l)
  "return the sixth element of a list."
  (nth 5 l))

(defsubst 7th (l)
  "return the 7th element of a list."
  (nth 6 l))

(defsubst 8th (l)
  "return the 8th element of a list."
  (nth 7 l))

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
       (modify-syntax-entry ?/ ". 14" table)
       (modify-syntax-entry ?* ". 23" table)
       (modify-syntax-entry ?( "()" table)
       (modify-syntax-entry ?) ")(" table)
       (modify-syntax-entry ?[ "(]" table)
       (modify-syntax-entry ?] ")[" table)
       (modify-syntax-entry ?\{ "(}" table)
       (modify-syntax-entry ?\} "){" table)
       (modify-syntax-entry ?_ "w" table)
       table))

(defvar star-debugging nil
  "Non-nil if should log messages to *star-debug*.")

;;; Initialise the key map
(defvar star-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'indent-for-tab-command)
    (define-key map "C-M-q" 'star-indent)
    (define-key map "C-c C-c" 'comment-region)
    (define-key map "C-c C-d" 'stardebug-buffer)
    (mapcar #'(lambda (key-seq)
		(define-key map 
		  key-seq 
		  'star-self-insert-and-indent-command))
	    '("{" "}" ";" "|" "," "(" ")"))
    map)
  "Keymap for Star major mode.")

(defun star-self-insert-and-indent-command (n)
  "Self insert and indent appropriately.
Argument N  oprefix."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value n))
  (indent-for-tab-command))

;;; Font-lock support

(defun star-one-of (l)
  "Construct optimized regexp from a list of strings (l)."
  (regexp-opt l t))

(defun star-compose-regexps (l)
  (if (cadr l) 
      (concat (car l) "\\|"
	      (star-compose-regexps (cdr l)))
    (car l)))

(defconst star-font-lock-function-regexp
  "^[ \t]*\\(\\sw+\\)([][0-9_a-zA-Z?,.:`'\\ ]*)[ \t]*=>"
  "Regular expression matching the function declarations to highlight in Star mode.")

;;; Regular expression matching important star operators
(defvar star-line-comment-regexp
  "\\(--[ \t].*$\\)")

(defvar star-line-comment-regexp-bol
  (concat "^" star-line-comment-regexp))

(defvar star-body-comment-regexp
  "/\\*"
  "Star body comment start")

(defvar star-keyword-regexp
  (concat "\\<"
	  (star-one-of
	   '(
	     "import"
	     "private"
	     "public"

	     "contract"
	     "implementation"
	     
	     "valof"
	     "return"
	     "do"
	     "if"
	     "then"
	     "else"
	     "while"
	     "for"
	     "in"
	     "open"

	     "try"
	     "catch"
	     "throw"

	     "where"
	     "type"
	     "all"
	     "exists"
	     "let"
	     )) "\\>")
  "Regular expression matching the keywords to highlight in Star mode.")

(defvar star-constant-regexp
  (concat "\\<"
	  (star-one-of
	   '(
	     "true"
	     "false"
	     "this"
	     ))
	  "\\>")
	  
  "Regular expression matching constants and numbers in Star mode.")

(defvar star-symbol-regexp
  (star-one-of '(
		 "::="
		 "=>"
		 "->"
		 "<=>"
		 "\\.\\."
		 ":="
		 "\\.="
		 "\\^="
		 ">="
		 ">>="
		 "<-"
		 "=="
		 "=<"
		 "="
		 "<~"
		 "\\*>"
		 "::="
		 "::"
		 ":"
		 "&&"
		 "\\|"
		 "\\|\\|"
		 "%%"
		 "~~"
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
		 ))
  "Regular expression matching the symbols to highlight in Star mode."
  )

(defvar star-type-regexp
  (concat "\\<"
	  (star-one-of
	   '(
	     "boolean"
	     "void"
	     "float"
	     "integer"
	     "string"
	     "action"
	     "task"
	     "list"
	     "set"
	     "cons"
	     "option"
	     )) "\\>")
  "Regular expression matching the standard types to highlight in Star mode.")

(defvar star-builtin-regexp
  (concat "\\<"
	  (star-one-of
	   '(
	     "show"
	     "assert"
	     "+"
	     "-"
	     "\\*"
	     "/"
	     ">"
	     "<"
	     "=<"
	     ">="
	     )) "\\>")
  "Regular expression matching some of the standard builtins.")

(defconst star-close-par "[])}]"
  "Star close parentheses")

(defvar star-mode-font-lock-defaults
  `(
    (,star-line-comment-regexp (1 font-lock-comment-face))
    (,star-constant-regexp (1 font-lock-constant-face))
    (,star-keyword-regexp (1 font-lock-keyword-face))
    (,star-type-regexp (1 font-lock-type-face))
    (,star-builtin-regexp (1 font-lock-builtin-face))
    (,star-symbol-regexp (1 font-lock-keyword-face))
    )
  "Keywords to syntax highlight with variable."
  )

(defun star-init-font-lock ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(star-mode-font-lock-defaults nil nil nil nil))
  (font-lock-ensure)
  )

(defvar star-debugging nil
  "Non-nil if should log messages to *star-debug*")

(defun star-debug (msg &rest args)
  "Print a debug message to the *star-debug* buffer"
  (if star-debugging
      (save-excursion
	(set-buffer (get-buffer-create "*star-debug*"))
	(insert (apply 'format msg args)))))
  
;; Parsing in the buffer

;; Operators we might be indenting on
;;; Parse tables
(defconst star-operators
  ;; Prec Text  Regex  Push Pop Hanging Delta
  '((5000 "{"   "{"    t    nil  nil    star-brace-indent)
    (5000 "}"   "}"    nil  same nil	0)
    (4500 ". "  "\\.\\([ \n\t]\\|$\\)" nil    t  nil  0)
    (4000 "["   "\\["  t    nil  t	star-bracket-indent)
    (4000 "("   "("    t    nil  t	star-paren-indent)
    (4000 ")"   ")"    nil  same nil	0)
    (4000 "]"   "\\]"  nil  same nil	0)
    (1250 ";"   ";"    t    t    nil	0)
    (1200 "-->"  "-->" t    t    nil	star-arrow-indent)
    (1100 "catch" "catch" t    t nil    0)
    (900  ":="  ":="   t    t    nil    star-arrow-indent)
    (1460 "::=" "::="  t    t    t	(* star-arrow-indent 2))
    (1199 "="  "\\b=\\b"   t    t    t	star-arrow-indent)
    (1199 "=>"  "=>"   t    t    t	star-arrow-indent)
    (1199 "<~"  "<~"   t    t    nil	star-arrow-indent)
    (1199 "~>"  "~>"   t    t    nil	star-arrow-indent)
    (1010 "|:" "|:"  t    t   nil	(* star-arrow-indent 2))
    (1250 "|"   "[^|]|[^|]"  t    t    nil	0)
    (1060 "||"  "||"   t    t    nil	0)
    (1010  "where" "where" t t   nil	(* star-arrow-indent 2))
    (1000 ","   ","    t    t    nil	0)
    (900 "->"  "->"    t    t    nil	star-arrow-indent)
    (900  "=="  "=="   t    nil    nil    0)
    (900  ".="  "\\.=" t    t    nil    0)
    (900  "^="  "\\^=" t    t    nil    0)
    (1040 "?"   "\\?"  t    t    nil	star-query-indent)
    )
  "Star operators and precedences")

;;; Speed up table lookups by encoding
;;; information in operator symbols.
(defun star-setup-operators-hash ()
  (let ((l star-operators))
    (while l 
      (let* ((o (car l))
	     (precedence (1st o))
	     (text (2nd o))
	     (regex (3rd o))
	     (push (4th o))
	     (pop  (5th o))
	     (hanging (6th o))
	     (delta (7th o))
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

(defconst star-operators-regex
  (star-compose-regexps (mapcar 'caddr star-operators))
  "Regular expression matching important star operators")

(defconst star-next-token-regex
  (star-one-of
   (list star-operators-regex 
	 star-body-comment-regexp
	 star-line-comment-regexp))
  "where is the next token?"
  )

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
  (3rd (car parse-state)))

(defsubst star-parse-state-op (parse-state)
  (2nd (car parse-state)))

(defsubst star-parse-state-in-comment (parse-state)
  (4th (car parse-state)))

;; star-indent-cache holds the parse state 
;; at particular points in the buffer.
;; It is a sorted list (largest points first)
;; of elements (POINT . PARSE-STATE)
;; PARSE-STATE are cells (STATE . STACK)

(defvar star-indent-cache nil
  "Incremental parse state cache.")

(defun star-cached-indent (cache)
  (1st (1st cache)))

(defun star-cached-pos (cache)
  (caar cache))

(defun star-after-change-function (from to &rest rest)
  ;; The buffer has changed, we need to
  ;; remove any parse states that have been saved
  ;; past point 'from' in the buffer.
  (while (and star-indent-cache
	      (>= (star-cached-indent star-indent-cache) from))
    (setq star-indent-cache (cdr star-indent-cache))))

(defun star-pick-operator (str)
  (if (string-match "\\.\\([ \n\t]\\|$\\)" str)
      (intern ". ")
    (intern str)))

;;; Parse from POS to TO given initial PARSE-STATE
;;; Return final PARSE-STATE at TO.
(defun star-parse (pos to parse-state)
  (let* ((state (car parse-state))
	 (stack (cdr parse-state))
	 (tos-prec   (1st  state))
	 (tos-op     (2nd state))
	 (tos-indent (3rd  state))
	 (tos-in-comment (4th state)))
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
	    (let* ((symbol (star-pick-operator (match-string 0)))
		   (symbol-prec (get symbol 'precedence)))

	      ;; Check to see if we should pop any operators off the stack
	      (if (get symbol 'pop)
		  ;; Yes, pop off any lower precedence operators
		  (while (<= tos-prec symbol-prec)
		    (setq state (car stack)
			  stack (cdr stack)
			  tos-prec   (1st state)
			  tos-op     (2nd state)
			  tos-indent (3rd state))))
	      
	      (if (get symbol 'pop-until-same)
		  ;; Yes, pop of all operators until
		  ;; we meet an operator with the same
		  ;; precedence (for brackets)
		  (progn
		    (while (and (/= tos-prec symbol-prec) (cdr stack))
		      (setq state (car stack)
			    stack (cdr stack)
			    tos-prec   (1st state)
			    tos-op     (2nd state)
			    tos-indent (3rd state)))
		    (setq state (car stack)
			  stack (cdr stack)
			  tos-prec   (1st state)
			  tos-op     (2nd state)
			  tos-indent (3rd state))))

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
				     (3rd (car stack)))))))

		;; Not Hanging
		(setq tos-indent (+ tos-indent 
				    (eval (get symbol 'delta)))))
	      ))
	   
	   ((looking-at star-line-comment-regexp)   ;; Skip comment
	    (star-skip-line-comment))
	   ((looking-at star-body-comment-regexp)
	    (let ((co-col (current-column)))
	      (star-skip-block-comment)
	      (if (>= (point) to)
		  (setq tos-indent (1+ co-col)
			tos-in-comment t))))
;	   ((looking-at star-escaped-string-regex)
;	    (forward-char 2))
	   ((looking-at "\"")
	    (star-skip-string))
	   ((looking-at "\'")
	    (star-skip-string))
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
    (if
	(= parse-pos pos) ;; Have we already parsed up till here?
	parse-state
      ;; Nope
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
    )
  )

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

     ;; If it's a | we need to parse past it to get the
     ;; real indentation level 
     ;; (this method would work fine for close braces as well)
     ((looking-at "[|?]")
      (- (star-calculate-brace-indent 
	  (star-line-get-pos-after pos "[|?]"))
	 (star-vertical-bar-adjust pos "[|?]")))
     
     ;; Otherwise standard indent position
     (t 
      (star-calculate-brace-indent pos))
     )
    )
  )

(defun star-goto-first-non-whitespace-maybe ()
  (let ((dest (save-excursion
		(beginning-of-line)
		(skip-chars-forward " \t")
		(point))))
    (if (< (point) dest)
	(goto-char dest))))

(defun star-calculate-brace-indent (pos)
  (star-parse-state-indent (star-parse-until pos)))

(defsubst star-skip-block-comment ()
  (forward-comment 1))

(defsubst star-skip-line-comment ()
  (search-forward "\n"))

(defsubst star-skip-string ()
  (goto-char (or (scan-sexps (point) 1) (buffer-end 1))))

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

(defun star-indent ()
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

;;; Provide `star-mode' user callable function
(define-derived-mode star-mode prog-mode "Star Mode"
  :syntax-table star-mode-syntax-table
  :after-hook star-mode-hook
  "Major mode for editing Star programs"

  ;; Comments
  (setq-local comment-start "-- ")
  (setq-local comment-start-skip "-- +")
  (setq-local star-indent-cache nil)
  (setq-local indent-line-function 'star-indent-line)
  ;; After a buffer change, we need
  ;; to ensure that the cache is consistent.
  (setq-local after-change-functions
	      (cons 'star-after-change-function after-change-functions))

  (use-local-map star-mode-map)

  ;; very important that case-fold-search is nil
  ;; since Star is a case-sensitive language
  (setq case-fold-search nil)

  (star-init-font-lock)
  (run-hooks 'star-mode-hook)
  )

(provide 'star)

;;; star.el ends here
