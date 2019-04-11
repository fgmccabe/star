;;; star.el --- Emacs mode for Star -*- lexical-binding: t; -*-
;;; Copyright (C) 2019 and beyond F.G. McCabe

;;; Commentary:
;; Implementation of Star mode
;; Supports fontification and indentation.
;;
;; This should be split into multiple files ... but ...

(require 'font-lock)

;;;
;;; Utility functions for star mode
;;;

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

(defun star-one-of (l)
  "Construct optimized regexp from a list of strings (l)."
  (regexp-opt l t))

(defun star-compose-regexps (l)
  (if (cadr l) 
      (concat (car l) "\\|"
	      (star-compose-regexps (cdr l)))
    (car l)))

(defsubst star-skip-block-comment ()
  (forward-comment 1))

(defsubst star-skip-line-comment ()
  (search-forward "\n"))

(defsubst star-skip-string ()
  (goto-char (or (scan-sexps (point) 1) (buffer-end 1))))

(defvar star-debugging nil
  "Non-nil if should log messages to *star-debug*")

(defun star-debug (msg &rest args)
  "Print a debug message to the *star-debug* buffer"
  (if star-debugging
      (let ((debug-buffer (get-buffer-create "*star-debug*")))
	(with-current-buffer debug-buffer 
	  (goto-char (point-max))
	  (insert (apply 'format (concat msg "\n") args)))))
  )

;;;
;;; Fontlock support for Star
;;;

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
       (modify-syntax-entry ?\( "()" table)
       (modify-syntax-entry ?\) ")(" table)
       (modify-syntax-entry ?\[ "(]" table)
       (modify-syntax-entry ?\] ")[" table)
       (modify-syntax-entry ?\{ "(}" table)
       (modify-syntax-entry ?\} "){" table)
       (modify-syntax-entry ?_ "w" table)
       (modify-syntax-entry ?\\ "\\" table)
       table))

;;; Initialise the key map
(defvar star-mode-map
  (let ((map (make-sparse-keymap))
        (electric-keys '("{" "}" ";" "|" "," "(" ")")))
    (define-key map "\t" 'indent-for-tab-command)
    (define-key map "\C-\M-q" 'star-indent)
    (define-key map "\C-c \C-c" 'comment-region)
    (define-key map "\C-c \C-d" 'stardebug-buffer)
    (dolist (key electric-keys)
      (define-key map 
	key 
	'star-self-insert-and-indent-command))
    map)
  "Keymap for Star major mode.")

;;; Font-lock support

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

(defconst star-close-par "[])}]"
  "Star close parentheses")

(defconst star-import-regexp
  "\\(import +[[:word:]]+\\([.][[:word:]]+\\)*\\)"
  "Match an import spec")

(defvar star-keyword-regexp
  (concat "\\<"
	  (star-one-of
	   '(
	     "private" "public"
	     "import"
	     "contract" "implementation"
	     "valof" "lift" "do" "if" "then" "else" "while" "for" "in"
	     "open"
	     "try" "catch" "throw"
	     "void"
	     "where" "type" "all" "exists" "let" "default"
	     "show" "assert")
	   )
	  "\\>")
  "Regular expression matching the keywords to highlight in Star mode."
  )

(defvar star-constant-regexp
  (concat "\\<\\("
	  (star-compose-regexps
	   '(
	     "true"
	     "false"
	     "this"
	     "none"
	     "some"
	     "zero"
	     "one"
	     "[-]?[0-9]+\\([.][0-9]+\\([eE][-+]?[0-9]+\\)?\\)?"
	     ))
	  "\\)\\>")
	  
  "Regular expression matching special constants and numbers in Star mode.")

(defvar star-symbol-regexp
  (star-one-of '(
		 "=>"
		 "->"
		 "<=>"
		 ".."
		 ":="
		 ".="
		 "^="
		 "<-"
		 "="
		 "<~"
		 "*>"
		 "::="
		 "::"
		 ":"
		 "&&"
		 "|"
		 "||"
		 "~~"
		 "@@"
		 "@"
		 "#"
		 "^"
		 "^^"
		 "\\+"
		 "\\="
		 ",.."
		 "."
		 ))
  "Regular expression matching the symbols to highlight in Star mode."
  )

(defvar star-type-regexp
  (concat "\\<"
	  (star-one-of
	   '(
	     "boolean" "float" "integer" "string" "ref"
	     "action" "task" "list" "set" "cons" "option"
	     )) "\\>")
  "Regular expression matching the standard types to highlight in Star mode.")

(defvar star-builtin-regexp
  (concat "[^-+*/<>=!]"
	  (star-one-of
	   '(
	     "+"
	     "-"
	     "*"
	     "/"
	     ">"
	     "<"
	     "=<"
	     ">="
	     "=="
	     ">>="
	     "!"
	     ))
	  "[^-+*/<>=!]")
  "Regular expression matching some of the standard builtins.")

(defvar star-mode-font-lock-defaults
  `(
    (,star-line-comment-regexp (1 font-lock-comment-face))
    (,star-constant-regexp (1 font-lock-constant-face))
    (,star-keyword-regexp (1 font-lock-keyword-face))
    (,star-type-regexp (1 font-lock-type-face))
    (,star-builtin-regexp (1 font-lock-builtin-face))
    (,star-symbol-regexp (1 font-lock-keyword-face))
    (,star-import-regexp (1 font-lock-doc-face))
    )
  "Keywords to syntax highlight with variable."
  )

(defun star-init-font-lock ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(star-mode-font-lock-defaults nil nil nil nil))
  (font-lock-ensure)
  )

;; Mode hook for Star

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

(defcustom star-bracket-indent 2
  "* Amount by which to indent after a left bracket in Star mode."
  :type 'integer
  :group 'star)

(defcustom star-arrow-indent 2
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

(defcustom build-repo "../Star/Build/"
  "* Where the repository for your project lives."
  :type 'directory
  :group 'star)

(defun star-self-insert-and-indent-command (n)
  "Self insert and indent appropriately.
Argument N  oprefix."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value n))
  (indent-for-tab-command))

;; Parsing in the buffer

;; Operators we might be indenting on
;;; Parse tables
(defvar star-operators
  ;; Prec Text  Regex  Align Hanging Delta
  '((5000 "{"   "{"    left  nil    star-brace-indent)
    (5000 "}"   "}"    right nil	0)
    (5000 "{."   "{\\." left  nil    star-brace-indent)
    (5000 ".}"   "\\.}"  right nil  0)
    (4500 ". "  "\\.\\([ \n\t]\\|$\\)" align nil  0)
    (3000 "("   "("    left nil	star-paren-indent)
    (3000 ")"   ")"    right nil	0)
    (2000 "["   "\\["  left nil star-bracket-indent)
    (2000 "]"   "\\]"  right nil 	0)
    (1250 ";"   ";"    align nil	0)
    (1100 "catch" "catch" align nil     0)
    (1000 "then" "then" align nil star-query-indent)
    (1000 "else" "else" align  t star-query-indent)
    (900  ":="  ":="   align  nil    star-arrow-indent)
    (1460 "::=" "::="  hang  nil	star-arrow-indent)
    (1199 "="  "\\b=\\b"  align nil	star-arrow-indent)
    (1199 "=>"  "=>"   align nil	star-arrow-indent)
    (1199 "<~"  "<~"   align nil	star-arrow-indent)
    (1199 "~>"  "~>"   align nil	star-arrow-indent)
    (1010 "|:" "|:"    align nil	0)
    (1250 "|"  "|"  align nil	0)
    (1060 "||"  "||"   align nil  t	0)
    (1040 "?"   "\\?"  align nil star-query-indent)
    (1010  "where" "where" hang nil	(* star-arrow-indent 2))
    (1000 ","   ","    align nil 0)
    (950 "&&"  "&&"    align nil 0)
    (900 "->"  "->"    align nil star-arrow-indent)
    (900  "=="  "=="   align nil 0)
    (900  ".="  "\\.=" align nil 0)
    (900  "^="  "\\^=" align nil 0)
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
	     (align (4th o))
	     (hanging (5th o))
	     (delta (6th o))
	     (symbol (intern text)))
	(put symbol 'precedence precedence)
	(put symbol 'text text)
	(put symbol 'regex regex)
	(put symbol 'align align)
	(put symbol 'hanging hanging)
	(put symbol 'delta delta)
	(put symbol 'length (length text)))
      (setq l (cdr l)))))
(star-setup-operators-hash)


(defconst star-operators-regex
  (star-compose-regexps (mapcar 'caddr star-operators))
  "Regular expression matching important star operators")

(defconst star-next-token-regex
  (star-compose-regexps
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

;; Accessor functions for a PARSE-STATE ((PREC OP INDENT IN-COMMENT) . STACK)
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

(defun star-state-prec (state)
  (1st state))

(defun star-state-op (state)
  (2nd state))

(defun star-state-indent (state)
  (3rd state))

(defun star-state-in-comment (state)
  (4th state))

(defun star-new-state (prec op indent incomment)
  (list prec op indent incomment)
  )

(defun star-adjust-indent (state indent)
  (star-new-state (star-state-prec state)
		  (star-state-op state)
		  indent
		  (star-state-in-comment state)))

(defun star-blank-line-to-left (pos)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (>= (point) pos)))

;;; Parse from POS to TO given initial PARSE-STATE
;;; Return final PARSE-STATE at TO.
(defun star-parse (pos to parse-state)
  (let* ((state (car parse-state))
	 (stack (cdr parse-state)))

    (save-excursion
      (goto-char pos)
      ;; We assume that the parsing does not
      ;; resume from within a (block) comment.
      ;; To implement that we would need
      ;; to check in-comment and scan for
      ;; end-of-comment (*/) to escape it first.

      (star-debug "\nstar-parse from %s to %s" pos to)
      (star-debug "state at start %s" state)
      (star-debug "stack at start %s" stack)
      
      (progn	
	(while (<= (point) to)
	  (cond 
	   ;; An important Star! operator
	   ((looking-at star-operators-regex)
	    (let* ((symbol (star-pick-operator (match-string 0)))
		   (symbol-prec (get symbol 'precedence))
		   (delta (eval (get symbol 'delta)))
		   (alignment (get symbol 'align)))

	      (star-debug "\nwe have operator %s @ %s : %s" symbol (point) alignment)
;;	      (star-debug "state: %s" state)
;;	      (star-debug "stack: %s" stack)

	      ;; Clear stack to proper base
	      (while (and stack
			  (< (star-state-prec state) symbol-prec))
		(setq state (car stack)
		      stack (cdr stack)))

	      (star-debug "after pop state: %s" state)
	      (star-debug "after pop stack: %s" stack)

	      (cond ((eq alignment 'left)
		     (progn
		       ;; push state back on
		       (setq stack (cons state stack))

		       ;; Compute new indentation
		       (let* ((indent (+ (star-state-indent state)
					 delta)))
			 (setq state (star-new-state
				      symbol-prec symbol
				      indent
				      nil)))
		       )
		     )
		    ((eq alignment 'right)
		     (progn
		       (if (and stack
				(= (star-state-prec state) symbol-prec))
		       ;; pop found state
			   (setq state (car stack)
				 stack (cdr stack)))
		       )
		     )
		    ((eq alignment 'align)
		     (progn
		       (if (/= (star-state-prec state) symbol-prec)
			   ;; push state back on
			   (setq stack (cons state stack))
			 )
		       (let*
			   ((indent (star-state-indent state)))

			 ;; Adjust the indentation for hanging
			 (if (and (get symbol 'hanging)
				  (star-blank-line-to-left (point)))
			     ;; Hanging
			    (setq delta (- delta (+ (get symbol 'length) 1)))
			 )
			 (setq state (star-new-state
				      symbol-prec symbol
				      (+ (star-state-indent state) delta)
				      nil))
			 )
		       )
		     )
		    ((eq alignment 'hang)
		     (progn
		       (if (/= (star-state-prec state) symbol-prec)
			   ;; push state back on
			   (setq stack (cons state stack))
			 )
		       )
		     (setq state (star-new-state
				  symbol-prec symbol
				  (current-column)
				  nil))
		     
		     )
		    )
	      
	      ;; Advance the pointer 
	      (forward-char (get symbol 'length))

	      (star-debug "state after operator: %s" state)
	      (star-debug "stack after operator: %s" stack)
	      )
	    )
	   
	   ((looking-at star-line-comment-regexp)   ;; Skip comment
	    (star-skip-line-comment))
	   ((looking-at star-body-comment-regexp)
	    (let ((co-col (current-column)))
	      (star-skip-block-comment)
	      (if (>= (point) to)
		  (setq state (star-adjust-indent state (1+ co-col))))))
	   ((looking-at "[\"\']") (star-skip-string))
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
	(star-debug "after parse state: %s" state)
	(star-debug "after parse stack: %s" stack)
	(cons state stack)))))

(defun star-parse-until (pos)
  ;; Find the most recent parse state in the cache 
  ;; that is <= pos

  (star-debug "parse until %s" pos)
  (star-debug "indent-cache: %s\n" star-indent-cache)
  
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
	    (star-debug "state from parse: %s" new-parse-state)
	    (if (star-parse-state-in-comment new-parse-state)
		new-parse-state
	      (progn
		;; Insert the new-parse-state into the indent-cache
		;; Cache is sorted, largest first.
		;; cache = (reverse after) <> [new-parse-state,parse-state,..before]

		(star-debug "indent-cache (before): %s\n" star-indent-cache)
		
		(setq star-indent-cache
		      (cons (cons parse-pos parse-state) 
			    before))
		(setq star-indent-cache
		      (cons (cons pos new-parse-state)
			    star-indent-cache))
		(while after
		  (setq star-indent-cache (cons (car after) star-indent-cache)
			after (cdr after)))

		(star-debug "indent-cache (after): %s\n" star-indent-cache)
			  
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
     ;; ((looking-at "[|?]")
     ;;  (- (star-calculate-brace-indent 
     ;; 	  (star-line-get-pos-after pos "[|?]"))
     ;; 	 (star-vertical-bar-adjust pos "[|?]")))
     
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

(defun star-calculate-outer-indent (pos)
  (save-excursion
    (condition-case nil
	(progn (goto-char pos)
	       (goto-char (scan-lists pos -1 1))
	       (star-indentation-level (point)))
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

(defvar-local star--flymake-proc nil)

(defvar star-compiler "/Users/fgm/Projects/star/src/BootCompiler/sbc"
  "Exec path to the star compiler")

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
      (setq star--flymake-proc
	    (star-compile source
			  build-repo
			  (star-package)
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
;; Instructions for use:
;; Add the following to your .emacs to enable automatic use of star mode
;;
;; Assumes that this file is located at:
;; (add-to-list 'load-path "~/lib/emacs/site-lisp")
;; (autoload 'star-mode "star")
;; (add-to-list 'auto-mode-alist '("\\.\\(star\\)$" . star-mode))
