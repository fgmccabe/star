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
       table))

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
	     "ref"
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
	     "zero"
	     "one"
	     "[-]?[0-9]+\\([.][0-9]+\\([eE][-+]?[0-9]+\\)?\\)?"
	     ))
	  "\\)\\>")
	  
  "Regular expression matching special constants and numbers in Star mode.")

(defvar star-symbol-regexp
  (star-one-of '(
		 "::="
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
	     "boolean" "float" "integer" "string"
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

(provide 'star-fontlock)
