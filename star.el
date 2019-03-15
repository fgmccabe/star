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
       (modify-syntax-entry ?( "(\)" table)
       (modify-syntax-entry ?) ")\(" table)
       (modify-syntax-entry ?[ "(\]" table)
       (modify-syntax-entry ?] ")\[" table)
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
    (define-key map "C-M-q" 'star-indent-sexp)
    (define-key map "C-c C-c" 'comment-region)
    (define-key map "C-c C-d" 'stardebug-buffer)
    map)
  "Keymap for Star major mode.")

(defun star-self-insert-and-indent-command (n)
  "Self insert and indent appropriately.
Argument N  oprefix."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value n))
  (indent-for-tab-command))

(defvar star-indent-cache nil
  "Incremental parse state cache.")

;;; Font-lock support

(defconst star-font-lock-function-regexp
  "^[ \t]*\\(\\sw+\\)([][0-9_a-zA-Z?,.:`'\\ ]*)[ \t]*=>"
  "Regular expression matching the function declarations to highlight in Star mode.")

(defvar star-comment-regexp-bol
  "^\\(--[ \t].*$\\)")

(defvar star-comment-regexp
  "\\(--[ \t].*$\\)")

(defun star-one-of (l)
  "Construct optimized regexp from a list of strings (L)."
  (regexp-opt l t))

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
  (star-one-of
   '(
     "true"
     "false"
     "this"
     "-?[0-9]+([.][-+]?[0-9]+)?"
     ))
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
		 "=="
		 "=<"
		 "="
		 "<~"
		 "\\*>"
		 "::="
		 "::"
		 ":"
		 "&&"
		 "|"
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


(defvar star-mode-font-lock-defaults
  `(
    (,star-comment-regexp (1 font-lock-comment-face))
    (,star-keyword-regexp (1 font-lock-keyword-face))
    (,star-type-regexp (1 font-lock-type-face))
    (,star-constant-regexp (1 font-lock-constant-face))
    (,star-builtin-regexp (1 font-lock-builtin-face))
    (,star-symbol-regexp (1 font-lock-reference-face))
    )
  "Keywords to syntax highlight with variable."
  )

(defun star-init-font-lock ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(star-mode-font-lock-defaults nil nil nil nil))
  (font-lock-ensure)
)

;;; Provide `star-mode' user callable function
(define-derived-mode star-mode prog-mode "Star Mode"
  :syntax-table star-mode-syntax-table
  :after-hook star-mode-hook
  "Major mode for editing Star programs"

  ;; Comments
  (setq-local comment-start "-- ")
  (setq-local comment-start-skip "-- +")

  (use-local-map star-mode-map)

  ;; very important that case-fold-search is nil
  ;; since Star is a case-sensitive language
  (setq case-fold-search nil)

  (star-init-font-lock)
  (run-hooks 'star-mode-hook)
  )

(provide 'star)

;;; star.el ends here
