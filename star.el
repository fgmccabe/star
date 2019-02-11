;;; star.el --- Emacs mode for Star
;;; Copyright (C) 2019 and beyond F.G. McCabe

(require 'cl)
(require 'font-lock)

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

;;; Font-lock support

(defvar star-font-lock-function-regexp
  "^[ \t]*\\(\\sw+\\)([][0-9_a-zA-Z?,.:`'\\ ]*)[ \t]*=>"
  "Regular expression matching the function declarations to highlight in Star mode")

(defvar star-include-regexp
  "import[ \t]+"
  "Regular expression matching the compiler import package statement")

(defvar star-comment-regexp-bol
  "^\\(--[ \t].*$\\)")

(defvar star-comment-regexp
  "\\(--[ \t].*$\\)")

(defun star-one-of (l)
  (regexp-opt l t))

(defvar star-keyword-regexp
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

(defconst star-symbol-regexp
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
		 "-"
		 "\\)"))
  "Regular expression matching the symbols to highlight in Star mode")

(defvar star-mode-font-lock-defaults
  `(
    (,star-comment-regexp-bol (1 font-lock-comment-face))
    (,star-comment-regexp     (1 font-lock-comment-face))
    (,star-keyword-regexp     (1 font-lock-keyword-face))
    (,star-symbol-regexp      (1 font-lock-reference-face))
    (,star-include-regexp     (1 font-lock-doc-string-face))
    (,star-font-lock-function-regexp    (1 font-lock-function-name-face))
    )
  "Keywords to syntax highlight with font-lock-mode"
  )

(defvar star-mode-hook nil)

;;; Provide `star-mode' user callable function
(define-derived-mode star-mode prog-mode "Star Mode"
  :syntax-table star-mode-syntax-table
  :after-hook star-mode-hook
  "Major mode for editing Star programs"

  ;; Comments
  (setq-local comment-start "-- ")
  (setq-local comment-start-skip "-- +")

  (setq font-lock-defaults star-mode-font-lock-defaults)

  (use-local-map star-mode-map)

  ;; very important that case-fold-search is nil
  ;; since Star is a case-sensitive language
  (setq case-fold-search nil)

  (font-lock-fontify-buffer)
  )

(provide 'star)
