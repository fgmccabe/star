;;; star.el --- Emacs mode for Star -*- lexical-binding: t; -*-
;;; Copyright (C) 2019 and beyond F.G. McCabe

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

(defvar star-debugging t
  "Non-nil if should log messages to *star-debug*")

(defun star-debug (msg &rest args)
  "Print a debug message to the *star-debug* buffer"
  (if star-debugging
      (let ((debug-buffer (get-buffer-create "*star-debug*")))
	(with-current-buffer debug-buffer 
	  (goto-char (point-max))
	  (insert (apply 'format (concat msg "\n") args)))))
  )

(defun star-clear-debug ()
  (if star-debugging
      (let ((debug-buffer (get-buffer-create "*star-debug*")))
	(with-current-buffer debug-buffer 
	  (erase-buffer)))))

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
       (modify-syntax-entry ?* ". 23b" table)
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
    (define-key map "\C-c\C-c" 'comment-region)
    (define-key map "\C-c\C-d" 'stardebug-buffer)
    (define-key map "\C-cm" 'flymake-mode)
    (define-key map "\C-cr" 'run-star)
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
		 "=>" "->" "<=>" ".." ":=" ".="
		 "^=" "<-" "=" "<~" "*>" "::=" "::" ":" "&&" "|" "||"
		 "~~" "@@" "@" "#" "^" "^^" "\\+" "\\=" ",.." "."
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
	     "+" "-" "*" "/" ">" "<" "=<" ">=" "==" ">>=" "!"
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

(defcustom star-paren-indent 2
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

(defcustom star-pred-indent 2
  "* Amount by which to indent after a predicate in Star mode."
  :type 'integer
  :group 'star)

(defcustom comment-column 40
  "* The column where -- comments are placed."
  :type 'integer
  :group 'star)

(defun star-self-insert-and-indent-command (n)
  "Self insert and indent appropriately.
Argument N  oprefix."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value n))
  (indent-for-tab-command))

;; Parsing the buffer

;; Accessor functions for a PARSE-STATE ((PREC OP INDENT IN-COMMENT) . STACK)
     
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
  (autoload 'star-after-change-function "star-indent")
  (autoload 'star-indent-line "star-indent")
  (autoload 'star-init-operators "star-ops")
  (autoload 'enable-star-flymake "star-repo")
  (autoload 'star-compile-maybe "star-repo")
  (autoload 'star-flymake "star-repo")
  (autoload 'run-star "star-shell")
  (add-hook 'flymake-diagnostic-functions 'star-flymake nil t)
  (add-hook 'after-save-hook 'star-compile-maybe nil t)

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
