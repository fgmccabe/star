;;; star.el --- Emacs mode for Star -*- lexical-binding: t; -*-
;;; Copyright (C) 2019 and beyond F.G. McCabe

;; Implementation of Star mode
;; Supports fontification and indentation.
;;
;; This should be split into multiple files ... but ...

(require 'font-lock)
(require 'star-util)
(require 'star-indent)
(require 'star-ops)
(require 'star-config)
(require 'star-shell)

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
    (define-key map "\C-cr" 'star-run)
    (define-key map "\C-co" 'star-remote)
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

(defconst star-import-regexp
  "\\(import +[[:word:]]+\\([.][[:word:]]+\\)*\\)"
  "Match an import spec")

(defconst star-annot-regexp
  "\\([[:word:]]+\\s*:\\b\\)"
  "Match a type annotation spec")

(defvar star-keyword-regexp
  (concat "\\<"
	  (star-one-of star-keywords)
	  "\\>")
  "Regular expression matching the keywords to highlight in Star mode."
  )

(defvar star-char-regexp
  "\\(\\\\u[0-9a-f]*;\\|\\\\.\\|[^\\]\\)")

(defvar star-constant-regexp
  (concat "\\<\\("
	  (star-compose-regexps
	   `(
	     "true"
	     "false"
	     "this"
	     "none"
	     "some"
	     "zero"
	     "one"
	     ,(concat "0c" star-char-regexp)
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
	     "boolean" "float" "integer" "chars" "string" "ref"
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
    (,star-annot-regexp (1 font-lock-function-name-face))
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

(defun star-self-insert-and-indent-command (n)
  "Self insert and indent appropriately.
Argument N  oprefix."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value n))
  (indent-for-tab-command))

;; Parsing the buffer

;; Accessor functions for a PARSE-STATE ((PREC OP INDENT IN-COMMENT) . STACK)
     
;;; Provide `star-mode' user callable function
(define-derived-mode star-mode prog-mode "Star"
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
  (autoload 'enable-star-flymake "star-repo")
  (autoload 'star-compile-maybe "star-repo")
  (autoload 'star-flymake "star-repo")
  (autoload 'star-run "star-shell")
  (autoload 'star-remote "star-shell")
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
