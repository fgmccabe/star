;;; star-config.el -*- lexical-binding: t; -*-

;; star mode configuration parameters

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

(defcustom star-repo-name ".star-repo"
  "Name of the star repository directory"
  :type 'directory
  :group 'star)

(defcustom star-compiler (executable-find "sbc")
  "Exec path to the star compiler"
  :type 'file
  :group 'star)

(defcustom star-compiler-flags '("-g")
  "Custom flags to pass into star compiler"
  :type (list 'string)
  :group 'star)

;; Customization point -- where is the executable for star
(defcustom star-path (executable-find "star")
  "Path name for star executable"
  :type 'file
  :group 'star)

(provide 'star-config)
