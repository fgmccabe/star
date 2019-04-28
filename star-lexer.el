;; Star lexer -*- lexical-binding: t; -*-

;;

(defconst star-num-regexp
  "\\([0-9]+\\(?:[.][0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\)?\\)"
  "regular expression that matches numeric literals")

(defconst star-id-regexp
  "\\([a-zA-Z_][a-zA-Z_0-9]*\\)"
  "regular expression that matches identifiers")

(
    
