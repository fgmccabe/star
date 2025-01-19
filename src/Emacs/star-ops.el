;;; starops.el -- Automatically generated -- Do not edit  -*- lexical-binding: t; -*-

(require 'star-util)

(defconst star-ops
  '(  ("all" ( (prefix 1010 1009)))
  (".<." ( (infix 699 700 699)))
  ("&&" ( (infix 909 910 910)))
  ("let" ( (prefix 899 898)))
  ("~=" ( (infix 899 900 899)))
  ("~>" ( (infix 1230 1231 1230)))
  ("throw" ( (prefix 930 929)))
  (".|." ( (infix 720 720 719)))
  ("do" ( (prefix 200 199) (infix 1199 1200 1199)))
  ("import" ( (prefix 900 899)))
  ("catch" ( (infix 1198 1199 1198)))
  ("valis" ( (prefix 930 929)))
  (",.." ( (infix 999 1000 999)))
  ("for" ( (prefix 1175 1174)))
  ("••" ( (infix 450 450 449)))
  ("..<" ( (infix 749 750 749)))
  ("**" ( (infix 600 600 599)))
  ("..>" ( (infix 749 750 749)))
  ("->" ( (infix 889 890 889)))
  (".+." ( (prefix 700 699)))
  ("raise" ( (prefix 930 929)))
  ("async" ( (prefix 1234 1233)))
  ("then" ( (infix 1179 1180 1179)))
  ("ζ" ( (prefix 1 0)))
  ("!" ( (postfix 99 100) (infix 99 100 99)))
  ("->>" ( (infix 1199 1200 1199)))
  ("?=" ( (infix 899 900 899)))
  ("default" ( (postfix 939 940)))
  ("<*" ( (infix 600 600 599)))
  ("#" ( (prefix 1750 1749) (infix 759 760 759)))
  ("??" ( (infix 919 920 920)))
  ("%" ( (infix 700 700 699)))
  (".>>>." ( (infix 600 600 599)))
  ("\\+" ( (infix 700 700 699)))
  ("*" ( (postfix 699 700) (infix 700 700 699)))
  ("\\-" ( (infix 700 700 699)))
  ("+" ( (postfix 699 700) (infix 720 720 719)))
  (".>>." ( (infix 600 600 599)))
  ("*>" ( (infix 904 905 904) (prefix 905 904)))
  ("resume" ( (prefix 999 998) (infix 998 999 998)))
  ("," ( (infix 999 1000 1000)))
  ("contract" ( (prefix 1560 1559)))
  ("\\/" ( (infix 720 720 719)))
  ("-" ( (prefix 300 299) (infix 720 720 719)))
  ("." ( (prefix 10 9) (infix 100 100 99)))
  ("raises" ( (infix 950 951 951) (prefix 999 998)))
  ("/" ( (infix 700 700 699)))
  ("try" ( (prefix 1200 1199)))
  ("exists" ( (prefix 1010 1009)))
  ("if" ( (prefix 1175 1174)))
  ("$$" ( (prefix 305 304)))
  (":" ( (infix 1249 1250 1249)))
  (";" ( (postfix 1250 1251) (infix 1250 1251 1251)))
  ("-->" ( (infix 1248 1249 1248)))
  ("<" ( (infix 899 900 899)))
  (".=" ( (infix 899 900 899)))
  ("=>>" ( (infix 949 950 950)))
  ("=" ( (infix 974 975 974)))
  ("|:" ( (infix 1234 1235 1234)))
  ("show" ( (prefix 1240 1239)))
  ("++" ( (infix 719 720 720)))
  (">" ( (infix 899 900 899)))
  ("return" ( (prefix 930 929)))
  ("@" ( (prefix 400 399) (infix 399 400 400)))
  ("|=" ( (infix 998 999 998)))
  ("in" ( (infix 899 900 900)))
  ("break" ( (prefix 10 9)))
  ("suspend" ( (prefix 999 998) (infix 998 999 998)))
  ("trace" ( (infix 139 140 139) (prefix 140 139)))
  ("~~" ( (infix 1239 1240 1240)))
  ("assert" ( (prefix 1240 1239)))
  ("!!" ( (postfix 99 100)))
  ("⊕" ( (infix 720 720 719)))
  (".^." ( (infix 720 720 719)))
  ("//" ( (infix 960 960 959)))
  ("public" ( (prefix 1700 1699)))
  ("ref" ( (prefix 899 898)))
  (".~." ( (prefix 650 649)))
  ("where" ( (infix 910 911 910)))
  ("=<" ( (infix 899 900 899)))
  ("case" ( (prefix 901 900)))
  ("==" ( (infix 899 900 899)))
  ("\\" ( (infix 700 700 699)))
  ("=>" ( (infix 949 950 950)))
  ("<=>" ( (infix 949 950 949)))
  ("valof" ( (prefix 300 299)))
  ("yield" ( (prefix 300 299)))
  ("while" ( (prefix 1175 1174)))
  ("private" ( (prefix 1700 1699)))
  ("•" ( (infix 450 450 449)))
  (".&." ( (infix 700 700 699)))
  ("///" ( (infix 960 960 959)))
  ("::" ( (infix 399 400 399)))
  ("+++" ( (infix 719 720 720)))
  (":=" ( (infix 974 975 974)))
  (":?" ( (infix 399 400 399)))
  (".<<." ( (infix 600 600 599)))
  ("implementation" ( (prefix 1260 1259)))
  (">>=" ( (infix 949 950 950)))
  ("^/" ( (infix 960 960 959)))
  ("<~" ( (infix 998 999 998)))
  ("type" ( (prefix 1251 1250)))
  ("|" ( (prefix 1548 1547) (infix 1548 1548 1547)))
  (".#." ( (infix 600 600 599)))
  ("~" ( (prefix 905 904)))
  ("^//" ( (infix 800 800 799)))
  ("||" ( (infix 919 920 920)))
  ("else" ( (infix 1199 1200 1200)))
  ("::=" ( (infix 1549 1550 1549)))
  ("/\\" ( (infix 700 700 699)))
  (">=" ( (infix 899 900 899)))
  (">>" ( (infix 949 950 950)))
)
  "Table of standard star operators"
)

(defconst star-brackets
  '(  ( "[||]" "[|" "|]" 2000)
  ( "<||>" "<|" "|>" 2000)
  ( "/../" "/." "./" 2000)
  ( "{..}" "{." ".}" 2000)
  ( "[]" "[" "]" 2000)
  ( "()" "(" ")" 2000)
  ( "{}" "{" "}" 2000)
  ( "{??}" "{?" "?}" 2000)
  ( "{!!}" "{!" "!}" 2000)
)
  "Table of standard star brackets"
)

(defconst star-keywords
  '("all"
"let"
"throw"
"do"
"import"
"catch"
"valis"
"for"
"raise"
"async"
"then"
"ζ"
"default"
"resume"
"contract"
"raises"
"try"
"exists"
"if"
"in"
"break"
"suspend"
"public"
"ref"
"where"
"case"
"generator"
"valof"
"yield"
"while"
"private"
"implementation"
"type"
"else"
)
  "Table of standard keywords"
)

(defun star-is-a-word (s)
  (string-match "\\`\\w+\\'" s))

(defvar star-op-regexp nil
  "operator regular expression")

(defvar star-bkt-regexp nil
  "bracket regular expression")

(defconst star-opers
  (let ((l star-ops)
        (b star-brackets)
        (ops (make-hash-table :test 'equal))
        (bs ())
        (syms ())
        (words ()))
      (while l
        (let* ((o (car l))
               (op (1st o))
               (specs (2nd o)))
          (puthash op specs ops)
	      (if (star-is-a-word op)
	        (setq words (cons op words))
	       (setq syms (cons op syms)))
          (setq l (cdr l))))
      (while b
        (let* ((o (car b))
               (left (2nd o))
               (right (3rd o))
               (inner (4th o)))
          (puthash left (list `(left ,right ,inner)) ops)
          (puthash right (list `(right ,left ,inner)) ops)
	      (setq bs (cons left (cons right bs)))
          (setq b (cdr b))))
       (let* ((symregexp (star-one-of syms))
              (wsregexp (concat "\\<" (star-one-of words) "\\>")))
         (setq star-op-regexp (star-compose-regexps (list symregexp wsregexp)))
         (setq star-bkt-regexp (star-one-of bs))
         ops)
        )
   "internal table of star operators"
)

(defun star-is-oper (op table mode)
  (let ((specs (gethash op table)))
    (if specs
      (catch 'star-is-oper
        (while specs
          (let ((sp (car specs)))
            (if (eq (1st sp) mode)
              (throw 'star-is-oper (cdr sp))
              (setq specs (cdr specs)))))
            nil)
       nil)))

(defun star-is-prefixop (op)
  (star-is-oper op star-opers 'prefix))

(defun star-prefix-priority (op)
  (let ((spec (star-is-prefixop op)))
    (if spec
      (1st spec)
      nil)))

(defun star-prefix-arg-priority (op)
  (let ((spec (star-is-prefixop op)))
    (if spec
      (2nd spec)
      nil)))

(defun star-is-infixop (op)
  (star-is-oper op star-opers 'infix))

(defun star-is-postfixop (op)
  (star-is-oper op star-opers 'postfix))

(defun star-is-left (op)
  (star-is-oper op star-opers 'left))

(defun star-is-right (op)
  (star-is-oper op star-opers 'right))

(provide 'star-ops)
