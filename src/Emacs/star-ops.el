;;; starops.el -- Automatically generated -- Do not edit  -*- lexical-binding: t; -*-

(require 'star-util)

(defconst star-ops
  '(  ("!" ( (postfix 99 100) (infix 99 100 99)))
  ("!!" ( (postfix 99 100)))
  ("#" ( (infix 759 760 759)))
  ("$$" ( (prefix 305 304)))
  ("%" ( (infix 700 700 699)))
  ("&&" ( (infix 909 910 910)))
  ("*" ( (postfix 699 700) (infix 700 700 699)))
  ("**" ( (infix 600 600 599)))
  ("*>" ( (infix 904 905 904) (prefix 905 904)))
  ("+" ( (postfix 699 700) (infix 720 720 719)))
  ("++" ( (infix 719 720 720)))
  ("+++" ( (infix 719 720 720)))
  ("," ( (infix 999 1000 1000)))
  (",.." ( (infix 999 1000 999)))
  ("-" ( (prefix 300 299) (infix 720 720 719)))
  ("-->" ( (infix 1248 1249 1248)))
  ("->" ( (infix 889 890 889)))
  ("->>" ( (infix 1199 1200 1199)))
  ("." ( (prefix 10 9) (infix 100 100 99)))
  (".#." ( (infix 600 600 599)))
  (".&." ( (infix 700 700 699)))
  (".+." ( (prefix 700 699)))
  ("..<" ( (infix 749 750 749)))
  ("..>" ( (infix 749 750 749)))
  (".<." ( (infix 699 700 699)))
  (".<<." ( (infix 600 600 599)))
  (".=" ( (infix 899 900 899)))
  (".>>." ( (infix 600 600 599)))
  (".>>>." ( (infix 600 600 599)))
  (".^." ( (infix 720 720 719)))
  (".|." ( (infix 720 720 719)))
  (".~." ( (prefix 650 649)))
  ("/" ( (infix 700 700 699)))
  ("//" ( (infix 960 960 959)))
  ("///" ( (infix 960 960 959)))
  ("/\\" ( (infix 700 700 699)))
  (":" ( (infix 1249 1250 1249)))
  ("::" ( (infix 399 400 399)))
  ("::=" ( (infix 1549 1550 1549)))
  (":=" ( (infix 974 975 974)))
  (":?" ( (infix 399 400 399)))
  (";" ( (postfix 1250 1251) (infix 1250 1251 1251)))
  ("<" ( (infix 899 900 899)))
  ("<*" ( (infix 600 600 599)))
  ("<-" ( (infix 974 975 974)))
  ("<=>" ( (infix 949 950 949)))
  ("<~" ( (infix 998 999 998)))
  ("=" ( (infix 974 975 974)))
  ("=<" ( (infix 899 900 899)))
  ("==" ( (infix 899 900 899)))
  ("=>" ( (infix 949 950 950)))
  ("=>>" ( (infix 949 950 950)))
  (">" ( (infix 899 900 899)))
  (">=" ( (infix 899 900 899)))
  (">>" ( (infix 949 950 950)))
  (">>=" ( (infix 949 950 950)))
  ("?" ( (infix 299 300 299) (prefix 300 299)))
  ("?=" ( (infix 899 900 899)))
  ("??" ( (infix 919 920 920) (prefix 950 949)))
  ("?|" ( (infix 960 960 959)))
  ("@" ( (prefix 400 399) (infix 399 400 400)))
  ("\\" ( (infix 700 700 699)))
  ("\\+" ( (infix 700 700 699)))
  ("\\-" ( (infix 700 700 699)))
  ("\\/" ( (infix 720 720 719)))
  ("^/" ( (infix 960 960 959)))
  ("^//" ( (infix 800 800 799)))
  ("all" ( (prefix 1010 1009)))
  ("assert" ( (prefix 1240 1239)))
  ("async" ( (prefix 1234 1233)))
  ("break" ( (prefix 10 9)))
  ("case" ( (prefix 901 900)))
  ("catch" ( (infix 1198 1199 1198)))
  ("collect" ( (prefix 300 299)))
  ("contract" ( (prefix 1560 1559)))
  ("default" ( (postfix 939 940)))
  ("do" ( (prefix 200 199) (infix 1199 1200 1199)))
  ("elemis" ( (prefix 930 929)))
  ("else" ( (infix 1199 1200 1200)))
  ("exists" ( (prefix 1010 1009)))
  ("for" ( (prefix 1175 1174)))
  ("if" ( (prefix 1175 1174)))
  ("implementation" ( (prefix 1260 1259)))
  ("import" ( (prefix 900 899)))
  ("in" ( (infix 899 900 900)))
  ("let" ( (prefix 899 898)))
  ("private" ( (prefix 1700 1699)))
  ("public" ( (prefix 1700 1699)))
  ("ref" ( (prefix 899 898)))
  ("resume" ( (infix 898 899 898)))
  ("retire" ( (prefix 899 898) (infix 898 899 898)))
  ("return" ( (prefix 930 929)))
  ("show" ( (prefix 1240 1239)))
  ("suspend" ( (prefix 899 898) (infix 898 899 898)))
  ("then" ( (infix 1179 1180 1179)))
  ("throw" ( (prefix 230 229)))
  ("throws" ( (infix 949 950 949)))
  ("trace" ( (infix 139 140 139) (prefix 140 139)))
  ("try" ( (prefix 1200 1199)))
  ("valis" ( (prefix 930 929)))
  ("valof" ( (prefix 300 299)))
  ("where" ( (infix 910 911 910)))
  ("while" ( (prefix 1175 1174)))
  ("yield" ( (prefix 300 299)))
  ("|" ( (prefix 1548 1547) (infix 1548 1548 1547)))
  ("|=" ( (infix 1234 1235 1234)))
  ("||" ( (infix 919 920 920)))
  ("~" ( (prefix 905 904)))
  ("~=" ( (infix 899 900 899)))
  ("~>" ( (infix 1230 1231 1230)))
  ("~~" ( (infix 1239 1240 1240)))
  ("ζ" ( (prefix 1 0)))
  ("•" ( (infix 450 450 449)))
  ("••" ( (infix 450 450 449)))
  ("⊕" ( (infix 720 720 719)))
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
"async"
"break"
"case"
"catch"
"collect"
"contract"
"default"
"do"
"elemis"
"else"
"exists"
"for"
"generator"
"if"
"implementation"
"import"
"in"
"let"
"private"
"public"
"ref"
"resume"
"retire"
"suspend"
"then"
"throw"
"throws"
"try"
"valis"
"valof"
"void"
"where"
"while"
"yield"
"ζ"
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
