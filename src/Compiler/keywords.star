star.comp.keywords {
  import star.

  public keyword:(string)=>boolean.
  keyword("|") => true.
  keyword("||") => true.
  keyword("&&") => true.
  keyword("*>") => true.
  keyword(";") => true.
  keyword(":") => true.
  keyword("::") => true.
  keyword(",") => true.
  keyword("?") => true.
  keyword("!") => true.
  keyword("^") => true.
  keyword("~") => true.
  keyword("~~") => true.
  keyword("=") => true.
  keyword(".=") => true.
  keyword("=.") => true.
  keyword("^=") => true.
  keyword(".~") => true.
  keyword("=>") => true.
  keyword("<=>") => true.
  keyword("->") => true.
  keyword("-->") => true.
  keyword("->>") => true.
  keyword("::=") => true.
  keyword("<~") => true.
  keyword("~>") => true.
  keyword("\\+") => true.
  keyword(",..") => true.
  keyword("..,") => true.
  keyword(".") => true.
  keyword("|:") => true.
  keyword("@") => true.
  keyword("let") => true.
  keyword("ref") => true.
  keyword("import") => true.
  keyword("public") => true.
  keyword("private") => true.
  keyword("open") => true.
  keyword("contract") => true.
  keyword("implementation") => true.
  keyword("type") => true.
  keyword("where") => true.
  keyword("all") => true.
  keyword("of") => true.
  keyword("exists") => true.
  keyword("let") => true.
  keyword("default") => true.
  keyword("in") => true.
  keyword("do") => true.
  keyword("valof") => true.
  keyword("valis") => true.
  keyword("throw") => true.
  keyword("case") => true.
  keyword("if") => true.
  keyword("then") => true.
  keyword("else") => true.
  keyword("while") => true.
  keyword("nothing") => true.
  keyword("for") => true.
  keyword("try") => true.
  keyword("catch") => true.
  keyword("val") => true.
  keyword("#") => true.

  keyword(_) default => false.
}
