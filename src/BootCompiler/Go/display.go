package display

import (
	term "term"
)

const ellipsis = "..."

type Display interface {
	Show(depth int) string
}

func (n *term.Normal) Show(depth int) string {
  if depth>=0 {
    return n.lbl.Show(depth-1)+showTpl(n.els,depth-1)
  } else{
    return ellipsis
  }
}

func showTpl(els []*term.Term,depth int) string {
  out := "("
  sep := ""
  for e:els {
    out += sep
    sep = ", "
    out += e.Show(depth-1)
  }
  return out+")"
}

func (ix *term.Integer) Show(depth int) string {
  if depth<0 {
    return ellipsis
  } else {
    return fmt.Sprintf("%v",ix.ix)
  }
}
