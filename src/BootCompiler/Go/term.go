package term

// Define the standard Star term structure for go-language

// All terms implement the Term interface
type Term interface {
	class() Class
}

// A class is also defined by interface, some classes are special
type Class interface {
	nameOf() string
}

type SpecialClass struct {
	name string
}

func (c *SpecialClass) nameOf() string {
	return c.name
}

// A label is a marker that specifies what sort of normal term we have
type Lbl struct {
	name  string
	arity int
}

var labelClass = SpecialClass{"labelClass"}

// Labels are also terms
func (l *Lbl) class() Class {
	return &labelClass
}

// And they are classes
func (l *Lbl) nameOf() string {
	return l.name
}

func (l *Lbl) arityOf() int {
	return l.arity
}

// A normal term has a label and an array of argument elements
type Normal struct {
	lbl *Lbl
	els []*Term
}

// The normal term's class is its label
func (l *Normal) class() Class {
	return l.lbl
}

// Integers, Floats and Strings are special terms. Their classes are special

type Integer struct {
	ix int64
}

func (i *Integer) class() Class {
	return &integerClass
}

var integerClass = SpecialClass{"integerClass"}

type Float struct {
	dx float64
}

func (i *Float) class() Class {
	return &floatClass
}

var floatClass = SpecialClass{"floatClass"}

type String struct {
	sx string
}

func (i *String) class() Class {
	return &stringClass
}

var stringClass = SpecialClass{"stringClass"}
