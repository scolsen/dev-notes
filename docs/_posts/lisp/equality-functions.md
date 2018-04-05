# Equality Functions

Lisp provides four equality functions:
EQ: Returns true if the compared objects are the same. Should not be used to compare numbers or characters as under certain implementations values can be represented by different objects in the Lisp reader: e.g. (eq 3 3) can return NIL or () (false)

EQL: Considers two objects *of the same class equal* if they represent the same value. (eql 1 1) = t (eq 1 1.0) = NIL

EQUAL: lists are equivalent if, when compared recursively, they have the same structure and content. Characters of the same case are equal.

EQUALP: Numbers are equal if they represent the same value (equalp 1 1.0) = t and strings and characters are equal regardless of case.
