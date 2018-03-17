package main

import (
	"github.com/go-li/transpiler/types"
)

type Type = types.Type

type Basic = types.Basic

type Interface = types.Interface

type BasicInfo = types.BasicInfo

type BasicKind = types.BasicKind

const IsUntyped = 1 << 6
const Invalid = 0

var Typ = types.Typ

func IdenticalIgnoreTags(a, b Type) bool {
	return types.IdenticalIgnoreTags(a, b)
}

func AssignableTo(a, b Type) bool {
	return types.AssignableTo(a, b)
}

//------------------------------------------------------------------------------
func isUntyped(typ Type) bool {
	t, ok := typ.Underlying().(*Basic)
	return ok && t.Info()&IsUntyped != 0
}

// Here we silently return arbitrary new type if old type is nil (or untyped nil)
// otherwise the old and new types shall be identical
// If not, we raise a wildcard conflict error
func check_typeconflict(newt, oldt Type, vararg_eface_promotion bool) Type {
	if newt == nil {
		return oldt
	}
	if oldt == nil {
		return newt
	}
	if isUntyped(newt) {
		return oldt
	}
	if isUntyped(oldt) {
		return newt
	}
	if IdenticalIgnoreTags(newt, oldt) {
		return oldt
	}
	if vararg_eface_promotion {
		switch x := oldt.(type) {
		case *Interface:
			if x.NumMethods() == 0 {
				if AssignableTo(newt, oldt) {
					return oldt
				}
			}

		}
	}
	return Typ[Invalid]
}
