// Copyright 2013 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This file implements typechecking of call and selector expressions.

package types

import (
	"github.com/go-li/transpiler/ast"
	"go/token"
)

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

	//	spew.Dump(oldt)
	//	spew.Dump(newt)
	//	panic("could not substitute wildcard")

	return Typ[Invalid]
}

func inval(a, b Type) Type {
	if a == Typ[Invalid] {
		return a
	}
	return b
}

func substitute(where Type, what Type, saw map[*Named]*Named) Type {

	//	println("Substituting:")
	//	spew.Dump(where)

	switch x := where.(type) {
	case *Pointer:
		ptr := substitute(x.Elem(), what, saw)
		if ptr == x.Elem() {
			return x
		}
		return inval(ptr, NewPointer(ptr))
	case *Basic:
		if x.kind == UntypedVoid {
			return what
		}
		return x
	case *Signature:
		para := substitute(x.Params(), what, saw).(*Tuple)
		resu := substitute(x.Results(), what, saw).(*Tuple)
		if para == x.Params() && resu == x.Results() {
			return x
		}
		return inval(resu, inval(para, NewSignature(x.Recv(), para, resu, x.Variadic())))
	case *Tuple:
		if x == nil {
			return x
		}
		var allsame = true
		var y = make([]*Var, x.Len())
		for i := range y {

			typ := x.At(i).Type()
			subs := substitute(typ, what, saw)
			if subs == Typ[Invalid] {
				return subs
			}

			if typ == subs {
				y[i] = x.At(i)
			} else {
				y[i] = NewVar(x.At(i).Pos(), x.At(i).Pkg(), "", subs)
				allsame = false
			}
		}
		if allsame {
			return x
		} else {
			return NewTuple(y...)
		}
	case *Named:

		if _, ok := saw[x]; !ok {
			saw[x] = nil

			val := substitute(x.Underlying(), what, saw)
			if val == Typ[Invalid] {
				return val
			}

			if val == x.Underlying() {
				return x
			}
		} else {
			return saw[x]
		}

	case *Slice:
		sub := substitute(x.Elem(), what, saw)
		if sub == x.Elem() {
			return x
		}
		return inval(sub, NewSlice(sub))
	case *Array:
		sub := substitute(x.Elem(), what, saw)
		if sub == x.Elem() {
			return x
		}
		return inval(sub, NewArray(sub, x.Len()))
	case *Struct:
		if x == nil {
			return x
		}
		var allsame = true
		var y = make([]*Var, x.NumFields())
		for i := range y {

			typ := x.Field(i).Type()
			subs := substitute(typ, what, saw)
			if subs == Typ[Invalid] {
				return subs
			}

			if typ == subs {
				y[i] = x.Field(i)
			} else {
				y[i] = NewVar(x.Field(i).Pos(), x.Field(i).Pkg(), "", subs)
				allsame = false
			}
		}
		if allsame {
			return x
		} else {
			return NewStruct(y, nil)
		}
	case *Chan:
		sub := substitute(x.Elem(), what, saw)
		if sub == x.Elem() {
			return x
		}
		return inval(sub, NewChan(x.Dir(), sub))
	case *Map:
		var a, b Type = nil, nil
		b = substitute(x.Key(), what, saw)
		if b == Typ[Invalid] || !Comparable(b) {
			return Typ[Invalid]
		}
		a = substitute(x.Elem(), what, saw)
		if a == x.Elem() && b == x.Key() {
			return x
		}
		return inval(a, inval(b, NewMap(b, a)))
	case *Interface:
		if x.NumMethods() == 0 {
			return x
		}
	}

	//	spew.Dump(what)
	//	spew.Dump(where)
	//	panic("could not substitute wildcard")

	return nil
}

func (check *Checker) call(x *operand, e *ast.CallExpr) exprKind {
	check.exprOrType(x, e.Fun)

	switch x.mode {
	case invalid:
		check.use(e.Args...)
		x.mode = invalid
		x.expr = e
		return statement

	case typexpr:
		// conversion
		T := x.typ
		x.mode = invalid
		switch n := len(e.Args); n {
		case 0:
			check.errorf(e.Rparen, "missing argument in conversion to %s", T)
		case 1:
			check.expr(x, e.Args[0])
			if x.mode != invalid {
				check.conversion(x, T)
			}
		default:
			check.errorf(e.Args[n-1].Pos(), "too many arguments in conversion to %s", T)
		}
		x.expr = e
		return conversion

	case builtin:
		id := x.id
		if !check.builtin(x, e, id) {
			x.mode = invalid
		}
		x.expr = e
		// a non-constant result implies a function call
		if x.mode != invalid && x.mode != constant_ {
			check.hasCallOrRecv = true
		}
		return predeclaredFuncs[id].kind

	default:
		// function/method call
		sig, _ := x.typ.Underlying().(*Signature)
		if sig == nil {
			check.invalidOp(x.pos(), "cannot call non-function %s", x)
			x.mode = invalid
			x.expr = e
			return statement
		}

		arg, n, _ := unpack(func(x *operand, i int) { check.multiExpr(x, e.Args[i]) }, len(e.Args), false)

		var wcard Type
		saw := make(map[*Named]*Named)

		if arg != nil {
			wcard = check.arguments(x, e, sig, arg, n, saw)
			if wcard == nil {
			} else if wcard == Typ[Invalid] {
				check.errorf(x.pos(), "wildcard type conflict in argument %s", x)
				x.mode = invalid
				x.expr = e
				return statement
			} else if isUntyped(wcard) {
				check.errorf(x.pos(), "wildcard cannot be untyped %s", x)
				x.mode = invalid
				x.expr = e
				return statement
			} else if _, ok := saw[nil]; ok && !Comparable(wcard) {
				check.errorf(x.pos(), "argument %s implies not comparable wildcard map key", x)
				x.mode = invalid
				x.expr = e
				return statement
			}
		} else {
			x.mode = invalid
		}

		// determine result
		switch sig.results.Len() {
		case 0:
			x.mode = novalue
		case 1:
			x.mode = value
			x.typ = sig.results.vars[0].typ // unpack tuple
		default:
			x.mode = value
			x.typ = sig.results
		}

		if wcard != nil && x.mode == value {
			if _, ok := wcard.(*Basic); !ok || wcard.(*Basic).kind != UntypedVoid {
				var xtyp = substitute(x.typ, wcard, saw)
				if xtyp == Typ[Invalid] {
					check.errorf(x.pos(), "map key of result has uncomparable type")
					x.mode = invalid
					x.expr = e
					return statement
				} else {
					x.typ = xtyp
				}
			}
		}

		x.expr = e
		check.hasCallOrRecv = true

		return statement
	}
}

// use type-checks each argument.
// Useful to make sure expressions are evaluated
// (and variables are "used") in the presence of other errors.
// The arguments may be nil.
func (check *Checker) use(arg ...ast.Expr) {
	var x operand
	for _, e := range arg {
		// The nil check below is necessary since certain AST fields
		// may legally be nil (e.g., the ast.SliceExpr.High field).
		if e != nil {
			check.rawExpr(&x, e, nil)
		}
	}
}

// useLHS is like use, but doesn't "use" top-level identifiers.
// It should be called instead of use if the arguments are
// expressions on the lhs of an assignment.
// The arguments must not be nil.
func (check *Checker) useLHS(arg ...ast.Expr) {
	var x operand
	for _, e := range arg {
		// If the lhs is an identifier denoting a variable v, this assignment
		// is not a 'use' of v. Remember current value of v.used and restore
		// after evaluating the lhs via check.rawExpr.
		var v *Var
		var v_used bool
		if ident, _ := unparen(e).(*ast.Ident); ident != nil {
			// never type-check the blank name on the lhs
			if ident.Name == "_" {
				continue
			}
			if _, obj := check.scope.LookupParent(ident.Name, token.NoPos); obj != nil {
				// It's ok to mark non-local variables, but ignore variables
				// from other packages to avoid potential race conditions with
				// dot-imported variables.
				if w, _ := obj.(*Var); w != nil && w.pkg == check.pkg {
					v = w
					v_used = v.used
				}
			}
		}
		check.rawExpr(&x, e, nil)
		if v != nil {
			v.used = v_used // restore v.used
		}
	}
}

// useGetter is like use, but takes a getter instead of a list of expressions.
// It should be called instead of use if a getter is present to avoid repeated
// evaluation of the first argument (since the getter was likely obtained via
// unpack, which may have evaluated the first argument already).
func (check *Checker) useGetter(get getter, n int) {
	var x operand
	for i := 0; i < n; i++ {
		get(&x, i)
	}
}

// A getter sets x as the i'th operand, where 0 <= i < n and n is the total
// number of operands (context-specific, and maintained elsewhere). A getter
// type-checks the i'th operand; the details of the actual check are getter-
// specific.
type getter func(x *operand, i int)

// unpack takes a getter get and a number of operands n. If n == 1, unpack
// calls the incoming getter for the first operand. If that operand is
// invalid, unpack returns (nil, 0, false). Otherwise, if that operand is a
// function call, or a comma-ok expression and allowCommaOk is set, the result
// is a new getter and operand count providing access to the function results,
// or comma-ok values, respectively. The third result value reports if it
// is indeed the comma-ok case. In all other cases, the incoming getter and
// operand count are returned unchanged, and the third result value is false.
//
// In other words, if there's exactly one operand that - after type-checking
// by calling get - stands for multiple operands, the resulting getter provides
// access to those operands instead.
//
// If the returned getter is called at most once for a given operand index i
// (including i == 0), that operand is guaranteed to cause only one call of
// the incoming getter with that i.
//
func unpack(get getter, n int, allowCommaOk bool) (getter, int, bool) {
	if n != 1 {
		// zero or multiple values
		return get, n, false
	}
	// possibly result of an n-valued function call or comma,ok value
	var x0 operand
	get(&x0, 0)
	if x0.mode == invalid {
		return nil, 0, false
	}

	if t, ok := x0.typ.(*Tuple); ok {
		// result of an n-valued function call
		return func(x *operand, i int) {
			x.mode = value
			x.expr = x0.expr
			x.typ = t.At(i).typ
		}, t.Len(), false
	}

	if x0.mode == mapindex || x0.mode == commaok {
		// comma-ok value
		if allowCommaOk {
			a := [2]Type{x0.typ, Typ[UntypedBool]}
			return func(x *operand, i int) {
				x.mode = value
				x.expr = x0.expr
				x.typ = a[i]
			}, 2, true
		}
		x0.mode = value
	}

	// single value
	return func(x *operand, i int) {
		if i != 0 {
			unreachable()
		}
		*x = x0
	}, 1, false
}

// arguments checks argument passing for the call with the given signature.
// The arg function provides the operand for the i'th argument.
func (check *Checker) arguments(x *operand, call *ast.CallExpr, sig *Signature, arg getter, n int, saw map[*Named]*Named) (wildc Type) {
	if call.Ellipsis.IsValid() {
		// last argument is of the form x...
		if !sig.variadic {
			check.errorf(call.Ellipsis, "cannot use ... in call to non-variadic %s", call.Fun)
			check.useGetter(arg, n)
			return nil
		}
		if len(call.Args) == 1 && n > 1 {
			// f()... is not permitted if f() is multi-valued
			check.errorf(call.Ellipsis, "cannot use ... with %d-valued %s", n, call.Args[0])
			check.useGetter(arg, n)
			return nil
		}
	}

	// evaluate arguments
	for i := 0; i < n; i++ {
		arg(x, i)
		if x.mode != invalid {
			var ellipsis token.Pos
			if i == n-1 && call.Ellipsis.IsValid() {
				ellipsis = call.Ellipsis
			}
			wildc = check_typeconflict(check.argument(call.Fun, sig, i, x, ellipsis, saw), wildc,
				sig.variadic && i+1 >= sig.params.Len())
		}
	}

	// ban generic varargs callsite with undetectable wildcard due to no vararg arg
	if sig.variadic && wildc == nil && n+1 == sig.params.Len() {
		// discover if vararg arg is generic type
		var typ = sig.params.vars[n].typ
		_ = typ

		var is_this_vararg_generic = wildcard(typ, typ, nil)

		//		spew.Dump(is_this_vararg_generic)

		if Typ[UntypedVoid] == is_this_vararg_generic {
			check.errorf(call.Rparen, "undetermined wildcard from missing varargs in call to %s", call.Fun)
			return Typ[Invalid]
		}

	}

	// check argument count
	if sig.variadic {
		// a variadic function accepts an "empty"
		// last argument: count one extra
		n++
	}
	if n < sig.params.Len() {
		check.errorf(call.Rparen, "too few arguments in call to %s", call.Fun)
		// ok to continue
	}
	return wildc
}

func wildcard(a Type, b Type, saw map[*Named]*Named) Type {
	if saw == nil {
		saw = make(map[*Named]*Named)
	}
	switch b.(type) {
	case *Pointer:
		if x, ok := a.(*Basic); ok {
			if x.Kind() == UntypedNil {
				if wildcard(b, b, nil) == Typ[UntypedVoid] {
					return Typ[UntypedNil]
				}
			}
		}

		if _, ok := a.(*Pointer); ok {
			return wildcard(a.(*Pointer).Elem(), b.(*Pointer).Elem(), saw)
		}
	case *Basic:
		if b.(*Basic).Kind() == UntypedVoid {
			return a
		}
		return nil
	case *Array:
		if x, ok := a.(*Array); ok && x.Len() == b.(*Array).Len() {
			return wildcard(a.(*Array).Elem(), b.(*Array).Elem(), saw)
		}
	case *Slice:
		if x, ok := a.(*Basic); ok {
			if x.Kind() == UntypedNil {
				if wildcard(b, b, nil) == Typ[UntypedVoid] {
					return Typ[UntypedNil]
				}
			}
		}
		if _, ok := a.(*Slice); ok {
			return wildcard(a.(*Slice).Elem(), b.(*Slice).Elem(), saw)
		}
	case *Signature:
		if x, ok := a.(*Basic); ok {
			if x.Kind() == UntypedNil {
				if wildcard(b, b, nil) == Typ[UntypedVoid] {
					return Typ[UntypedNil]
				}
			}
		}
		if _, ok := a.(*Signature); ok {
			x := wildcard(a.(*Signature).Params(), b.(*Signature).Params(), saw)
			y := wildcard(a.(*Signature).Results(), b.(*Signature).Results(), saw)
			return check_typeconflict(x, y, false)
		}
	case *Tuple:
		if _, ok := a.(*Tuple); ok && a.(*Tuple).Len() == b.(*Tuple).Len() {
			var x, y Type = nil, nil
			for i := 0; i < a.(*Tuple).Len(); i++ {
				x = wildcard(a.(*Tuple).At(i).Type(), b.(*Tuple).At(i).Type(), saw)
				y = check_typeconflict(y, x, false)
			}
			return y
		}
	case *Named:
		if _, ok := a.(*Named); ok {
			if look, ok := saw[b.(*Named)]; ok {
				if !IdenticalIgnoreTags(look, a.(*Named)) {
					return Typ[Invalid]
				}
				return nil
			}
			saw[b.(*Named)] = a.(*Named)
			return wildcard(a.(*Named).Underlying(), b.(*Named).Underlying(), saw)
		}
	case *Struct:
		if _, ok := a.(*Struct); ok && a.(*Struct).NumFields() == b.(*Struct).NumFields() {
			var x, y Type = nil, nil
			for i := 0; i < a.(*Struct).NumFields(); i++ {
				x = wildcard(a.(*Struct).Field(i).Type(), b.(*Struct).Field(i).Type(), saw)
				y = check_typeconflict(y, x, false)
			}
			return y
		}
	case *Chan:
		if x, ok := a.(*Basic); ok {
			if x.Kind() == UntypedNil {
				if wildcard(b, b, nil) == Typ[UntypedVoid] {
					return Typ[UntypedNil]
				}
			}
		}

		if x, ok := a.(*Chan); ok && x.Dir() == b.(*Chan).Dir() {
			return wildcard(a.(*Chan).Elem(), b.(*Chan).Elem(), saw)
		}
	case *Map:
		if x, ok := a.(*Basic); ok {
			if x.Kind() == UntypedNil {
				if wildcard(b, b, nil) == Typ[UntypedVoid] {
					return Typ[UntypedNil]
				}
			}
		}
		if _, ok := a.(*Map); ok {
			var x Type = nil
			x = wildcard(a.(*Map).Key(), b.(*Map).Key(), saw)
			if x != nil {
				saw[nil] = nil
			}
			return check_typeconflict(wildcard(a.(*Map).Elem(), b.(*Map).Elem(), saw), x, false)

		}

	}

	//	spew.Dump(a)
	//	spew.Dump(b)
	//	panic("could not determine wildcard")

	return nil
}

// argument checks passing of argument x to the i'th parameter of the given signature.
// If ellipsis is valid, the argument is followed by ... at that position in the call.
func (check *Checker) argument(fun ast.Expr, sig *Signature, i int, x *operand, ellipsis token.Pos, saw map[*Named]*Named) Type {
	check.singleValue(x)
	if x.mode == invalid {
		return nil
	}

	n := sig.params.Len()

	// determine parameter type
	var typ Type
	switch {
	case i < n:
		typ = sig.params.vars[i].typ
	case sig.variadic:
		typ = sig.params.vars[n-1].typ
		if debug {
			if _, ok := typ.(*Slice); !ok {
				check.dump("%s: expected unnamed slice type, got %s", sig.params.vars[n-1].Pos(), typ)
			}
		}
	default:
		check.errorf(x.pos(), "too many arguments")
		return nil
	}

	if ellipsis.IsValid() {
		// argument is of the form x... and x is single-valued
		if i != n-1 {
			check.errorf(ellipsis, "can only use ... with matching parameter")
			return nil
		}
		if _, ok := x.typ.Underlying().(*Slice); !ok && x.typ != Typ[UntypedNil] { // see issue #18268
			check.errorf(x.pos(), "cannot use %s as parameter of type %s", x, typ)
			return nil
		}
	} else if sig.variadic && i >= n-1 {
		// use the variadic parameter slice's element type
		typ = typ.(*Slice).elem
	}

	check.assignment(x, typ, check.sprintf("argument to %s", fun))

	return wildcard(x.typ, typ, saw)
}

type FakeObj struct{}

func (FakeObj) Parent() *Scope                        { return nil }  // scope in which this object is declared; nil for methods and struct fields
func (FakeObj) Pos() token.Pos                        { return 0 }    // position of object identifier in declaration
func (FakeObj) Pkg() *Package                         { return nil }  // package to which this object belongs; nil for labels and objects in the Universe scope
func (FakeObj) Name() string                          { return "" }   // package local object name
func (FakeObj) Type() Type                            { return nil }  // object type
func (FakeObj) Exported() bool                        { return true } // reports whether the name starts with a capital letter
func (FakeObj) Id() string                            { return "" }   // object name if exported, qualified name if not exported (see func Id)
func (FakeObj) String() string                        { return "" }
func (FakeObj) order() uint32                         { return 0 }
func (FakeObj) setOrder(uint32)                       {}
func (FakeObj) setParent(*Scope)                      {}
func (FakeObj) sameId(pkg *Package, name string) bool { return false }
func (FakeObj) scopePos() token.Pos                   { return 0 }
func (FakeObj) setScopePos(pos token.Pos)             {}

func (check *Checker) selector(x *operand, e *ast.SelectorExpr) {
	// these must be declared before the "goto Error" statements
	var (
		obj      Object
		index    []int
		indirect bool
	)

	sel := e.Sel.Name
	// If the identifier refers to a package, handle everything here
	// so we don't need a "package" mode for operands: package names
	// can only appear in qualified identifiers which are mapped to
	// selector expressions.
	if ident, ok := e.X.(*ast.Ident); ok {
		_, obj := check.scope.LookupParent(ident.Name, check.pos)
		if pname, _ := obj.(*PkgName); pname != nil {
			assert(pname.pkg == check.pkg)
			check.recordUse(ident, pname)
			pname.used = true
			pkg := pname.imported
			exp := pkg.scope.Lookup(sel)
			if exp == nil {
				if !pkg.fake {
					check.errorf(e.Pos(), "%s not declared by package %s", sel, pkg.name)
					goto Error
				}
			} else {
				if !exp.Exported() && !pkg.fake {
					check.errorf(e.Pos(), "%s not exported by package %s", sel, pkg.name)
					// ok to continue
				}
				check.recordUse(e.Sel, exp)
			}

			if exp == nil && pkg.fake {
				x.mode = novalue
				x.typ = NewNamed(NewTypeName(0, pkg, sel, emptyInterface.Complete()), emptyInterface.Complete(), []*Func{})
				x.expr = e
				return
			}

			// Simplified version of the code for *ast.Idents:
			// - imported objects are always fully initialized
			switch exp := exp.(type) {
			case *Const:
				assert(exp.Val() != nil)
				x.mode = constant_
				x.typ = exp.typ
				x.val = exp.val
			case *TypeName:
				x.mode = typexpr
				x.typ = exp.typ
			case *Var:
				x.mode = variable
				x.typ = exp.typ
			case *Func:
				x.mode = value
				x.typ = exp.typ
			case *Builtin:
				x.mode = builtin
				x.typ = exp.typ
				x.id = exp.id
			default:
				check.dump("unexpected object %v", exp)
				unreachable()
			}
			x.expr = e
			return
		}
	}

	check.exprOrType(x, e.X)
	if x.mode == invalid {
		goto Error
	}

	obj, index, indirect = LookupFieldOrMethod(x.typ, x.mode == variable, check.pkg, sel)
	if obj == nil {
		switch {
		case index != nil:
			// TODO(gri) should provide actual type where the conflict happens
			check.invalidOp(e.Pos(), "ambiguous selector %s", sel)
		case indirect:
			check.invalidOp(e.Pos(), "%s is not in method set of %s", sel, x.typ)
		default:
			//			check.invalidOp(e.Pos(), "%s has no field or method %s", x, sel)
		}
		//		goto Error
		obj = FakeObj{}
	}

	if x.mode == typexpr {
		// method expression
		m, _ := obj.(*Func)
		if m == nil {
			check.invalidOp(e.Pos(), "%s has no method %s", x, sel)
			goto Error
		}

		check.recordSelection(e, MethodExpr, x.typ, m, index, indirect)

		// the receiver type becomes the type of the first function
		// argument of the method expression's function type
		var params []*Var
		sig := m.typ.(*Signature)
		if sig.params != nil {
			params = sig.params.vars
		}
		x.mode = value
		x.typ = &Signature{
			params:   NewTuple(append([]*Var{NewVar(token.NoPos, check.pkg, "", x.typ)}, params...)...),
			results:  sig.results,
			variadic: sig.variadic,
		}

		check.addDeclDep(m)

	} else {
		// regular selector
		switch obj := obj.(type) {
		case *Var:
			check.recordSelection(e, FieldVal, x.typ, obj, index, indirect)
			if x.mode == variable || indirect {
				x.mode = variable
			} else {
				x.mode = value
			}
			x.typ = obj.typ

		case *Func:
			// TODO(gri) If we needed to take into account the receiver's
			// addressability, should we report the type &(x.typ) instead?
			check.recordSelection(e, MethodVal, x.typ, obj, index, indirect)

			if debug {
				// Verify that LookupFieldOrMethod and MethodSet.Lookup agree.
				typ := x.typ
				if x.mode == variable {
					// If typ is not an (unnamed) pointer or an interface,
					// use *typ instead, because the method set of *typ
					// includes the methods of typ.
					// Variables are addressable, so we can always take their
					// address.
					if _, ok := typ.(*Pointer); !ok && !IsInterface(typ) {
						typ = &Pointer{base: typ}
					}
				}
				// If we created a synthetic pointer type above, we will throw
				// away the method set computed here after use.
				// TODO(gri) Method set computation should probably always compute
				// both, the value and the pointer receiver method set and represent
				// them in a single structure.
				// TODO(gri) Consider also using a method set cache for the lifetime
				// of checker once we rely on MethodSet lookup instead of individual
				// lookup.
				mset := NewMethodSet(typ)
				if m := mset.Lookup(check.pkg, sel); m == nil || m.obj != obj {
					check.dump("%s: (%s).%v -> %s", e.Pos(), typ, obj.name, m)
					check.dump("%s\n", mset)
					panic("method sets and lookup don't agree")
				}
			}

			x.mode = value

			// remove receiver
			sig := *obj.typ.(*Signature)
			sig.recv = nil
			x.typ = &sig

			check.addDeclDep(obj)

		default:
			//			unreachable()
		}
	}

	// everything went well
	x.expr = e
	return

Error:
	x.mode = invalid
	x.expr = e
}
