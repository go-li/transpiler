package main

import (
	"flag"
	"io/ioutil"
)
import (
	"fmt"
	"github.com/go-li/mapast"
	"github.com/go-li/transpiler/ast"
	"github.com/go-li/transpiler/importer"
	"github.com/go-li/transpiler/parser"
	"github.com/go-li/transpiler/types"
	"go/token"
)
import "crypto/sha256"
import "github.com/gopherjs/gopherjs/js"
import "strings"
import "os"
import "path/filepath"

func error_throw_generictype() []string {
	return []string{"X"}
}

func error_found_generictype(x []string) bool {
	return len(x) == 1 && len(x[0]) == 1 && x[0][0] == 'X'
}

/*
 * check Generic Type Banned in Nongeneric toplevel function scope
 */
func check_gentype_banned_nongeneric(asttree map[uint64][]byte, root uint64, gtypenames map[string]uint64, intype bool) (out []string) {
	root = mapast.O(root)
	var ok bool
	for _, ok = asttree[root]; ok; _, ok = asttree[root] {
		if mapast.Which(asttree[root]) == nil {
			if intype {
				if gtypenames[string(asttree[root])] != 0 {
					return error_throw_generictype()
				}
			}
		} else if &asttree[root][0] == &mapast.GenericExp[0] {
			return error_throw_generictype()
		} else if &asttree[root][0] == &mapast.RootOfType[0] {
			var err = check_gentype_banned_nongeneric(asttree, root, gtypenames, true)
			if error_found_generictype(err) {
				return err
			}
		} else {
			var err = check_gentype_banned_nongeneric(asttree, root, gtypenames, intype)
			if error_found_generictype(err) {
				return err
			}
		}
		root++
	}
	return out
}

/*
 * check Generic Type Banned in Toplevel scope
 */
func check_gentype_banned(asttree map[uint64][]byte, root uint64, gtypenames map[string]uint64, gfuncset map[uint64]byte) (out []string) {
	root = mapast.O(root)
	var ok bool
	for _, ok = asttree[root]; ok; _, ok = asttree[root] {
		if mapast.Which(asttree[root]) == nil {
		} else if &asttree[root][0] == &mapast.ToplevFunc[0] {
			if _, ok := gfuncset[root]; ok {
				root++
				continue
			}
			var err = check_gentype_banned_nongeneric(asttree, root, gtypenames, false)
			if error_found_generictype(err) {
				out = append(out, "nongeneric func "+string(asttree[mapast.O(root)]))
			}
		} else if &asttree[root][0] == &mapast.GenericExp[0] {
			out = append(out, "toplevel var or const")
		} else if &asttree[root][0] == &mapast.TypDefStmt[0] {
		} else {
			var err = check_gentype_banned(asttree, root, gtypenames, gfuncset)
			if len(err) > 0 {
				out = append(out, err...)
			}
		}
		root++
	}
	return out
}

const inside_a_type_marker uint64 = 1

/*
 * We recursively lookup toplevel generic types, till there are none to be found
 * This includes toplevel structs types and toplevel interface types.
 */
func resolve_toplevel_generic_types(ast map[uint64][]byte, gtypenames map[string]uint64, gtypeset map[uint64]struct{}, iterator uint64) (num int) {
	var node = ast[iterator]
	if len(node) == 0 {
		return 0
	}
	switch &node[0] {
	case &mapast.RootMatter[0]:
		return resolve_toplevel_generic_types(ast, gtypenames, gtypeset, mapast.O(iterator))

	case &mapast.GenericExp[0]:
		return 1

	}
	for i := uint64(0); ast[iterator+i] != nil; i++ {
		switch &ast[iterator+i][0] {
		case &mapast.FileMatter[0]:
			num += resolve_toplevel_generic_types(ast, gtypenames, gtypeset, mapast.O(iterator+i))

		case &mapast.TypedIdent[0]:
			num += resolve_toplevel_generic_types(ast, gtypenames, gtypeset, mapast.O(iterator+i))

		case &mapast.RootOfType[0]:
			gtypeset[inside_a_type_marker] = struct{}{}
			num += resolve_toplevel_generic_types(ast, gtypenames, gtypeset, mapast.O(iterator+i))
			delete(gtypeset, inside_a_type_marker)
			var typename = string(ast[mapast.O(iterator+i)])
			if mapast.Which(ast[mapast.O(iterator+i)]) == nil {
				if gtypenames[typename] != 0 {
					num++
				}
			}

		case &mapast.TypDefStmt[0]:
			var typename = string(ast[mapast.O(iterator+i)])
			if _, ok := gtypeset[iterator+i]; !ok {
				n := resolve_toplevel_generic_types(ast, gtypenames, gtypeset, mapast.O(iterator+i))
				if n > 0 {
					gtypenames[typename] = iterator + i
					gtypeset[iterator+i] = struct{}{}
				}
				num += n
			}

		case &mapast.StructType[0]:
			num += resolve_toplevel_generic_types(ast, gtypenames, gtypeset, mapast.O(iterator+i))

		case &mapast.IfceTypExp[0]:
			num += resolve_toplevel_generic_types(ast, gtypenames, gtypeset, mapast.O(iterator+i))

		case &mapast.IfceMethod[0]:
			gtypeset[inside_a_type_marker] = struct{}{}
			num += resolve_toplevel_generic_types(ast, gtypenames, gtypeset, mapast.O(iterator+i))
			delete(gtypeset, inside_a_type_marker)

		case &mapast.Expression[0]:
			num += resolve_toplevel_generic_types(ast, gtypenames, gtypeset, mapast.O(iterator+i))
			if _, ok := gtypeset[inside_a_type_marker]; ok {
				var typename = string(ast[mapast.O(iterator+i)])
				if mapast.Which(ast[mapast.O(iterator+i)]) == nil {
					if gtypenames[typename] != 0 {
						num++
					}
				}
			}

		case &mapast.GenericExp[0]:
			return 1

		}
	}
	return num
}

/*
 * We mark toplevel generic functions.
 */
func resolve_toplevel_generic_funcs(ast map[uint64][]byte, gtypenames map[string]uint64, gtypeset map[uint64]struct{}, grecvfuncnames map[string]struct{}, gfuncnames map[GfuncName]uint64, gfuncset map[uint64]byte, iterator uint64) (num int) {
	var node = ast[iterator]
	if len(node) == 0 {
		return 0
	}
	switch &node[0] {
	case &mapast.RootMatter[0]:
		return resolve_toplevel_generic_funcs(ast, gtypenames, gtypeset, grecvfuncnames, gfuncnames, gfuncset, mapast.O(iterator))

	case &mapast.ToplevFunc[0]:
		if _, ok := gfuncset[iterator]; ok {
			return 0
		}
		var put byte = 254
		var funcname = string(ast[mapast.O(iterator)])
		if false {
			fmt.Printf("Toplevel function %s %d %d \n", funcname, len(node), cap(node))
		}
		if len(node)-1 == 0 || len(node)-1 == 1 {
			for i := len(node) - 1; i < cap(node)-1; i++ {
				if i+1 == cap(node)-1 && len(ast[mapast.O(iterator)+uint64(i+1)])-1 == 2 {
					put = byte(2 * (i - (len(node) - 1)))
				}
				if false {
					mapast.PrintDump(ast, mapast.O(iterator)+uint64(i+1), 0)
				}
				num += resolve_toplevel_generic_funcs(ast, gtypenames, gtypeset, grecvfuncnames, gfuncnames, gfuncset, mapast.O(iterator)+uint64(i+1))
			}
		}
		if num > 0 {
			if len(node)-1 == 0 {
				gfuncnames[GfuncName{name: funcname}] = iterator
			} else {
				rcv := mapast.O(mapast.O(iterator) + 1)
				if len(ast[rcv]) > 0 && &ast[rcv][0] != &mapast.RootOfType[0] {
					rcv++
				}
				if false {
					mapast.PrintDump(ast, (rcv), 0)
				}
				gfuncnames[b32_to_GfuncName(funcname, ast_sha256(ast, (rcv)))] = iterator
				grecvfuncnames[funcname] = struct{}{}
			}
			if len(node)-1 == 1 {
				put |= 1
			}
			gfuncset[iterator] = put
		}
		return num

	case &mapast.GenericExp[0]:
		return 1

	}
	for i := uint64(0); ast[iterator+i] != nil; i++ {
		switch &ast[iterator+i][0] {
		case &mapast.FileMatter[0]:
			num += resolve_toplevel_generic_funcs(ast, gtypenames, gtypeset, grecvfuncnames, gfuncnames, gfuncset, mapast.O(iterator+i))

		case &mapast.RootOfType[0]:
			gtypeset[inside_a_type_marker] = struct{}{}
			num += resolve_toplevel_generic_funcs(ast, gtypenames, gtypeset, grecvfuncnames, gfuncnames, gfuncset, mapast.O(iterator+i))
			delete(gtypeset, inside_a_type_marker)
			if mapast.Which(ast[mapast.O(iterator+i)]) == nil {
				var typename = string(ast[mapast.O(iterator+i)])
				if false {
					fmt.Printf("Root typename %s \n", typename)
				}
				if gtypenames[typename] != 0 {
					num++
				}
			}

		case &mapast.StructType[0]:
			_, mark := gtypeset[inside_a_type_marker]
			delete(gtypeset, inside_a_type_marker)
			num += resolve_toplevel_generic_funcs(ast, gtypenames, gtypeset, grecvfuncnames, gfuncnames, gfuncset, mapast.O(iterator+i))
			if mark {
				gtypeset[inside_a_type_marker] = struct{}{}
			}

		case &mapast.IfceTypExp[0]:
			_, mark := gtypeset[inside_a_type_marker]
			delete(gtypeset, inside_a_type_marker)
			num += resolve_toplevel_generic_funcs(ast, gtypenames, gtypeset, grecvfuncnames, gfuncnames, gfuncset, mapast.O(iterator+i))
			if mark {
				gtypeset[inside_a_type_marker] = struct{}{}
			}

		case &mapast.Expression[0]:
			num += resolve_toplevel_generic_funcs(ast, gtypenames, gtypeset, grecvfuncnames, gfuncnames, gfuncset, mapast.O(iterator+i))
			if _, ok := gtypeset[inside_a_type_marker]; ok {
				if mapast.Which(ast[mapast.O(iterator+i)]) == nil {
					var typename = string(ast[mapast.O(iterator+i)])
					if false {
						fmt.Printf("Expr typename %s \n", typename)
					}
					if gtypenames[typename] != 0 {
						num++
					}
				}
			}

		case &mapast.IfceMethod[0]:
			num += resolve_toplevel_generic_funcs(ast, gtypenames, gtypeset, grecvfuncnames, gfuncnames, gfuncset, mapast.O(iterator+i))

		case &mapast.ToplevFunc[0]:
			num += resolve_toplevel_generic_funcs(ast, gtypenames, gtypeset, grecvfuncnames, gfuncnames, gfuncset, (iterator + i))

		case &mapast.GenericExp[0]:
			num += resolve_toplevel_generic_funcs(ast, gtypenames, gtypeset, grecvfuncnames, gfuncnames, gfuncset, (iterator + i))

		case &mapast.TypedIdent[0]:
			num += resolve_toplevel_generic_funcs(ast, gtypenames, gtypeset, grecvfuncnames, gfuncnames, gfuncset, mapast.O(iterator+i))

		}
	}
	return num
}

/*
 * We mark generic callsites.
 */
func resolve_generic_calls(ast map[uint64][]byte, grecvfuncnames map[string]struct{}, gfuncnames map[GfuncName]uint64, gfuncset map[uint64]byte, gcallset map[uint64]*Callsite, gcallstack *[]uint64, iterator uint64) (num int) {
	var node = ast[iterator]
	if len(node) == 0 {
		return 0
	}
	switch &node[0] {
	case &mapast.RootMatter[0]:
		return resolve_generic_calls(ast, grecvfuncnames, gfuncnames, gfuncset, gcallset, gcallstack, mapast.O(iterator))

	}
	for i := uint64(0); ast[iterator+i] != nil; i++ {
		switch &ast[iterator+i][0] {
		case &mapast.FileMatter[0]:
			num += resolve_generic_calls(ast, grecvfuncnames, gfuncnames, gfuncset, gcallset, gcallstack, mapast.O(iterator+i))

		case &mapast.ToplevFunc[0]:
			var saved = gfuncnames[GfuncName{}]
			gfuncnames[GfuncName{}] = iterator + i
			num += resolve_generic_calls(ast, grecvfuncnames, gfuncnames, gfuncset, gcallset, gcallstack, mapast.O(iterator+i))
			gfuncnames[GfuncName{}] = saved

		case &mapast.BlocOfCode[0]:
			num += resolve_generic_calls(ast, grecvfuncnames, gfuncnames, gfuncset, gcallset, gcallstack, mapast.O(iterator+i))

		case &mapast.Expression[0]:
			if len(ast[iterator+i])-1 == int(mapast.ExpressionCall) || len(ast[iterator+i])-1 == int(mapast.ExpressionCallDotDotDot) {
				if mapast.Which(ast[mapast.O(iterator+i)]) == nil {
					var where = gfuncnames[GfuncName{string(ast[mapast.O(iterator+i)]), [4]uint64{}}]
					if where != 0 && (gfuncset[where]&1) == 0 {
						gcallset[iterator+i] = &Callsite{calls_func: where, inside_func: gfuncnames[GfuncName{}]}
						(*gcallstack) = append((*gcallstack), iterator+i)
					}
				} else if len(ast[mapast.O(iterator+i)])-1 == int(mapast.ExpressionDot) {
					if _, ok := grecvfuncnames[string(ast[mapast.O(mapast.O(iterator+i))+1])]; ok {
						gcallset[iterator+i] = &Callsite{calls_func: 0, inside_func: gfuncnames[GfuncName{}]}
						(*gcallstack) = append((*gcallstack), iterator+i)
					}
				}
			}
			num += resolve_generic_calls(ast, grecvfuncnames, gfuncnames, gfuncset, gcallset, gcallstack, mapast.O(iterator+i))

		case &mapast.GoDferStmt[0]:
			num += resolve_generic_calls(ast, grecvfuncnames, gfuncnames, gfuncset, gcallset, gcallstack, mapast.O(iterator+i))

		case &mapast.ReturnStmt[0]:
			num += resolve_generic_calls(ast, grecvfuncnames, gfuncnames, gfuncset, gcallset, gcallstack, mapast.O(iterator+i))

		case &mapast.IncDecStmt[0]:
			num += resolve_generic_calls(ast, grecvfuncnames, gfuncnames, gfuncset, gcallset, gcallstack, mapast.O(iterator+i))

		case &mapast.VarDefStmt[0]:
			num += resolve_generic_calls(ast, grecvfuncnames, gfuncnames, gfuncset, gcallset, gcallstack, mapast.O(iterator+i))

		case &mapast.AssignStmt[0]:
			num += resolve_generic_calls(ast, grecvfuncnames, gfuncnames, gfuncset, gcallset, gcallstack, mapast.O(iterator+i))

		case &mapast.ClosureExp[0]:
			var saved = gfuncnames[GfuncName{}]
			gfuncnames[GfuncName{}] = iterator + i
			num += resolve_generic_calls(ast, grecvfuncnames, gfuncnames, gfuncset, gcallset, gcallstack, mapast.O(iterator+i))
			gfuncnames[GfuncName{}] = saved

		}
	}
	return num
}

type Callsite struct {
	calls_func   uint64
	inside_func  uint64
	wildctype    uint64
	argsnamedmap *StrStrBinTree
}

func signature_getarg_type(asttree map[uint64][]byte, sig uint64, argno int) uint64 {
	var args = uint64(len(asttree[sig]) - 1)
	var j = 0
	for i := mapast.O(sig); i < mapast.O(sig)+args; i++ {
		var k uint64
		for k = mapast.O(i); &asttree[k][0] != &mapast.RootOfType[0]; k++ {
		}
		if mapast.O(i) == k {
			if j == argno {
				return mapast.O(k)
			}
			j++
		}
		for l := mapast.O(i); l < k; l++ {
			if j == argno {
				return mapast.O(k)
			}
			j++
		}
	}
	return 9999
}

func struct_getarg_type(asttree map[uint64][]byte, structure uint64, argno int) uint64 {
	var j = 0
	for i := mapast.O(structure); len(asttree[i]) > 0; i++ {
		var k uint64
		for k = mapast.O(i); &asttree[k][0] != &mapast.RootOfType[0]; k++ {
		}
		if mapast.O(i) == k {
			if j == argno {
				return mapast.O(k)
			}
			j++
		}
		for l := mapast.O(i); l < k; l++ {
			if j == argno {
				return mapast.O(k)
			}
			j++
		}
	}
	return 9999
}

func func_getarg_type(asttree map[uint64][]byte, function uint64, argno int) uint64 {
	var recv = uint64(len(asttree[function]) - 1)
	var args = uint64(cap(asttree[function]) - 1)
	var j = 0
	for i := mapast.O(function) + recv + 1; i < mapast.O(function)+args+1; i++ {
		var k uint64
		for k = mapast.O(i); &asttree[k][0] != &mapast.RootOfType[0]; k++ {
		}
		for l := mapast.O(i); l < k; l++ {
			if j == argno {
				return k
			}
			j++
		}
		if byte(len(asttree[i])-1) == mapast.TypedIdentEllipsis {
			return k
		}
	}
	return 9999
}

func derive_wildcard(asttree map[uint64][]byte, gtypenames map[string]uint64, funtype uint64, argside_type types.Type, sawmap **StrStrBinTree) (out types.Type) {
	if mapast.Which(asttree[funtype]) == nil {
		ntype := string(asttree[funtype])
		var lookedupgtype = gtypenames[ntype]
		if lookedupgtype == 0 {
			return nil
		}
		strnamed, ok := argside_type.(*types.Named)
		if ok {
			str := strnamed.String()
			if len(str) >= 8 && str[0:8] == "demopkg." {
				str = string(str[8:])
			}
			switch StrStrBinTreeInsert(sawmap, ntype, str) {
			case 2:
				return nil

			case 3:
				panic("type conflict in param to arg mapping, wrong program.\n")

			}
			return derive_wildcard(asttree, gtypenames, mapast.O(mapast.O(lookedupgtype)+1), argside_type.Underlying(), sawmap)
		} else {
			return derive_wildcard(asttree, gtypenames, mapast.O(mapast.O(lookedupgtype)+1), argside_type, sawmap)
		}
	}
	if (len(asttree[funtype]) > 0) && (&asttree[funtype][0] == &mapast.GenericExp[0]) {
		if len(asttree[funtype])-1 == 0 {
			return argside_type
		} else {
			return nil
		}
	}
	switch argside_type.(type) {
	case *types.Pointer:
		if (len(asttree[funtype])-1 == int(mapast.ExpressionMul)) && (&asttree[funtype][0] == &mapast.Expression[0]) && cap(asttree[funtype])-int(mapast.ExpressionTotalCount) == 1 {
			return derive_wildcard(asttree, gtypenames, mapast.O(funtype), argside_type.(*types.Pointer).Elem(), sawmap)
		}

	case *types.Slice:
		if (len(asttree[funtype])-1 == int(mapast.ExpressionSliceType)) && (&asttree[funtype][0] == &mapast.Expression[0]) && cap(asttree[funtype])-int(mapast.ExpressionTotalCount) == 1 {
			return derive_wildcard(asttree, gtypenames, mapast.O(funtype), argside_type.(*types.Slice).Elem(), sawmap)
		}

	case *types.Array:
		if (len(asttree[funtype])-1 == int(mapast.ExpressionArrayType)) && (&asttree[funtype][0] == &mapast.Expression[0]) && cap(asttree[funtype])-int(mapast.ExpressionTotalCount) == 2 {
			return derive_wildcard(asttree, gtypenames, mapast.O(funtype)+1, argside_type.(*types.Array).Elem(), sawmap)
		}

	case *types.Basic:
		if argside_type.(*types.Basic).Kind() == types.UntypedNil {
			return nil
		}

	case *types.Struct:
		for i := 0; i < argside_type.(*types.Struct).NumFields(); i++ {
			arg := argside_type.(*types.Struct).Field(i).Type()
			ret := derive_wildcard(asttree, gtypenames, struct_getarg_type(asttree, funtype, i), arg, sawmap)
			out = check_typeconflict(ret, out, false)
		}
		return

	case *types.Signature:
		for i := 0; i < argside_type.(*types.Signature).Params().Len(); i++ {
			arg := argside_type.(*types.Signature).Params().At(i).Type()
			ret := derive_wildcard(asttree, gtypenames, signature_getarg_type(asttree, funtype, i), arg, sawmap)
			out = check_typeconflict(ret, out, false)
		}
		return

	case *types.Chan:
		if (len(asttree[funtype])-1 == int(mapast.ExpressionChan)+int(argside_type.(*types.Chan).Dir())) && (&asttree[funtype][0] == &mapast.Expression[0]) && cap(asttree[funtype])-int(mapast.ExpressionTotalCount) == 1 {
			return derive_wildcard(asttree, gtypenames, mapast.O(funtype), argside_type.(*types.Chan).Elem(), sawmap)
		}

	case *types.Map:
		key := derive_wildcard(asttree, gtypenames, mapast.O(funtype), argside_type.(*types.Map).Key(), sawmap)
		val := derive_wildcard(asttree, gtypenames, mapast.O(funtype)+1, argside_type.(*types.Map).Elem(), sawmap)
		return check_typeconflict(key, val, false)

	}
	return nil
}

func build_type(asttree map[uint64][]byte, typ types.Type) uint64 {
	for i := uint64(0); i < uint64(0xfffffffffffffff0); i++ {
		if len(asttree[i]) == 0 && len(asttree[i+1]) == 0 && len(asttree[i+2]) == 0 {
			asttree[i+1] = mapast.RootOfType
			if typ != nil {
				construct_type(asttree, mapast.O(i+1), typ)
			}
			return i + 1
		}
	}
	panic("map is full")
}

func construct_func(asttree map[uint64][]byte, where uint64, fun *types.Func) {
	varargs := fun.Type().(*types.Signature).Variadic()
	paramlen := uint64(fun.Type().(*types.Signature).Params().Len())
	asttree[where] = mapast.IfceMethod[0 : 1+int(paramlen)]
	asttree[mapast.O(where)] = mapast.TypedIdent[0 : 1+mapast.TypedIdentNormal]
	asttree[mapast.O(mapast.O(where))] = []byte(fun.Name())
	asttree[mapast.O(mapast.O(where))+1] = mapast.RootOfType
	construct_type(asttree, mapast.O(where)+1, fun.Type().(*types.Signature).Params())
	construct_type(asttree, mapast.O(where)+1+paramlen, fun.Type().(*types.Signature).Results())
	if varargs {
		asttree[mapast.O(where)+1+paramlen-1] = mapast.TypedIdent[0 : 1+mapast.TypedIdentEllipsis]
	}
	return
}

func contains(s string, char byte) bool {
	for i := range s {
		if s[i] == char {
			return true
		}
	}
	return false
}

func construct_type(asttree map[uint64][]byte, where uint64, typ types.Type) {
	switch typ.(type) {
	case *types.Basic:
		if typ.(*types.Basic).Kind() == types.UntypedVoid {
			asttree[where] = mapast.GenericExp[0:1]
			return
		}
		asttree[where] = []byte(typ.(*types.Basic).String())
		return

	case *types.Named:
		str := typ.(*types.Named).String()
		if len(str) >= 8 && str[0:8] == "demopkg." {
			asttree[where] = []byte(str[8:])
		} else {
			asttree[where] = after_last_slash([]byte(str))
		}
		return

	case *types.Pointer:
		asttree[where] = mapast.ExpressionNode(mapast.ExpressionMul, 1)
		construct_type(asttree, mapast.O(where), typ.(*types.Pointer).Elem())
		return

	case *types.Interface:
		asttree[where] = mapast.IfceTypExp
		for i := 0; i < typ.(*types.Interface).NumMethods(); i++ {
			construct_func(asttree, mapast.O(where)+uint64(i), typ.(*types.Interface).Method(i))
		}
		return

	case *types.Array:
		asttree[where] = mapast.ExpressionNode(mapast.ExpressionArrayType, 2)
		asttree[mapast.O(where)] = []byte(fmt.Sprintf("%d", typ.(*types.Array).Len()))
		construct_type(asttree, mapast.O(where)+1, typ.(*types.Array).Elem())
		return

	case *types.Struct:
		asttree[where] = mapast.StructType
		if typ.(*types.Struct).NumFields() == 0 {
			return
		}
		for i := 0; i < typ.(*types.Struct).NumFields(); i++ {
			asttree[mapast.O(where)+uint64(i)] = mapast.TypedIdent[0 : 1+mapast.TypedIdentNormal]
			var fieldanon = typ.(*types.Struct).Field(i).Anonymous()
			var fieldname string
			if !fieldanon {
				fieldname = typ.(*types.Struct).Field(i).Name()
			}
			var fieldis uint64
			if len(fieldname) != 0 {
				asttree[mapast.O(mapast.O(where)+uint64(i))] = []byte(fieldname)
				fieldis = 1
			} else {
			}
			asttree[mapast.O(mapast.O(where)+uint64(i))+fieldis] = mapast.RootOfType
			var tag = typ.(*types.Struct).Tag(i)
			if len(tag) > 0 {
				var escapechar = `"`
				if contains(tag, '"') && !contains(tag, '\\') && !contains(tag, '\b') && !contains(tag, '\t') {
					escapechar = "`"
				}
				if contains(tag, '`') {
					escapechar = `"`
				}
				if escapechar == `"` {
					tag = strings.Replace(tag, "\\", `\\`, -1)
					tag = strings.Replace(tag, "\t", `\t`, -1)
					tag = strings.Replace(tag, "\"", `\"`, -1)
					tag = strings.Replace(tag, "\b", `\b`, -1)
				}
				asttree[mapast.O(mapast.O(where)+uint64(i))+fieldis+1] = []byte(escapechar + tag + escapechar)
			}
			construct_type(asttree, mapast.O(mapast.O(mapast.O(where)+uint64(i))+fieldis), typ.(*types.Struct).Field(i).Type())
		}
		return

	case *types.Chan:
		dir := typ.(*types.Chan).Dir()
		asttree[where] = mapast.ExpressionNode(mapast.ExpressionChan+byte(dir), 1)
		construct_type(asttree, mapast.O(where), typ.(*types.Chan).Elem())
		return

	case *types.Slice:
		asttree[where] = mapast.ExpressionNode(mapast.ExpressionSliceType, 1)
		construct_type(asttree, mapast.O(where), typ.(*types.Slice).Elem())
		return

	case *types.Signature:
		paramlen := uint64(typ.(*types.Signature).Params().Len())
		if typ.(*types.Signature).Recv() == nil {
			varargs := typ.(*types.Signature).Variadic()
			asttree[where] = mapast.ClosureExpNode(paramlen)
			construct_type(asttree, mapast.O(where), typ.(*types.Signature).Params())
			construct_type(asttree, mapast.O(where)+paramlen, typ.(*types.Signature).Results())
			if varargs {
				asttree[mapast.O(where)+paramlen-1] = mapast.TypedIdent[0 : 1+mapast.TypedIdentEllipsis]
			}
			return
		}

	case *types.Tuple:
		if typ.(*types.Tuple) == nil {
			return
		}
		for i := uint64(0); i < uint64(typ.(*types.Tuple).Len()); i++ {
			asttree[where+i] = mapast.TypedIdent[0 : 1+mapast.TypedIdentNormal]
			asttree[mapast.O(where+i)] = mapast.RootOfType
			construct_type(asttree, mapast.O(mapast.O(where+i)), typ.(*types.Tuple).At(int(i)).Type())
		}
		return

	case *types.Map:
		asttree[where] = mapast.ExpressionNode(mapast.ExpressionMap, 2)
		construct_type(asttree, mapast.O(where), typ.(*types.Map).Key())
		construct_type(asttree, mapast.O(where)+1, typ.(*types.Map).Elem())
		return

	}
}

func ast_sha256(asttree map[uint64][]byte, where uint64) (dst []byte) {
	h := sha256.New()
	mapast.Code(func(s string) {
		h.Write([]byte(s))
	}, asttree, where, where)
	return h.Sum(nil)
}

func tree_sha256(tree *StrStrBinTree, init []byte) (dst []byte) {
	h := sha256.New()
	h.Write(init)
	StrStrBinTreeInorder(tree, func(a string, b string) {
		h.Write([]byte(a))
		h.Write([]byte(" "))
		h.Write([]byte(b))
		h.Write([]byte(";"))
	})
	return h.Sum(nil)
}

func b32_to_5_u64(n uint64, x []byte) (dst [5]uint64) {
	dst[0] = n
	dst[1] = uint64(x[7]) | uint64(x[6])<<8 | uint64(x[5])<<16 | uint64(x[4])<<24 | uint64(x[3])<<32 | uint64(x[2])<<40 | uint64(x[1])<<48 | uint64(x[0])<<56
	dst[2] = uint64(x[15]) | uint64(x[14])<<8 | uint64(x[13])<<16 | uint64(x[12])<<24 | uint64(x[11])<<32 | uint64(x[10])<<40 | uint64(x[9])<<48 | uint64(x[8])<<56
	dst[3] = uint64(x[23]) | uint64(x[22])<<8 | uint64(x[21])<<16 | uint64(x[20])<<24 | uint64(x[19])<<32 | uint64(x[18])<<40 | uint64(x[17])<<48 | uint64(x[16])<<56
	dst[4] = uint64(x[31]) | uint64(x[30])<<8 | uint64(x[29])<<16 | uint64(x[28])<<24 | uint64(x[27])<<32 | uint64(x[26])<<40 | uint64(x[25])<<48 | uint64(x[24])<<56
	return dst
}

func b32_to_GfuncName(n string, x []byte) (dst GfuncName) {
	dst.name = n
	y := b32_to_5_u64(0, x)
	dst.rhash[0] = y[1]
	dst.rhash[1] = y[2]
	dst.rhash[2] = y[3]
	dst.rhash[3] = y[4]
	return dst
}

func seekfuncslot(asttree map[uint64][]byte) (src uint64) {
	src = mapast.O(mapast.O(0))
	var ok bool
	for _, ok = asttree[src]; ok; _, ok = asttree[src] {
		src++
	}
	return src
}

func copypasta(asttree map[uint64][]byte, dst uint64, src uint64) {
	asttree[dst] = asttree[src]
	dst = mapast.O(dst)
	src = mapast.O(src)
	var ok bool
	for _, ok = asttree[src]; ok; _, ok = asttree[src] {
		copypasta(asttree, dst, src)
		dst++
		src++
	}
}

func erase(asttree map[uint64][]byte, what uint64) {
	delete(asttree, what)
	what = mapast.O(what)
	var ok bool
	for _, ok = asttree[what]; ok; _, ok = asttree[what] {
		erase(asttree, what)
		what++
	}
}

func specialize(asttree map[uint64][]byte, root uint64, typ uint64) {
	root = mapast.O(root)
	var ok bool
	for _, ok = asttree[root]; ok; _, ok = asttree[root] {
		if len(asttree[root]) > 0 && &asttree[root][0] == &mapast.GenericExp[0] && len(asttree[root])-1 == 0 {
			copypasta(asttree, root, typ)
		} else {
			specialize(asttree, root, typ)
		}
		root++
	}
}

func replicate(asttree map[uint64][]byte, dst uint64, src uint64, gcallset map[uint64]*Callsite, inside uint64, wildcard uint64, heretree *StrStrBinTree) {
	dst = mapast.O(dst)
	src = mapast.O(src)
	var ok bool
	for _, ok = asttree[src]; ok; _, ok = asttree[src] {
		var sr = gcallset[src]
		if sr != nil {
			var cs = &Callsite{}
			cs.calls_func = sr.calls_func
			cs.inside_func = inside
			cs.wildctype = build_type(asttree, nil)
			StrStrBinTreePreorder(sr.argsnamedmap, func(a, b string) {
				StrStrBinTreeInsert(&cs.argsnamedmap, a, b)
			})
			StrStrBinTreePreorder(heretree, func(a, b string) {
				StrStrBinTreeInsert(&cs.argsnamedmap, a, b)
			})
			copypasta(asttree, cs.wildctype, sr.wildctype)
			specialize(asttree, cs.wildctype, wildcard)
			gcallset[dst] = cs
		}
		replicate(asttree, dst, src, gcallset, inside, wildcard, heretree)
		dst++
		src++
	}
}

func mutate_type(asttree map[uint64][]byte, root uint64, sawmap *StrStrBinTree) {
	root = mapast.O(root)
	var ok bool
	for _, ok = asttree[root]; ok; _, ok = asttree[root] {
		if mapast.Which(asttree[root]) == nil {
			var translated = StrStrBinTreeSelect(sawmap, string(asttree[root]))
			if len(translated) > 0 {
				asttree[root] = []byte(translated)
			}
		}
		mutate_type(asttree, root, sawmap)
		root++
	}
}

func mutate(asttree map[uint64][]byte, root uint64, sawmap *StrStrBinTree) {
	root = mapast.O(root)
	var ok bool
	for _, ok = asttree[root]; ok; _, ok = asttree[root] {
		if len(asttree[root]) > 0 && &asttree[root][0] == &mapast.RootOfType[0] {
			mutate_type(asttree, root, sawmap)
		} else {
			mutate(asttree, root, sawmap)
		}
		root++
	}
}

type GfuncName struct {
	name  string
	rhash [4]uint64
}

func after_last_slash(b []byte) []byte {
	var i int
	for i = len(b) - 1; i >= 0; i-- {
		if b[i] == '/' {
			i++
			break
		}
	}
	if i == -1 {
		return b
	}
	return b[i:]
}

func main2(asttree map[uint64][]byte, fset *token.FileSet, files []*ast.File, ignore_err bool) ([]string, error) {
	gtypenames := make(map[string]uint64)
	gtypeset := make(map[uint64]struct{})
	_ = gtypenames
	_ = gtypeset
	gfuncnames := make(map[GfuncName]uint64)
	grecvfuncnames := make(map[string]struct{})
	gfuncset := make(map[uint64]byte)
	_ = gfuncnames
	_ = gfuncset
	gcallset := make(map[uint64]*Callsite)
	_ = gcallset
	gcallstack := make([]uint64, 0)
	for resolve_toplevel_generic_types(asttree, gtypenames, gtypeset, 0) > 0 {
	}
	for resolve_toplevel_generic_funcs(asttree, gtypenames, gtypeset, grecvfuncnames, gfuncnames, gfuncset, 0) > 0 {
	}
	errors := check_gentype_banned(asttree, 0, gtypenames, gfuncset)
	if len(errors) > 0 {
		return errors, nil
	}
	for resolve_generic_calls(asttree, grecvfuncnames, gfuncnames, gfuncset, gcallset, &gcallstack, 0) > 0 {
	}
	conf := types.Config{Importer: importer.Default()}
	info := &types.Info{Types: make(map[ast.Expr]types.TypeAndValue)}
	_, err := conf.Check("demopkg", fset, files, info)
	if err != nil {
		return nil, err
	}
	var gcallstack_i = 0
	for fileid := 0; fileid < len(files); fileid++ {
		ast.Inspect(files[fileid], func(n ast.Node) bool {
			if gcallstack_i > len(gcallstack) {
				return true
			}
			if cexpr, ok := n.(*ast.CallExpr); ok {
				var current GfuncName
				var funcname, ok2 = cexpr.Fun.(*ast.Ident)
				if !ok2 {
					selector, ok3 := cexpr.Fun.(*ast.SelectorExpr)
					if !ok3 {
						return true
					}
					funcname, ok2 = selector.Sel, ok3
					if _, ok4 := grecvfuncnames[funcname.Name]; ok4 {
						scratchpad := make(map[uint64][]byte)
						scratchpad[0] = mapast.RootOfType
						typ := info.Types[selector.X].Type
						construct_type(scratchpad, mapast.O(0), typ)
						current = b32_to_GfuncName(funcname.Name, ast_sha256(scratchpad, 0))
						scratchpad = nil
					}
				} else {
					current.name = funcname.Name
				}
				if ok2 {
					if gfuncnames[current] != 0 {
						var current_call_id = gcallstack[gcallstack_i]
						var currentcallsite = gcallset[current_call_id]
						if currentcallsite.calls_func == 0 {
							currentcallsite.calls_func = gfuncnames[current]
						}
						var novarargs = int(gfuncset[currentcallsite.calls_func]) / 2
						if false {
							mapast.PrintCode(asttree, current_call_id, current_call_id)
						}
						var dotdotdot = func_getarg_type(asttree, currentcallsite.calls_func, 9999) != 9999
						var passdotdotdot = len(asttree[current_call_id])-1 == int(mapast.ExpressionCallDotDotDot)
						var wildcard, wcard types.Type
						_ = wildcard
						var sawmap *StrStrBinTree
					outer:
						for argnum, expr := range cexpr.Args {
							if tv, ok := info.Types[expr]; ok {
								var islastarg = dotdotdot && argnum >= novarargs
								_ = islastarg
								switch tv.Type.(type) {
								case *types.Tuple:
									if len(cexpr.Args) == 1 {
										for i := 0; i < tv.Type.(*types.Tuple).Len(); i++ {
											var islastarg = dotdotdot && i >= novarargs
											_ = islastarg
											under := tv.Type.(*types.Tuple).At(i).Type()
											wcard = derive_wildcard(asttree, gtypenames, mapast.O(func_getarg_type(asttree, currentcallsite.calls_func, i)), under, &sawmap)
											if islastarg {
												if passdotdotdot {
													wildcard = check_typeconflict(wcard.(*types.Slice).Elem(), wildcard, true)
												} else {
													wildcard = check_typeconflict(wcard, wildcard, true)
												}
											} else {
												wildcard = check_typeconflict(wcard, wildcard, false)
											}
										}
										break outer
									}

								}
								if tv.Value != nil {
								}
								wcard = derive_wildcard(asttree, gtypenames, mapast.O(func_getarg_type(asttree, currentcallsite.calls_func, argnum)), tv.Type, &sawmap)
								if islastarg {
									if passdotdotdot {
										wildcard = check_typeconflict(wcard.(*types.Slice).Elem(), wildcard, true)
									} else {
										wildcard = check_typeconflict(wcard, wildcard, true)
									}
								} else {
									wildcard = check_typeconflict(wcard, wildcard, false)
								}
							}
						}
						if wildcard == nil {
							if !ignore_err {
								panic("Undetermined wildcard")
							}
						} else {
						}
						currentcallsite.wildctype = build_type(asttree, wildcard)
						currentcallsite.argsnamedmap = sawmap
						gcallstack_i++
					} else if current.rhash != [4]uint64{} {
						gcallstack_i++
					}
				}
			}
			return true
		})
	}
	instantiated := make(map[[5]uint64]struct{})
outer:
	for infiniteloop := 0; infiniteloop < 20000; infiniteloop++ {
		for k, v := range gcallset {
			if _, ok2 := gfuncset[v.inside_func]; ok2 {
				continue
			}
			if v.calls_func == 0 {
				continue
			}
			checksum := b32_to_5_u64(v.calls_func, tree_sha256(v.argsnamedmap, ast_sha256(asttree, v.wildctype)))
			if _, ok := instantiated[checksum]; ok {
				if (gfuncset[v.calls_func] & 1) != 0 {
					var name = asttree[mapast.O(mapast.O(k))+1]
					name = append(name, fmt.Sprintf("%016X", checksum[1])...)
					asttree[mapast.O(mapast.O(k))+1] = name
				} else {
					var name = asttree[mapast.O(k)]
					name = append(name, fmt.Sprintf("%016X", checksum[1])...)
					asttree[mapast.O(k)] = name
				}
				delete(gcallset, k)
				continue outer
			}
			instantiated[checksum] = struct{}{}
			slot := seekfuncslot(asttree)
			copypasta(asttree, slot, v.calls_func)
			replicate(asttree, slot, v.calls_func, gcallset, slot, v.wildctype, v.argsnamedmap)
			var name = asttree[mapast.O(slot)]
			name = append(name, fmt.Sprintf("%016X", checksum[1])...)
			asttree[mapast.O(slot)] = name
			specialize(asttree, slot, v.wildctype)
			mutate(asttree, slot, v.argsnamedmap)
			continue outer
		}
		break outer
	}
	for k := range gfuncset {
		erase(asttree, k)
		asttree[k] = nil
	}
	for k := range gtypeset {
		erase(asttree, k)
		asttree[k] = nil
	}
	return nil, nil
}

func process(content []byte) (output []byte) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "demo", content, parser.ParseComments)
	if err != nil {
		return []byte(`["` + err.Error() + `"]`)
	}
	asttree := make(map[uint64][]byte)
	asttree[0] = mapast.RootMatter
	asttree[mapast.O(0)] = mapast.FileMatter
	ender := make(map[int]struct{})
	separ := make(map[int]struct{})
	var endersepar = [2]map[int]struct{}{ender, separ}
	mapast.LookupComments(content, endersepar)
	ast.Walk(&Conversion{AstTree: asttree, MyFile: mapast.O(0), EnderSepared: endersepar, Comments1: true}, file)
	errors, err := main2(asttree, fset, []*ast.File{file}, false)
	if len(errors) > 0 {
		var msg = []byte(`["Generic types cannot be used in scope of: `)
		for i := range errors {
			msg = append(msg, []byte(errors[i]+", ")...)
		}
		msg = msg[0 : len(msg)-2]
		msg = append(msg, []byte(`"]`)...)
		return msg
	}
	if err != nil {
		return []byte(`["` + err.Error() + `"]`)
	}
	mapast.Code(func(s string) {
		if len(s) == 0 {
			output = append(output, '\n')
		} else {
			output = append(output, []byte(s)...)
		}
	}, asttree, 0, 0)
	return output
}

func main() {
	if js.Global != nil {
		js.Global.Set("Process", map[string]interface{}{"process": process})
	} else {
		var filename string
		var pakg_dir string
		var pakg_out string
		var ignore_err bool
		flag.StringVar(&filename, "I", "", "go source code file to translate")
		flag.StringVar(&pakg_dir, "P", "", "go source code package directory to translate")
		flag.StringVar(&pakg_out, "T", "", "go source code package target directory")
		flag.BoolVar(&ignore_err, "i", false, "ignore typechecker errors")
		flag.Parse()
		if len(filename) > 0 {
			content, err := ioutil.ReadFile(filename)
			if err != nil {
				panic(err)
			}
			var ret = process(content)
			if len(ret) > 0 && ret[0] == '[' {
				fmt.Println(string(ret))
				os.Exit(3)
			}
			print(string(ret))
			return
		}
		if len(pakg_dir)*len(pakg_out) == 0 {
			fmt.Println("Please provide both input package directory (-P) and output directory (-T)")
			os.Exit(4)
		}
		var files []string
		filez, err := ioutil.ReadDir(pakg_dir)
		if err != nil {
			panic("Cannot read directory")
		}
		for _, file := range filez {
			if !file.IsDir() {
				if filepath.Ext(file.Name()) == ".go" {
					files = append(files, file.Name())
				}
			}
		}
		fset := token.NewFileSet()
		astfiles := make([]*ast.File, len(files))
		asttree := make(map[uint64][]byte)
		asttree[0] = mapast.RootMatter
		for i := range files {
			content, err := ioutil.ReadFile(pakg_dir + "/" + files[i])
			if err != nil {
				panic(err)
			}
			asttree[mapast.O(0)+uint64(i)] = mapast.FileMatter
			ender := make(map[int]struct{})
			separ := make(map[int]struct{})
			var endersepar = [2]map[int]struct{}{ender, separ}
			mapast.LookupComments(content, endersepar)
			file, err := parser.ParseFile(fset, "demo", content, parser.ParseComments)
			if err != nil {
				panic(err)
			}
			ast.Walk(&Conversion{AstTree: asttree, MyFile: mapast.O(0) + uint64(i), EnderSepared: endersepar, Comments1: true}, file)
			astfiles[i] = file
		}
		errors, err1 := main2(asttree, fset, astfiles, ignore_err)
		if len(errors) > 0 {
			for i := range errors {
				println(string(errors[i]))
			}
			panic("Illegal generic type usage occured")
		}
		if err1 != nil && !ignore_err {
			println(string(err1.Error()))
			panic("Transpile error occured")
		}
		for i := range files {
			f, err := os.OpenFile(pakg_out+"/"+files[i], os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
			if err != nil {
				panic("Cannot open File")
			}
			mapast.Code(func(s string) {
				if len(s) == 0 {
					n, err := f.Write([]byte("\n"))
					if err != nil && n < 1 {
						f.Close()
						panic("Cannot write File")
					}
				} else {
					n, err := f.Write([]byte(s))
					if err != nil && n < len(s) {
						f.Close()
						panic("Cannot write File")
					}
				}
			}, asttree, mapast.O(0)+uint64(i), mapast.O(0)+uint64(i))
			err = f.Close()
			if err != nil {
				panic("Cannot close File")
			}
		}
	}
}
