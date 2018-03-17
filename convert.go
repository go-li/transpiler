// Package convert helps to convert go/ast to mapast
package main

import (
	"github.com/go-li/mapast"
	"github.com/go-li/transpiler/ast"
	"go/token"
)

func o(n uint64) uint64 {
	return mapast.O(n)
}

func bool2byte(s bool) byte {
	if s {
		return 1
	}
	return 0
}

func how_many_substmts_labeled_stmt(list *ast.LabeledStmt) int {
	var n = 0
	switch list.Stmt.(type) {
	case *ast.EmptyStmt:
		n++

	case *ast.BranchStmt:
		n++

	case *ast.ExprStmt:
		n++

	case *ast.ReturnStmt:
		n++

	case *ast.IncDecStmt:
		n++

	case *ast.GoStmt:
		n++

	case *ast.DeferStmt:
		n++

	case *ast.AssignStmt:
		n++

	case *ast.DeclStmt:
		var xx = list.Stmt.(*ast.DeclStmt)
		if xx.Decl.(*ast.GenDecl).Tok == token.TYPE {
			n += (len(xx.Decl.(*ast.GenDecl).Specs))
		} else {
			n++
		}

	case *ast.SendStmt:
		n++

	case *ast.LabeledStmt:
		n += how_many_substmts_labeled_stmt(list.Stmt.(*ast.LabeledStmt))
		n++

	}
	return n
}

func how_many_substmts_stmt_list(list []ast.Stmt) int {
	var n = 0
	for i := range list {
		switch list[i].(type) {
		case *ast.EmptyStmt:
			n++

		case *ast.BranchStmt:
			n++

		case *ast.ExprStmt:
			n++

		case *ast.ReturnStmt:
			n++

		case *ast.IncDecStmt:
			n++

		case *ast.GoStmt:
			n++

		case *ast.DeferStmt:
			n++

		case *ast.AssignStmt:
			n++

		case *ast.DeclStmt:
			var xx = list[i].(*ast.DeclStmt)
			if xx.Decl.(*ast.GenDecl).Tok == token.TYPE {
				n += (len(xx.Decl.(*ast.GenDecl).Specs))
			} else {
				n++
			}

		case *ast.SendStmt:
			n++

		case *ast.LabeledStmt:
			n += how_many_substmts_labeled_stmt(list[i].(*ast.LabeledStmt))
			n++

		}
	}
	return n
}

func how_many_substmts_block(x *ast.BlockStmt) int {
	return how_many_substmts_stmt_list(x.List)
}

func how_many_subblocks_labeled_stmt(list *ast.LabeledStmt) int {
	var n = 0
	switch list.Stmt.(type) {
	case *ast.IfStmt:
		xx := list.Stmt.(*ast.IfStmt)
		var ok = true
		for ok && xx.Else != nil {
			switch xx.Else.(type) {
			case *ast.IfStmt:
				xx = xx.Else.(*ast.IfStmt)
				ok = true

			case *ast.BlockStmt:
				n--
				ok = false

			}
			n++
		}
		n++

	case *ast.ForStmt:
		n++

	case *ast.RangeStmt:
		n++

	case *ast.SwitchStmt:
		n++

	case *ast.CaseClause:
		n++

	case *ast.TypeSwitchStmt:
		n++

	case *ast.BlockStmt:
		n++

	case *ast.SelectStmt:
		n++

	case *ast.CommClause:
		n++

	case *ast.LabeledStmt:
		n += how_many_subblocks_labeled_stmt(list.Stmt.(*ast.LabeledStmt))

	}
	return n
}

func how_many_subblocks_stmt_list(list []ast.Stmt) int {
	var n = 0
	for i := range list {
		switch list[i].(type) {
		case *ast.IfStmt:
			xx := list[i].(*ast.IfStmt)
			var ok = true
			for ok && xx.Else != nil {
				switch xx.Else.(type) {
				case *ast.IfStmt:
					xx = xx.Else.(*ast.IfStmt)
					ok = true

				case *ast.BlockStmt:
					n--
					ok = false

				}
				n++
			}
			n++

		case *ast.ForStmt:
			n++

		case *ast.RangeStmt:
			n++

		case *ast.SwitchStmt:
			n++

		case *ast.CaseClause:
			n++

		case *ast.TypeSwitchStmt:
			n++

		case *ast.BlockStmt:
			n++

		case *ast.SelectStmt:
			n++

		case *ast.CommClause:
			n++

		case *ast.LabeledStmt:
			n += how_many_subblocks_labeled_stmt(list[i].(*ast.LabeledStmt))

		default:

		}
	}
	return n
}

func how_many_subblocks_block(x *ast.BlockStmt) int {
	return how_many_subblocks_stmt_list(x.List)
}

func coolcomment(n string) bool {
	if len(n) >= 9 && n[0:9] == "// +build" {
		return true
	}
	if len(n) >= 5 && n[0:5] == "//go:" {
		return true
	}
	if len(n) >= 6 && n[0:6] == "//line" {
		return true
	}
	if len(n) >= 8 && n[0:8] == "//extern" {
		return true
	}
	if len(n) >= 9 && n[0:9] == "// import" {
		return true
	}
	if len(n) >= 8 && n[0:8] == "//export" {
		return true
	}
	return false
}

// NewConversion creates a new conversion for a source file. Whichfile is the
// number of file to be translated, starting from zero. File is the slice
// filled with the source code, used to scan for comments info.
func NewConversion(asttree map[uint64][]byte, whichfile uint64, file []byte) *Conversion {
	ender := make(map[int]struct{})
	separ := make(map[int]struct{})
	var endersepar = [2]map[int]struct{}{ender, separ}
	mapast.LookupComments(file, endersepar)
	asttree[0] = mapast.RootMatter
	asttree[o(0)+whichfile] = mapast.FileMatter
	return &Conversion{AstTree: asttree, MyFile: o(0), EnderSepared: endersepar, Comments1: true}
}

// Conversion holds the state of translation of a single file. Please put your
// ast tree map to AstTree field and the key of your file to the MyFile field.
// Conversion is usually not reused.
type Conversion struct {
	AstTree            map[uint64][]byte
	MyFile             uint64
	EnderSepared       [2]map[int]struct{}
	Comments1          bool
	importswhere       uint64
	nestedimports      uint64
	structfield        [][2]uint64
	typefield          []uint64
	nowblock           []uint64
	subblocks          []int
	substmts           []int
	skippedassignments byte
	skippedexpressions byte
	skippedincdecs     byte
	skippedsends       byte
	skippedellipsis    byte
	deadif             map[*ast.IfStmt]struct{}
	deadfunc           map[*ast.FuncType]struct{}
	skippedbalits      map[*ast.BasicLit]struct{}
	blocksstmts        map[*ast.BlockStmt]uint64
	ifblocks           map[*ast.BlockStmt]uint64
	deadassignments    map[*ast.AssignStmt]struct{}
	deadsends          map[*ast.SendStmt]struct{}
	deadincdecs        map[*ast.IncDecStmt]struct{}
	deadexprs          map[*ast.ExprStmt]struct{}
	typedcases         map[*ast.CaseClause]struct{}
	comments           []string
	commentpos         []int
}

func packint(n int, ender bool, separ bool) int {
	if ender {
		n |= 1 << 29
	}
	if separ {
		n |= 1 << 30
	}
	return n
}

func fetchvariant(n int) byte {
	if n&(1<<29) != 0 {
		return mapast.CommentRowEnder
	}
	if n&(1<<30) != 0 {
		return mapast.CommentRowSeparate
	}
	return mapast.CommentRowNormal
}

// Visit is the main function used to translate go/ast to mapast. Visit is not
// called directly, but instead the Conversion is passed to the ast.Walk call.
func (c *Conversion) Visit(x ast.Node) ast.Visitor {
	switch x.(type) {
	case *ast.File:
		var xx = (x).(*ast.File)
		var imp = int(xx.Package)
		if len(xx.Imports) > 0 {
			imp = int(xx.Imports[0].Path.ValuePos)
		}
		var n = ((x).(*ast.File)).Name.Name
		var pk = int(xx.Package)
		for i := range xx.Comments {
			for j := range xx.Comments[i].List {
				var ctext = xx.Comments[i].List[j].Text
				var sl = int(xx.Comments[i].List[j].Slash)
				var ender bool
				var separ bool
				if c.EnderSepared[0] != nil {
					_, ender1 := c.EnderSepared[0][sl/2]
					_, ender2 := c.EnderSepared[0][(sl+1)/2]
					ender = ender1 || ender2
				}
				if c.EnderSepared[1] != nil {
					_, separ1 := c.EnderSepared[1][sl/2]
					_, separ2 := c.EnderSepared[1][(sl+1)/2]
					separ = separ1 || separ2
				}
				var variant = mapast.CommentRowNormal
				if ender {
					variant = mapast.CommentRowEnder
				} else if separ {
					variant = mapast.CommentRowSeparate
				}
				if sl > pk {
					for k := range c.commentpos {
						c.AstTree[o(c.MyFile)+c.importswhere] = mapast.CommentRow[0 : 1+fetchvariant(c.commentpos[k])]
						c.AstTree[o(o(c.MyFile)+c.importswhere)] = []byte(c.comments[k])
						c.importswhere++
					}
					c.commentpos = c.commentpos[0:0]
					c.comments = c.comments[0:0]
					var separ bool
					if c.EnderSepared[1] != nil {
						_, separ1 := c.EnderSepared[1][pk/2]
						_, separ2 := c.EnderSepared[1][(pk+1)/2]
						separ = separ1 || separ2
					}
					var variant = mapast.PackageDefNormal
					if separ {
						variant = mapast.PackageDefSeparate
					}
					c.AstTree[o(c.MyFile)+c.importswhere] = mapast.PackageDef[0 : 1+variant]
					c.AstTree[o(o(c.MyFile)+c.importswhere)] = []byte(n)
					pk = 0xffffff
					c.importswhere++
				}
				if sl < imp {
					c.AstTree[o(c.MyFile)+c.importswhere] = mapast.CommentRow[0 : 1+variant]
					c.AstTree[o(o(c.MyFile)+c.importswhere)] = []byte(ctext)
					c.importswhere++
				} else {
					c.comments = append(c.comments, ctext)
					c.commentpos = append(c.commentpos, packint(sl, ender, separ))
				}
			}
		}
		if int(pk) != 0xffffff {
			var separ bool
			if c.EnderSepared[1] != nil {
				_, separ1 := c.EnderSepared[1][pk/2]
				_, separ2 := c.EnderSepared[1][(pk+1)/2]
				separ = separ1 || separ2
			}
			var variant = mapast.PackageDefNormal
			if separ {
				variant = mapast.PackageDefSeparate
			}
			c.AstTree[o(c.MyFile)+c.importswhere] = mapast.PackageDef[0 : 1+variant]
			c.AstTree[o(o(c.MyFile)+c.importswhere)] = []byte(((x).(*ast.File)).Name.Name)
			c.importswhere++
		}
		c.comments = append(c.comments, "")
		c.commentpos = append(c.commentpos, 0xfffffff)
		c.deadfunc = make(map[*ast.FuncType]struct{})
		c.skippedbalits = make(map[*ast.BasicLit]struct{})
		c.blocksstmts = make(map[*ast.BlockStmt]uint64)
		c.ifblocks = make(map[*ast.BlockStmt]uint64)

	case *ast.GenDecl:
		var xx = (x).(*ast.GenDecl)
		for (c.commentpos[0] & 0xfffffff) < int(xx.TokPos) {
			c.AstTree[o(c.MyFile)+c.importswhere] = mapast.CommentRow[0 : 1+fetchvariant(c.commentpos[0])]
			c.AstTree[o(o(c.MyFile)+c.importswhere)] = []byte(c.comments[0])
			c.importswhere++
			c.commentpos = c.commentpos[1:]
			c.comments = c.comments[1:]
		}
		if xx.Tok == token.IMPORT {
			if xx.Lparen == 0 {
				c.importswhere++
				c.nestedimports = 0
			} else {
				c.AstTree[o(c.MyFile)+c.importswhere] = mapast.ImportsDef
				c.importswhere++
				c.nestedimports = 1
			}
		}
		if (xx.Tok == token.VAR) || (xx.Tok == token.CONST) {
			var variant byte = bool2byte(xx.Tok == token.CONST)
			_ = variant
			for len(c.substmts) > 0 && len(c.subblocks) > 0 && c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
				c.substmts = c.substmts[0 : len(c.substmts)-1]
				c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
				c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
			}
			if len(c.substmts) > 0 {
				c.substmts[len(c.substmts)-1]--
			}
			var stack []uint64
			var blk uint64
			if len(c.nowblock) == 0 {
				blk = o(c.MyFile) + c.importswhere
				c.importswhere++
			} else {
				blk = c.nowblock[len(c.nowblock)-1]
				c.nowblock[len(c.nowblock)-1]++
			}
			c.AstTree[blk] = mapast.VarDefStmtNode(variant)
			for i := 0; i < len(xx.Specs); i++ {
				xxx := xx.Specs[i].(*ast.ValueSpec)
				var names = uint64(len(xxx.Names))
				var types = uint64(bool2byte(xxx.Type != nil))
				var variant byte = mapast.AssignStmtMoreEqual
				if len(xxx.Values) == len(xxx.Names) {
					variant = mapast.AssignStmtEqual
				} else if len(xxx.Values) == 0 {
					if types == 0 {
						variant = mapast.AssignStmtIotaIsLast
					} else {
						variant = mapast.AssignStmtTypeIsLast
					}
				} else if len(xxx.Values) != 1 {
					panic("multiple values.")
				}
				c.AstTree[o(blk)+uint64(i)] = mapast.AssignStmtNode(variant, names+types+uint64(len(xxx.Values)))
				for j := range xxx.Names {
					c.AstTree[o(o(blk)+uint64(i))+uint64(j)] = []byte(xxx.Names[j].Name)
				}
				if xxx.Type != nil {
					id, ok := xxx.Type.(*ast.Ident)
					var ident []byte
					if ok {
						ident = []byte(id.Name)
					}
					c.AstTree[o(o(blk)+uint64(i))+names] = mapast.RootOfType
					if ok {
						c.AstTree[o(o(o(blk)+uint64(i))+names)] = ident
					} else {
						stack = append([]uint64{o(o(o(blk)+uint64(i)) + names)}, stack...)
					}
				}
				for j := range xxx.Values {
					id, ok := xxx.Values[j].(*ast.Ident)
					var ident []byte
					if ok {
						ident = []byte(id.Name)
					}
					if ok {
						c.AstTree[o(o(blk)+uint64(i))+uint64(j)+names+types] = ident
					} else {
						stack = append([]uint64{o(o(blk)+uint64(i)) + uint64(j) + names + types}, stack...)
					}
				}
			}
			c.typefield = append(c.typefield, stack...)
		}

	case *ast.ImportSpec:
		var where uint64
		if c.nestedimports >= 1 {
			where = o(o(c.MyFile)+c.importswhere-1) + c.nestedimports - 1
			c.nestedimports++
		} else {
			where = o(c.MyFile) + c.importswhere - 1
		}
		c.AstTree[where] = mapast.ImportStmt
		var p []byte
		p = []byte(((x).(*ast.ImportSpec)).Path.Value)
		var n []byte
		if (x).(*ast.ImportSpec).Name != nil {
			n = p
			p = []byte(((x).(*ast.ImportSpec)).Name.Name)
		}
		c.AstTree[o(where)] = p
		if n != nil {
			c.AstTree[o(where)+1] = n
		}

	case *ast.FuncDecl:
		var xx = (x).(*ast.FuncDecl)
		for (c.commentpos[0] & 0xfffffff) < int(xx.Type.Func) {
			if coolcomment(c.comments[0]) || c.Comments1 {
				c.AstTree[o(c.MyFile)+c.importswhere] = mapast.CommentRow[0 : 1+fetchvariant(c.commentpos[0])]
				c.AstTree[o(o(c.MyFile)+c.importswhere)] = []byte(c.comments[0])
				c.importswhere++
			}
			c.commentpos = c.commentpos[1:]
			c.comments = c.comments[1:]
		}
		var recv_count = uint64(bool2byte(xx.Recv != nil))
		var argument_count uint64 = 0
		var result_count uint64 = 0
		if xx.Type.Params != nil {
			argument_count = uint64(len(xx.Type.Params.List))
		}
		if xx.Type.Results != nil {
			result_count = uint64(len(xx.Type.Results.List))
		}
		_ = result_count
		var totalparams = uint64(argument_count + result_count + recv_count)
		var where uint64
		where = o(c.MyFile) + c.importswhere
		c.importswhere++
		c.AstTree[where] = mapast.ToplevFuncNode(recv_count > 0, argument_count)
		c.AstTree[o(where)] = []byte(xx.Name.Name)
		c.structfield = append(c.structfield, [2]uint64{o(where) + 1, totalparams})
		c.deadif = make(map[*ast.IfStmt]struct{})
		c.deadassignments = make(map[*ast.AssignStmt]struct{})
		c.deadsends = make(map[*ast.SendStmt]struct{})
		c.deadincdecs = make(map[*ast.IncDecStmt]struct{})
		c.deadexprs = make(map[*ast.ExprStmt]struct{})
		c.typedcases = make(map[*ast.CaseClause]struct{})
		if xx.Body != nil {
			c.AstTree[o(where)+totalparams+1] = mapast.BlocOfCodeNode(mapast.BlocOfCodePlain, 0)
			c.typefield = []uint64{}
			c.blocksstmts[xx.Body] = o(o(where) + totalparams + 1)
			c.nowblock = []uint64{o(o(where) + totalparams + 1)}
			c.subblocks = []int{how_many_subblocks_block(xx.Body)}
			c.substmts = []int{how_many_substmts_block(xx.Body)}
		}
		c.deadfunc[xx.Type] = struct{}{}

	case *ast.TypeSpec:
		var xx = (x).(*ast.TypeSpec)
		for len(c.substmts) > 0 && len(c.subblocks) > 0 && c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		if len(c.substmts) > 0 {
			c.substmts[len(c.substmts)-1]--
		}
		var where uint64
		if len(c.nowblock) == 0 {
			where = o(c.MyFile) + c.importswhere
			c.importswhere++
		} else {
			where = c.nowblock[len(c.nowblock)-1]
			c.nowblock[len(c.nowblock)-1]++
		}
		var variant byte
		if int(xx.Assign) == 0 {
			variant = mapast.TypDefStmtNormal
		} else {
			variant = mapast.TypDefStmtAlias
		}
		c.AstTree[where] = mapast.TypDefStmtNode(variant)
		c.AstTree[o(where)] = []byte(xx.Name.Name)
		c.AstTree[o(where)+1] = mapast.RootOfType
		switch xxx := xx.Type.(type) {
		case *ast.Ident:
			c.AstTree[o(o(where)+1)] = []byte(xxx.Name)

		default:
			c.typefield = append(c.typefield, o(o(where)+1))

		}

	case *ast.Field:
		var xx = (x).(*ast.Field)
		if len(c.structfield) == 0 {
			break
		}
		if c.structfield[len(c.structfield)-1][1] == 0 {
			c.structfield = c.structfield[0 : len(c.structfield)-1]
			break
		}
		var variant byte = mapast.TypedIdentNormal
		if xx.Tag != nil {
			variant = mapast.TypedIdentTagged
		}
		var t = c.structfield[len(c.structfield)-1][0]
		for i := uint64(0); i < uint64(len(xx.Names)); i++ {
			c.AstTree[o(t)+i] = []byte(xx.Names[i].Name)
		}
		c.AstTree[o(t)+uint64(len(xx.Names))] = mapast.RootOfType
		switch yyy := xx.Type.(type) {
		case *ast.Ellipsis:
			c.skippedellipsis++
			variant = mapast.TypedIdentEllipsis
			switch xxx := yyy.Elt.(type) {
			case *ast.Ident:
				c.AstTree[o(o(t)+uint64(len(xx.Names)))] = []byte(xxx.Name)

			default:
				c.typefield = append(c.typefield, o(o(t)+uint64(len(xx.Names))))

			}

		case *ast.Ident:
			c.AstTree[o(o(t)+uint64(len(xx.Names)))] = []byte(yyy.Name)

		case *ast.FuncType:
			_, ok := c.deadfunc[yyy]
			if !ok {
				c.typefield = append(c.typefield, o(o(t)+uint64(len(xx.Names))))
			}

		default:
			c.typefield = append(c.typefield, o(o(t)+uint64(len(xx.Names))))

		}
		if xx.Tag != nil {
			c.skippedbalits[xx.Tag] = struct{}{}
			c.AstTree[(o(t) + 1 + uint64(len(xx.Names)))] = []byte(xx.Tag.Value)
		}
		c.AstTree[t] = mapast.TypedIdent[0 : 1+variant]
		c.structfield[len(c.structfield)-1][0]++
		if c.structfield[len(c.structfield)-1][1] != 0 {
			c.structfield[len(c.structfield)-1][1]--
		}
		if c.structfield[len(c.structfield)-1][1] == 0 {
			c.structfield = c.structfield[0 : len(c.structfield)-1]
		}

	case *ast.IfStmt:
		var xx = (x).(*ast.IfStmt)
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.subblocks[len(c.subblocks)-1]--
		if _, dead := c.deadif[xx]; dead {
			break
		}
		var stack []uint64
		var variant = mapast.BlocOfCodeIfElse
		if xx.Else == nil {
			variant = mapast.BlocOfCodeIf
		}
		var t = c.nowblock[len(c.nowblock)-1]
		var theadcount uint64 = 0
		if xx.Init != nil {
			switch xx.Init.(type) {
			case *ast.ExprStmt:
				xxx := xx.Init.(*ast.ExprStmt)
				id, ok := xxx.X.(*ast.Ident)
				var ident []byte
				if ok {
					ident = []byte(id.Name)
				}
				c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionBrackets, 1)
				if ok {
					c.AstTree[o(o(t)+theadcount)] = ident
				} else {
					stack = append([]uint64{o(o(t) + theadcount)}, stack...)
				}
				theadcount++
				c.skippedexpressions++

			case *ast.IncDecStmt:
				xxx := xx.Init.(*ast.IncDecStmt)
				id, ok := xxx.X.(*ast.Ident)
				var ident []byte
				if ok {
					ident = []byte(id.Name)
				}
				_ = ident
				c.AstTree[o(t)+theadcount] = mapast.IncDecStmtNode(bool2byte(xxx.Tok != token.INC))
				if ok {
					c.AstTree[o(o(t)+theadcount)] = ident
				} else {
					stack = append([]uint64{o(o(t) + theadcount)}, stack...)
				}
				theadcount++
				c.skippedincdecs++

			case *ast.SendStmt:
				xxx := xx.Init.(*ast.SendStmt)
				id1, ok1 := xxx.Chan.(*ast.Ident)
				var ident1 []byte
				if ok1 {
					ident1 = []byte(id1.Name)
				}
				id2, ok2 := xxx.Value.(*ast.Ident)
				var ident2 []byte
				if ok2 {
					ident2 = []byte(id2.Name)
				}
				_ = ident1
				_ = ident2
				c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionArrow, 2)
				if ok1 {
					c.AstTree[o(o(t)+theadcount)] = ident1
				} else {
					stack = append([]uint64{o(o(t) + theadcount)}, stack...)
				}
				if ok2 {
					c.AstTree[o(o(t)+theadcount)+1] = ident2
				} else {
					stack = append([]uint64{o(o(t)+theadcount) + 1}, stack...)
				}
				theadcount++
				c.skippedsends++

			case *ast.AssignStmt:
				xxx := xx.Init.(*ast.AssignStmt)
				var variant byte
				switch xxx.Tok {
				case token.ASSIGN:
					variant = mapast.AssignStmtEqual

				case token.DEFINE:
					variant = mapast.AssignStmtColonEq

				case token.ADD_ASSIGN:
					variant = mapast.AssignStmtAdd

				case token.SUB_ASSIGN:
					variant = mapast.AssignStmtSub

				case token.MUL_ASSIGN:
					variant = mapast.AssignStmtMul

				case token.QUO_ASSIGN:
					variant = mapast.AssignStmtQuo

				case token.REM_ASSIGN:
					variant = mapast.AssignStmtRem

				case token.AND_ASSIGN:
					variant = mapast.AssignStmtAnd

				case token.OR_ASSIGN:
					variant = mapast.AssignStmtOr

				case token.XOR_ASSIGN:
					variant = mapast.AssignStmtXor

				case token.SHL_ASSIGN:
					variant = mapast.AssignStmtShl

				case token.SHR_ASSIGN:
					variant = mapast.AssignStmtShr

				case token.AND_NOT_ASSIGN:
					variant = mapast.AssignStmtAndNot

				}
				if len(xxx.Lhs) != len(xxx.Rhs) {
					variant += mapast.AssignStmtMoreEqual
				}
				var r uint64 = 0
				for i := range xxx.Lhs {
					var ident []byte
					id2, ok2 := xxx.Lhs[i].(*ast.Ident)
					if ok2 {
						ident = []byte(id2.Name)
					}
					if ok2 {
						c.AstTree[o(o(t)+theadcount)+r] = ident
					} else {
						stack = append([]uint64{o(o(t)+theadcount) + r}, stack...)
					}
					r++
				}
				for i := range xxx.Rhs {
					var ident []byte
					id2, ok2 := xxx.Rhs[i].(*ast.Ident)
					if ok2 {
						ident = []byte(id2.Name)
					}
					if ok2 {
						c.AstTree[o(o(t)+theadcount)+r] = ident
					} else {
						stack = append([]uint64{o(o(t)+theadcount) + r}, stack...)
					}
					r++
				}
				c.AstTree[o(t)+theadcount] = mapast.AssignStmtNode(variant, (r))
				theadcount++
				c.skippedassignments++

			}
			c.AstTree[o(t)+theadcount] = mapast.BranchStmtNode(mapast.BranchStmtSemi)
			theadcount++
		}
		id, ok := xx.Cond.(*ast.Ident)
		var ident []byte
		if ok {
			ident = []byte(id.Name)
		}
		_ = ident
		c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionBrackets, 1)
		if ok {
			c.AstTree[o(o(t)+theadcount)] = ident
		} else {
			stack = append([]uint64{o(o(t) + theadcount)}, stack...)
		}
		theadcount++
		c.AstTree[t] = mapast.BlocOfCodeNode(variant, (theadcount))
		c.ifblocks[xx.Body] = o(t) + theadcount
		c.nowblock[len(c.nowblock)-1]++
		for xx.Else != nil {
			var ok bool
			var yy *ast.IfStmt
			yy, ok = xx.Else.(*ast.IfStmt)
			if ok {
				xx = yy
				c.deadif[xx] = struct{}{}
				variant = mapast.BlocOfCodeIfElse
				if xx.Else == nil {
					variant = mapast.BlocOfCodeIf
				}
				var t = c.nowblock[len(c.nowblock)-1]
				var theadcount uint64 = 0
				if xx.Init != nil {
					switch xx.Init.(type) {
					case *ast.ExprStmt:
						xxx := xx.Init.(*ast.ExprStmt)
						id, ok := xxx.X.(*ast.Ident)
						var ident []byte
						if ok {
							ident = []byte(id.Name)
						}
						c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionBrackets, 1)
						if ok {
							c.AstTree[o(o(t)+theadcount)] = ident
						} else {
							stack = append([]uint64{o(o(t) + theadcount)}, stack...)
						}
						theadcount++
						c.deadexprs[xxx] = struct{}{}

					case *ast.IncDecStmt:
						xxx := xx.Init.(*ast.IncDecStmt)
						id, ok := xxx.X.(*ast.Ident)
						var ident []byte
						if ok {
							ident = []byte(id.Name)
						}
						_ = ident
						c.AstTree[o(t)+theadcount] = mapast.IncDecStmtNode(bool2byte(xxx.Tok != token.INC))
						if ok {
							c.AstTree[o(o(t)+theadcount)] = ident
						} else {
							stack = append([]uint64{o(o(t) + theadcount)}, stack...)
						}
						theadcount++
						c.deadincdecs[xxx] = struct{}{}

					case *ast.SendStmt:
						xxx := xx.Init.(*ast.SendStmt)
						id1, ok1 := xxx.Chan.(*ast.Ident)
						var ident1 []byte
						if ok1 {
							ident1 = []byte(id1.Name)
						}
						id2, ok2 := xxx.Value.(*ast.Ident)
						var ident2 []byte
						if ok2 {
							ident2 = []byte(id2.Name)
						}
						_ = ident1
						_ = ident2
						c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionArrow, 2)
						if ok1 {
							c.AstTree[o(o(t)+theadcount)] = ident1
						} else {
							stack = append([]uint64{o(o(t) + theadcount)}, stack...)
						}
						if ok2 {
							c.AstTree[o(o(t)+theadcount)+1] = ident2
						} else {
							stack = append([]uint64{o(o(t)+theadcount) + 1}, stack...)
						}
						theadcount++
						c.deadsends[xxx] = struct{}{}

					case *ast.AssignStmt:
						xxx := xx.Init.(*ast.AssignStmt)
						var variant byte
						switch xxx.Tok {
						case token.ASSIGN:
							variant = mapast.AssignStmtEqual

						case token.DEFINE:
							variant = mapast.AssignStmtColonEq

						case token.ADD_ASSIGN:
							variant = mapast.AssignStmtAdd

						case token.SUB_ASSIGN:
							variant = mapast.AssignStmtSub

						case token.MUL_ASSIGN:
							variant = mapast.AssignStmtMul

						case token.QUO_ASSIGN:
							variant = mapast.AssignStmtQuo

						case token.REM_ASSIGN:
							variant = mapast.AssignStmtRem

						case token.AND_ASSIGN:
							variant = mapast.AssignStmtAnd

						case token.OR_ASSIGN:
							variant = mapast.AssignStmtOr

						case token.XOR_ASSIGN:
							variant = mapast.AssignStmtXor

						case token.SHL_ASSIGN:
							variant = mapast.AssignStmtShl

						case token.SHR_ASSIGN:
							variant = mapast.AssignStmtShr

						case token.AND_NOT_ASSIGN:
							variant = mapast.AssignStmtAndNot

						}
						if len(xxx.Lhs) != len(xxx.Rhs) {
							variant += mapast.AssignStmtMoreEqual
						}
						var r uint64 = 0
						for i := range xxx.Lhs {
							var ident []byte
							id2, ok2 := xxx.Lhs[i].(*ast.Ident)
							if ok2 {
								ident = []byte(id2.Name)
							}
							if ok2 {
								c.AstTree[o(o(t)+theadcount)+r] = ident
							} else {
								stack = append([]uint64{o(o(t)+theadcount) + r}, stack...)
							}
							r++
						}
						for i := range xxx.Rhs {
							var ident []byte
							id2, ok2 := xxx.Rhs[i].(*ast.Ident)
							if ok2 {
								ident = []byte(id2.Name)
							}
							if ok2 {
								c.AstTree[o(o(t)+theadcount)+r] = ident
							} else {
								stack = append([]uint64{o(o(t)+theadcount) + r}, stack...)
							}
							r++
						}
						c.AstTree[o(t)+theadcount] = mapast.AssignStmtNode(variant, (r))
						theadcount++
						c.deadassignments[xxx] = struct{}{}

					}
					c.AstTree[o(t)+theadcount] = mapast.BranchStmtNode(mapast.BranchStmtSemi)
					theadcount++
				}
				id, ok := xx.Cond.(*ast.Ident)
				var ident []byte
				if ok {
					ident = []byte(id.Name)
				}
				_ = ident
				c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionBrackets, 1)
				if ok {
					c.AstTree[o(o(t)+theadcount)] = ident
				} else {
					stack = append([]uint64{o(o(t) + theadcount)}, stack...)
				}
				theadcount++
				c.AstTree[t] = mapast.BlocOfCodeNode(variant, (theadcount))
				c.ifblocks[xx.Body] = o(t) + theadcount
				c.nowblock[len(c.nowblock)-1]++
				continue
			} else {
				var zz *ast.BlockStmt
				zz, ok = xx.Else.(*ast.BlockStmt)
				_ = zz
				var t = c.nowblock[len(c.nowblock)-1]
				var theadcount uint64 = 0
				c.AstTree[t] = mapast.BlocOfCodeNode(mapast.BlocOfCodePlain, 0)
				c.nowblock[len(c.nowblock)-1]++
				c.ifblocks[zz] = o(t) + theadcount
				break
			}
		}
		c.typefield = append(c.typefield, stack...)

	case *ast.RangeStmt:
		var xx = (x).(*ast.RangeStmt)
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.subblocks[len(c.subblocks)-1]--
		var stack []uint64
		if xx.Key == nil && xx.Value == nil {
			var t = c.nowblock[len(c.nowblock)-1]
			var theadcount uint64 = 0
			id, ok := xx.X.(*ast.Ident)
			var ident []byte
			if ok {
				ident = []byte(id.Name)
			}
			_ = ident
			c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionBrackets, 1)
			if ok {
				c.AstTree[o(o(t)+theadcount)] = ident
			} else {
				stack = append([]uint64{o(o(t) + theadcount)}, stack...)
			}
			theadcount++
			c.AstTree[t] = mapast.BlocOfCodeNode(mapast.BlocOfCodeForRange, (theadcount))
			c.nowblock[len(c.nowblock)-1]++
			theadcount++
			c.blocksstmts[xx.Body] = o(t) + theadcount - 1
			c.nowblock = append(c.nowblock, o(t)+theadcount-1)
			c.subblocks = append(c.subblocks, how_many_subblocks_block(xx.Body))
			c.substmts = append(c.substmts, how_many_substmts_block(xx.Body))
		} else {
			var variant byte
			switch xx.Tok {
			case token.ASSIGN:
				variant = mapast.AssignStmtMoreEqualRange

			case token.DEFINE:
				variant = mapast.AssignStmtMoreColonEqRange

			}
			_ = variant
			var offset uint64
			var t = c.nowblock[len(c.nowblock)-1]
			var theadcount uint64 = 0
			if xx.Key != nil {
				id, ok := xx.Key.(*ast.Ident)
				var ident []byte
				if ok {
					ident = []byte(id.Name)
				}
				_ = ident
				c.AstTree[o(o(t)+theadcount)] = mapast.ExpressionNode(mapast.ExpressionIdentifier, 1)
				if ok {
					c.AstTree[o(o(o(t)+theadcount))] = ident
				} else {
					stack = append([]uint64{(o(o(t) + theadcount))}, stack...)
				}
			}
			if xx.Value != nil {
				id, ok := xx.Value.(*ast.Ident)
				var ident []byte
				if ok {
					ident = []byte(id.Name)
				}
				_ = ident
				c.AstTree[o(o(t)+theadcount)+1] = mapast.ExpressionNode(mapast.ExpressionIdentifier, 1)
				if ok {
					c.AstTree[o(o(o(t)+theadcount)+1)] = ident
				} else {
					stack = append([]uint64{(o(o(t)+theadcount) + 1)}, stack...)
				}
				offset = 2
			} else {
				offset = 1
			}
			id, ok := xx.X.(*ast.Ident)
			var ident []byte
			if ok {
				ident = []byte(id.Name)
			}
			_ = ident
			c.AstTree[o(o(t)+theadcount)+offset] = mapast.ExpressionNode(mapast.ExpressionBrackets, 1)
			if ok {
				c.AstTree[o(o(o(t)+theadcount)+offset)] = ident
			} else {
				stack = append([]uint64{o(o(o(t)+theadcount) + offset)}, stack...)
			}
			c.AstTree[o(t)+theadcount] = mapast.AssignStmtNode(variant, (1 + offset))
			theadcount++
			c.AstTree[t] = mapast.BlocOfCodeNode(mapast.BlocOfCodeFor, (theadcount))
			c.nowblock[len(c.nowblock)-1]++
			theadcount++
			c.blocksstmts[xx.Body] = o(t) + theadcount - 1
			c.nowblock = append(c.nowblock, o(t)+theadcount-1)
			c.subblocks = append(c.subblocks, how_many_subblocks_block(xx.Body))
			c.substmts = append(c.substmts, how_many_substmts_block(xx.Body))
		}
		c.typefield = append(c.typefield, stack...)

	case *ast.ForStmt:
		var xx = (x).(*ast.ForStmt)
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.subblocks[len(c.subblocks)-1]--
		var uniform = (xx.Init == nil) && (xx.Post == nil)
		_ = uniform
		var t = c.nowblock[len(c.nowblock)-1]
		var theadcount uint64 = 0
		c.nowblock[len(c.nowblock)-1]++
		var stack []uint64
		switch xx.Init.(type) {
		case *ast.ExprStmt:
			xxx := xx.Init.(*ast.ExprStmt)
			id, ok := xxx.X.(*ast.Ident)
			var ident []byte
			if ok {
				ident = []byte(id.Name)
			}
			_ = ident
			c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionBrackets, 1)
			if ok {
				c.AstTree[o(o(t)+theadcount)] = ident
			} else {
				stack = append([]uint64{o(o(t) + theadcount)}, stack...)
			}
			theadcount++
			c.skippedexpressions++

		case *ast.IncDecStmt:
			xxx := xx.Init.(*ast.IncDecStmt)
			id, ok := xxx.X.(*ast.Ident)
			var ident []byte
			if ok {
				ident = []byte(id.Name)
			}
			_ = ident
			c.AstTree[o(t)+theadcount] = mapast.IncDecStmtNode(bool2byte(xxx.Tok != token.INC))
			if ok {
				c.AstTree[o(o(t)+theadcount)] = ident
			} else {
				stack = append([]uint64{o(o(t) + theadcount)}, stack...)
			}
			theadcount++
			c.skippedincdecs++

		case *ast.SendStmt:
			xxx := xx.Init.(*ast.SendStmt)
			id1, ok1 := xxx.Chan.(*ast.Ident)
			var ident1 []byte
			if ok1 {
				ident1 = []byte(id1.Name)
			}
			id2, ok2 := xxx.Value.(*ast.Ident)
			var ident2 []byte
			if ok2 {
				ident2 = []byte(id2.Name)
			}
			_ = ident1
			_ = ident2
			c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionArrow, 2)
			if ok1 {
				c.AstTree[(o(o(t) + theadcount))] = ident1
			} else {
				stack = append([]uint64{(o(o(t) + theadcount))}, stack...)
			}
			if ok2 {
				c.AstTree[(o(o(t)+theadcount) + 1)] = ident2
			} else {
				stack = append([]uint64{(o(o(t)+theadcount) + 1)}, stack...)
			}
			theadcount++
			c.skippedsends++

		case *ast.AssignStmt:
			xxx := xx.Init.(*ast.AssignStmt)
			var variant byte
			switch xxx.Tok {
			case token.ASSIGN:
				variant = mapast.AssignStmtEqual

			case token.DEFINE:
				variant = mapast.AssignStmtColonEq

			case token.ADD_ASSIGN:
				variant = mapast.AssignStmtAdd

			case token.SUB_ASSIGN:
				variant = mapast.AssignStmtSub

			case token.MUL_ASSIGN:
				variant = mapast.AssignStmtMul

			case token.QUO_ASSIGN:
				variant = mapast.AssignStmtQuo

			case token.REM_ASSIGN:
				variant = mapast.AssignStmtRem

			case token.AND_ASSIGN:
				variant = mapast.AssignStmtAnd

			case token.OR_ASSIGN:
				variant = mapast.AssignStmtOr

			case token.XOR_ASSIGN:
				variant = mapast.AssignStmtXor

			case token.SHL_ASSIGN:
				variant = mapast.AssignStmtShl

			case token.SHR_ASSIGN:
				variant = mapast.AssignStmtShr

			case token.AND_NOT_ASSIGN:
				variant = mapast.AssignStmtAndNot

			}
			if len(xxx.Lhs) != len(xxx.Rhs) {
				variant += mapast.AssignStmtMoreEqual
			}
			_ = variant
			var l = uint64(len(xxx.Lhs))
			c.AstTree[o(t)+theadcount] = mapast.AssignStmtNode(variant, uint64(len(xxx.Lhs)+len(xxx.Rhs)))
			for i := range xxx.Lhs {
				var ident []byte
				id, ok := xxx.Lhs[i].(*ast.Ident)
				if ok {
					ident = []byte(id.Name)
				}
				if ok {
					c.AstTree[(o(o(t)+theadcount) + uint64(i))] = ident
				} else {
					stack = append([]uint64{(o(o(t)+theadcount) + uint64(i))}, stack...)
				}
			}
			for i := range xxx.Rhs {
				var ident []byte
				id, ok := xxx.Rhs[i].(*ast.Ident)
				if ok {
					ident = []byte(id.Name)
				}
				if ok {
					c.AstTree[(o(o(t)+theadcount) + uint64(i) + l)] = ident
				} else {
					stack = append([]uint64{(o(o(t)+theadcount) + uint64(i) + l)}, stack...)
				}
			}
			theadcount++
			c.skippedassignments++

		}
		if !uniform {
			c.AstTree[o(t)+theadcount] = mapast.BranchStmtNode(mapast.BranchStmtSemi)
			theadcount++
		}
		if xx.Cond != nil {
			id, ok := xx.Cond.(*ast.Ident)
			var ident []byte
			if ok {
				ident = []byte(id.Name)
			}
			_ = ident
			c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionBrackets, 1)
			if ok {
				c.AstTree[o(o(t)+theadcount)] = ident
			} else {
				stack = append([]uint64{o(o(t) + theadcount)}, stack...)
			}
			theadcount++
		}
		if !uniform {
			c.AstTree[o(t)+theadcount] = mapast.BranchStmtNode(mapast.BranchStmtSemi)
			theadcount++
		}
		switch xx.Post.(type) {
		case *ast.ExprStmt:
			xxx := xx.Post.(*ast.ExprStmt)
			id, ok := xxx.X.(*ast.Ident)
			var ident []byte
			if ok {
				ident = []byte(id.Name)
			}
			_ = ident
			c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionBrackets, 1)
			if ok {
				c.AstTree[o(o(t)+theadcount)] = ident
			} else {
				stack = append([]uint64{o(o(t) + theadcount)}, stack...)
			}
			theadcount++
			c.skippedexpressions++

		case *ast.IncDecStmt:
			xxx := xx.Post.(*ast.IncDecStmt)
			id, ok := xxx.X.(*ast.Ident)
			var ident []byte
			if ok {
				ident = []byte(id.Name)
			}
			_ = ident
			c.AstTree[o(t)+theadcount] = mapast.IncDecStmtNode(bool2byte(xxx.Tok != token.INC))
			if ok {
				c.AstTree[o(o(t)+theadcount)] = ident
			} else {
				stack = append([]uint64{o(o(t) + theadcount)}, stack...)
			}
			theadcount++
			c.skippedincdecs++

		case *ast.SendStmt:
			xxx := xx.Post.(*ast.SendStmt)
			id1, ok1 := xxx.Chan.(*ast.Ident)
			var ident1 []byte
			if ok1 {
				ident1 = []byte(id1.Name)
			}
			id2, ok2 := xxx.Value.(*ast.Ident)
			var ident2 []byte
			if ok2 {
				ident2 = []byte(id2.Name)
			}
			_ = ident1
			_ = ident2
			c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionArrow, 2)
			if ok1 {
				c.AstTree[(o(o(t) + theadcount))] = ident1
			} else {
				stack = append([]uint64{(o(o(t) + theadcount))}, stack...)
			}
			if ok2 {
				c.AstTree[(o(o(t)+theadcount) + 1)] = ident2
			} else {
				stack = append([]uint64{(o(o(t)+theadcount) + 1)}, stack...)
			}
			theadcount++
			c.skippedsends++

		case *ast.AssignStmt:
			xxx := xx.Post.(*ast.AssignStmt)
			var variant byte
			switch xxx.Tok {
			case token.ASSIGN:
				variant = mapast.AssignStmtEqual

			case token.DEFINE:
				variant = mapast.AssignStmtColonEq

			case token.ADD_ASSIGN:
				variant = mapast.AssignStmtAdd

			case token.SUB_ASSIGN:
				variant = mapast.AssignStmtSub

			case token.MUL_ASSIGN:
				variant = mapast.AssignStmtMul

			case token.QUO_ASSIGN:
				variant = mapast.AssignStmtQuo

			case token.REM_ASSIGN:
				variant = mapast.AssignStmtRem

			case token.AND_ASSIGN:
				variant = mapast.AssignStmtAnd

			case token.OR_ASSIGN:
				variant = mapast.AssignStmtOr

			case token.XOR_ASSIGN:
				variant = mapast.AssignStmtXor

			case token.SHL_ASSIGN:
				variant = mapast.AssignStmtShl

			case token.SHR_ASSIGN:
				variant = mapast.AssignStmtShr

			case token.AND_NOT_ASSIGN:
				variant = mapast.AssignStmtAndNot

			}
			if len(xxx.Lhs) != len(xxx.Rhs) {
				variant += mapast.AssignStmtMoreEqual
			}
			var l = uint64(len(xxx.Lhs))
			c.AstTree[o(t)+theadcount] = mapast.AssignStmtNode(variant, uint64(len(xxx.Lhs)+len(xxx.Rhs)))
			for i := range xxx.Lhs {
				var ident []byte
				id, ok := xxx.Lhs[i].(*ast.Ident)
				if ok {
					ident = []byte(id.Name)
				}
				if ok {
					c.AstTree[(o(o(t)+theadcount) + uint64(i))] = ident
				} else {
					stack = append([]uint64{(o(o(t)+theadcount) + uint64(i))}, stack...)
				}
			}
			for i := range xxx.Rhs {
				var ident []byte
				id, ok := xxx.Rhs[i].(*ast.Ident)
				if ok {
					ident = []byte(id.Name)
				}
				if ok {
					c.AstTree[(o(o(t)+theadcount) + uint64(i) + l)] = ident
				} else {
					stack = append([]uint64{(o(o(t)+theadcount) + uint64(i) + l)}, stack...)
				}
			}
			theadcount++
			c.skippedassignments++

		}
		c.blocksstmts[xx.Body] = o(t) + theadcount
		c.nowblock = append(c.nowblock, o(t)+theadcount)
		c.subblocks = append(c.subblocks, how_many_subblocks_block(xx.Body))
		c.substmts = append(c.substmts, how_many_substmts_block(xx.Body))
		c.AstTree[t] = mapast.BlocOfCodeNode(mapast.BlocOfCodeFor, (theadcount))
		c.typefield = append(c.typefield, stack...)

	case *ast.SwitchStmt:
		var xx = (x).(*ast.SwitchStmt)
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.subblocks[len(c.subblocks)-1]--
		var t = c.nowblock[len(c.nowblock)-1]
		c.AstTree[t] = mapast.BlocOfCodeNode(mapast.BlocOfCodeSwitch, uint64(bool2byte(xx.Tag != nil)+2*bool2byte(xx.Init != nil)))
		c.nowblock[len(c.nowblock)-1]++
		var theadcount uint64
		var stack []uint64
		if xx.Init != nil {
			switch xx.Init.(type) {
			case *ast.ExprStmt:
				xxx := xx.Init.(*ast.ExprStmt)
				id, ok := xxx.X.(*ast.Ident)
				var ident []byte
				if ok {
					ident = []byte(id.Name)
				}
				_ = ident
				c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionBrackets, 1)
				if ok {
					c.AstTree[o(o(t)+theadcount)] = ident
				} else {
					stack = append([]uint64{o(o(t) + theadcount)}, stack...)
				}
				theadcount++
				c.skippedexpressions++

			case *ast.IncDecStmt:
				xxx := xx.Init.(*ast.IncDecStmt)
				id, ok := xxx.X.(*ast.Ident)
				var ident []byte
				if ok {
					ident = []byte(id.Name)
				}
				_ = ident
				c.AstTree[o(t)+theadcount] = mapast.IncDecStmtNode(bool2byte(xxx.Tok != token.INC))
				if ok {
					c.AstTree[o(o(t)+theadcount)] = ident
				} else {
					stack = append([]uint64{o(o(t) + theadcount)}, stack...)
				}
				theadcount++
				c.skippedincdecs++

			case *ast.SendStmt:
				xxx := xx.Init.(*ast.SendStmt)
				id1, ok1 := xxx.Chan.(*ast.Ident)
				var ident1 []byte
				if ok1 {
					ident1 = []byte(id1.Name)
				}
				id2, ok2 := xxx.Value.(*ast.Ident)
				var ident2 []byte
				if ok2 {
					ident2 = []byte(id2.Name)
				}
				_ = ident1
				_ = ident2
				c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionArrow, 2)
				if ok1 {
					c.AstTree[(o(o(t) + theadcount))] = ident1
				} else {
					stack = append([]uint64{(o(o(t) + theadcount))}, stack...)
				}
				if ok2 {
					c.AstTree[(o(o(t)+theadcount) + 1)] = ident2
				} else {
					stack = append([]uint64{(o(o(t)+theadcount) + 1)}, stack...)
				}
				theadcount++
				c.skippedsends++

			case *ast.AssignStmt:
				xxx := xx.Init.(*ast.AssignStmt)
				var variant byte
				switch xxx.Tok {
				case token.ASSIGN:
					variant = mapast.AssignStmtEqual

				case token.DEFINE:
					variant = mapast.AssignStmtColonEq

				case token.ADD_ASSIGN:
					variant = mapast.AssignStmtAdd

				case token.SUB_ASSIGN:
					variant = mapast.AssignStmtSub

				case token.MUL_ASSIGN:
					variant = mapast.AssignStmtMul

				case token.QUO_ASSIGN:
					variant = mapast.AssignStmtQuo

				case token.REM_ASSIGN:
					variant = mapast.AssignStmtRem

				case token.AND_ASSIGN:
					variant = mapast.AssignStmtAnd

				case token.OR_ASSIGN:
					variant = mapast.AssignStmtOr

				case token.XOR_ASSIGN:
					variant = mapast.AssignStmtXor

				case token.SHL_ASSIGN:
					variant = mapast.AssignStmtShl

				case token.SHR_ASSIGN:
					variant = mapast.AssignStmtShr

				case token.AND_NOT_ASSIGN:
					variant = mapast.AssignStmtAndNot

				}
				if len(xxx.Lhs) != len(xxx.Rhs) {
					variant += mapast.AssignStmtMoreEqual
				}
				var l = uint64(len(xxx.Lhs))
				c.AstTree[o(t)+theadcount] = mapast.AssignStmtNode(variant, uint64(len(xxx.Lhs)+len(xxx.Rhs)))
				for i := range xxx.Lhs {
					var ident []byte
					id, ok := xxx.Lhs[i].(*ast.Ident)
					if ok {
						ident = []byte(id.Name)
					}
					if ok {
						c.AstTree[(o(o(t)+theadcount) + uint64(i))] = ident
					} else {
						stack = append([]uint64{(o(o(t)+theadcount) + uint64(i))}, stack...)
					}
				}
				for i := range xxx.Rhs {
					var ident []byte
					id, ok := xxx.Rhs[i].(*ast.Ident)
					if ok {
						ident = []byte(id.Name)
					}
					if ok {
						c.AstTree[(o(o(t)+theadcount) + uint64(i) + l)] = ident
					} else {
						stack = append([]uint64{(o(o(t)+theadcount) + uint64(i) + l)}, stack...)
					}
				}
				theadcount++
				c.skippedassignments++

			}
			c.AstTree[o(t)+theadcount] = mapast.BranchStmtNode(mapast.BranchStmtSemi)
			theadcount++
		}
		if xx.Tag != nil {
			var ident []byte
			id, ok := xx.Tag.(*ast.Ident)
			if ok {
				ident = []byte(id.Name)
			} else {
				id, ok2 := xx.Tag.(*ast.BasicLit)
				if ok2 {
					ident = []byte(id.Value)
					ok = true
					c.skippedbalits[id] = struct{}{}
				}
			}
			if ok {
				c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionIdentifier, 1)
				c.AstTree[o(o(t)+theadcount)] = ident
			} else {
				stack = append([]uint64{o(t) + theadcount}, stack...)
			}
			theadcount++
		}
		c.blocksstmts[xx.Body] = o(t) + theadcount
		c.nowblock = append(c.nowblock, o(t)+theadcount)
		c.subblocks = append(c.subblocks, how_many_subblocks_block(xx.Body))
		c.substmts = append(c.substmts, 0)
		c.typefield = append(c.typefield, stack...)

	case *ast.TypeSwitchStmt:
		var xx = (x).(*ast.TypeSwitchStmt)
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.subblocks[len(c.subblocks)-1]--
		var t = c.nowblock[len(c.nowblock)-1]
		c.AstTree[t] = mapast.BlocOfCodeNode(mapast.BlocOfCodeTypeSwitch, 1+2*uint64(bool2byte(xx.Init != nil)))
		c.nowblock[len(c.nowblock)-1]++
		for _, v := range xx.Body.List {
			if w, ok := v.(*ast.CaseClause); ok {
				c.typedcases[w] = struct{}{}
			}
		}
		var theadcount uint64
		var stack []uint64
		var ident []byte
		var ok bool
		if xx.Init != nil {
			switch xx.Init.(type) {
			case *ast.ExprStmt:
				xxx := xx.Init.(*ast.ExprStmt)
				id, ok := xxx.X.(*ast.Ident)
				var ident []byte
				if ok {
					ident = []byte(id.Name)
				}
				_ = ident
				c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionBrackets, 1)
				if ok {
					c.AstTree[o(o(t)+theadcount)] = ident
				} else {
					stack = append([]uint64{o(o(t) + theadcount)}, stack...)
				}
				theadcount++
				c.skippedexpressions++

			case *ast.IncDecStmt:
				xxx := xx.Init.(*ast.IncDecStmt)
				id, ok := xxx.X.(*ast.Ident)
				var ident []byte
				if ok {
					ident = []byte(id.Name)
				}
				_ = ident
				c.AstTree[o(t)+theadcount] = mapast.IncDecStmtNode(bool2byte(xxx.Tok != token.INC))
				if ok {
					c.AstTree[o(o(t)+theadcount)] = ident
				} else {
					stack = append([]uint64{o(o(t) + theadcount)}, stack...)
				}
				theadcount++
				c.skippedincdecs++

			case *ast.SendStmt:
				xxx := xx.Init.(*ast.SendStmt)
				id1, ok1 := xxx.Chan.(*ast.Ident)
				var ident1 []byte
				if ok1 {
					ident1 = []byte(id1.Name)
				}
				id2, ok2 := xxx.Value.(*ast.Ident)
				var ident2 []byte
				if ok2 {
					ident2 = []byte(id2.Name)
				}
				_ = ident1
				_ = ident2
				c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionArrow, 2)
				if ok1 {
					c.AstTree[(o(o(t) + theadcount))] = ident1
				} else {
					stack = append([]uint64{(o(o(t) + theadcount))}, stack...)
				}
				if ok2 {
					c.AstTree[(o(o(t)+theadcount) + 1)] = ident2
				} else {
					stack = append([]uint64{(o(o(t)+theadcount) + 1)}, stack...)
				}
				theadcount++
				c.skippedsends++

			case *ast.AssignStmt:
				xxx := xx.Init.(*ast.AssignStmt)
				var variant byte
				switch xxx.Tok {
				case token.ASSIGN:
					variant = mapast.AssignStmtEqual

				case token.DEFINE:
					variant = mapast.AssignStmtColonEq

				case token.ADD_ASSIGN:
					variant = mapast.AssignStmtAdd

				case token.SUB_ASSIGN:
					variant = mapast.AssignStmtSub

				case token.MUL_ASSIGN:
					variant = mapast.AssignStmtMul

				case token.QUO_ASSIGN:
					variant = mapast.AssignStmtQuo

				case token.REM_ASSIGN:
					variant = mapast.AssignStmtRem

				case token.AND_ASSIGN:
					variant = mapast.AssignStmtAnd

				case token.OR_ASSIGN:
					variant = mapast.AssignStmtOr

				case token.XOR_ASSIGN:
					variant = mapast.AssignStmtXor

				case token.SHL_ASSIGN:
					variant = mapast.AssignStmtShl

				case token.SHR_ASSIGN:
					variant = mapast.AssignStmtShr

				case token.AND_NOT_ASSIGN:
					variant = mapast.AssignStmtAndNot

				}
				if len(xxx.Lhs) != len(xxx.Rhs) {
					variant += mapast.AssignStmtMoreEqual
				}
				var l = uint64(len(xxx.Lhs))
				c.AstTree[o(t)+theadcount] = mapast.AssignStmtNode(variant, uint64(len(xxx.Lhs)+len(xxx.Rhs)))
				for i := range xxx.Lhs {
					var ident []byte
					id, ok := xxx.Lhs[i].(*ast.Ident)
					if ok {
						ident = []byte(id.Name)
					}
					if ok {
						c.AstTree[(o(o(t)+theadcount) + uint64(i))] = ident
					} else {
						stack = append([]uint64{(o(o(t)+theadcount) + uint64(i))}, stack...)
					}
				}
				for i := range xxx.Rhs {
					var ident []byte
					id, ok := xxx.Rhs[i].(*ast.Ident)
					if ok {
						ident = []byte(id.Name)
					}
					if ok {
						c.AstTree[(o(o(t)+theadcount) + uint64(i) + l)] = ident
					} else {
						stack = append([]uint64{(o(o(t)+theadcount) + uint64(i) + l)}, stack...)
					}
				}
				theadcount++
				c.skippedassignments++

			}
			c.AstTree[o(t)+theadcount] = mapast.BranchStmtNode(mapast.BranchStmtSemi)
			theadcount++
		}
		if _, ok = xx.Assign.(*ast.ExprStmt); ok {
			id, ok := xx.Assign.(*ast.ExprStmt).X.(*ast.TypeAssertExpr).X.(*ast.Ident)
			if ok {
				ident = []byte(id.Name)
			}
			if ok {
				c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionType, 1)
				c.AstTree[o(o(t)+theadcount)] = ident
			} else {
				stack = append([]uint64{(o(t) + theadcount)}, stack...)
			}
			theadcount++
			c.skippedexpressions++
		} else {
			xxx := xx.Assign.(*ast.AssignStmt)
			var variant byte
			switch xxx.Tok {
			case token.ASSIGN:
				variant = mapast.AssignStmtEqual

			case token.DEFINE:
				variant = mapast.AssignStmtColonEq

			case token.ADD_ASSIGN:
				variant = mapast.AssignStmtAdd

			case token.SUB_ASSIGN:
				variant = mapast.AssignStmtSub

			case token.MUL_ASSIGN:
				variant = mapast.AssignStmtMul

			case token.QUO_ASSIGN:
				variant = mapast.AssignStmtQuo

			case token.REM_ASSIGN:
				variant = mapast.AssignStmtRem

			case token.AND_ASSIGN:
				variant = mapast.AssignStmtAnd

			case token.OR_ASSIGN:
				variant = mapast.AssignStmtOr

			case token.XOR_ASSIGN:
				variant = mapast.AssignStmtXor

			case token.SHL_ASSIGN:
				variant = mapast.AssignStmtShl

			case token.SHR_ASSIGN:
				variant = mapast.AssignStmtShr

			case token.AND_NOT_ASSIGN:
				variant = mapast.AssignStmtAndNot

			}
			if len(xxx.Lhs) != len(xxx.Rhs) {
				variant += mapast.AssignStmtMoreEqual
			}
			var l = uint64(len(xxx.Lhs))
			c.AstTree[o(t)+theadcount] = mapast.AssignStmtNode(variant, uint64(len(xxx.Lhs)+len(xxx.Rhs)))
			for i := range xxx.Lhs {
				var ident []byte
				id, ok := xxx.Lhs[i].(*ast.Ident)
				if ok {
					ident = []byte(id.Name)
				}
				if ok {
					c.AstTree[(o(o(t)+theadcount) + uint64(i))] = ident
				} else {
					stack = append([]uint64{(o(o(t)+theadcount) + uint64(i))}, stack...)
				}
			}
			for i := range xxx.Rhs {
				var ident []byte
				id, ok := xxx.Rhs[i].(*ast.Ident)
				if ok {
					ident = []byte(id.Name)
				}
				if ok {
					c.AstTree[(o(o(t)+theadcount) + uint64(i) + l)] = ident
				} else {
					stack = append([]uint64{(o(o(t)+theadcount) + uint64(i) + l)}, stack...)
				}
			}
			theadcount++
			c.skippedassignments++
		}
		c.blocksstmts[xx.Body] = o(t) + theadcount
		c.nowblock = append(c.nowblock, o(t)+theadcount)
		c.subblocks = append(c.subblocks, how_many_subblocks_block(xx.Body))
		c.substmts = append(c.substmts, 0)
		c.typefield = append(c.typefield, stack...)

	case *ast.CaseClause:
		var xx = (x).(*ast.CaseClause)
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.subblocks[len(c.subblocks)-1]--
		var theadcount uint64 = uint64(len(xx.List))
		var variant = mapast.BlocOfCodeCase
		if xx.List == nil {
			variant = mapast.BlocOfCodeDefault
			theadcount = 0
		}
		var stack []uint64
		var t = c.nowblock[len(c.nowblock)-1]
		c.nowblock[len(c.nowblock)-1]++
		c.AstTree[t] = mapast.BlocOfCodeNode(variant, theadcount)
		for i := uint64(0); i < uint64(len(xx.List)); i++ {
			id, ok := xx.List[i].(*ast.Ident)
			var ident []byte
			if ok {
				ident = []byte(id.Name)
			} else {
				id3, ok3 := xx.List[i].(*ast.BasicLit)
				if ok3 {
					ok = true
					ident = []byte(id3.Value)
					c.skippedbalits[id3] = struct{}{}
				}
			}
			if _, ok2 := c.typedcases[xx]; ok2 {
				c.AstTree[o(t)+i] = mapast.RootOfType
			} else {
				c.AstTree[o(t)+i] = mapast.ExpressionNode(mapast.ExpressionIdentifier, 1)
			}
			if ok {
				c.AstTree[o(o(t)+i)] = ident
			} else {
				stack = append([]uint64{(o(t) + i)}, stack...)
			}
		}
		c.typefield = append(c.typefield, stack...)
		var subs = how_many_subblocks_stmt_list(xx.Body)
		var sus = how_many_substmts_stmt_list(xx.Body)
		c.nowblock = append(c.nowblock, o(t)+(theadcount))
		c.subblocks = append(c.subblocks, subs)
		c.substmts = append(c.substmts, sus)

	case *ast.BlockStmt:
		var xx = (x).(*ast.BlockStmt)
		_ = xx
		var zz = c.ifblocks[xx]
		if zz != 0 {
			var subs = how_many_subblocks_block(xx)
			var sus = how_many_substmts_block(xx)
			c.nowblock = append(c.nowblock, zz)
			c.subblocks = append(c.subblocks, subs)
			c.substmts = append(c.substmts, sus)
		} else {
			zz = c.blocksstmts[xx]
		}
		if zz == 0 {
			for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
				c.substmts = c.substmts[0 : len(c.substmts)-1]
				c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
				c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
			}
			c.subblocks[len(c.subblocks)-1]--
			var t = c.nowblock[len(c.nowblock)-1]
			c.AstTree[t] = mapast.BlocOfCodeNode(mapast.BlocOfCodePlain, 0)
			c.nowblock[len(c.nowblock)-1]++
			var subs = how_many_subblocks_stmt_list(xx.List)
			var sus = how_many_substmts_stmt_list(xx.List)
			c.nowblock = append(c.nowblock, o(t))
			c.subblocks = append(c.subblocks, subs)
			c.substmts = append(c.substmts, sus)
		}

	case *ast.EmptyStmt:
		var xx = (x).(*ast.EmptyStmt)
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.substmts[len(c.substmts)-1]--
		if xx.Implicit == false {
			var blk = c.nowblock[len(c.nowblock)-1]
			c.AstTree[blk] = mapast.BranchStmtNode(mapast.BranchStmtSemi)
			c.nowblock[len(c.nowblock)-1]++
		}

	case *ast.BranchStmt:
		var xx = (x).(*ast.BranchStmt)
		var variant byte = 0
		switch xx.Tok {
		case token.BREAK:
			variant = mapast.BranchStmtBreak

		case token.CONTINUE:
			variant = mapast.BranchStmtContinue

		case token.FALLTHROUGH:
			variant = mapast.BranchStmtFallthrough

		case token.GOTO:
			variant = 255

		}
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.substmts[len(c.substmts)-1]--
		_ = variant
		var blk = c.nowblock[len(c.nowblock)-1]
		if variant == 255 {
			if xx.Label == nil {
				c.AstTree[blk] = mapast.BranchStmtNode(mapast.BranchStmtGoto)
			} else {
				c.AstTree[blk] = mapast.LblGotoCntNode(mapast.LblGotoCntGoto)
				c.AstTree[o(blk)] = []byte(xx.Label.Name)
			}
		} else {
			if xx.Label == nil {
				c.AstTree[blk] = mapast.BranchStmtNode(variant)
			} else {
				switch variant {
				case mapast.BranchStmtContinue:
					variant = mapast.LblGotoCntContinue

				case mapast.BranchStmtBreak:
					variant = mapast.LblGotoCntBreak

				}
				c.AstTree[blk] = mapast.LblGotoCntNode(variant)
				c.AstTree[o(blk)] = []byte(xx.Label.Name)
			}
		}
		c.nowblock[len(c.nowblock)-1]++

	case *ast.ExprStmt:
		var xx = (x).(*ast.ExprStmt)
		if c.skippedexpressions > 0 {
			c.skippedexpressions--
			break
		}
		if _, ok := c.deadexprs[xx]; ok {
			break
		}
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.substmts[len(c.substmts)-1]--
		id, ok := xx.X.(*ast.Ident)
		var ident []byte
		if ok {
			ident = []byte(id.Name)
		}
		_ = ident
		var stack []uint64
		var blk = c.nowblock[len(c.nowblock)-1]
		c.AstTree[blk] = mapast.ExpressionNode(mapast.ExpressionBrackets, 1)
		if ok {
			c.AstTree[o(blk)] = ident
		} else {
			stack = append([]uint64{(blk)}, stack...)
		}
		c.nowblock[len(c.nowblock)-1]++
		c.typefield = append(c.typefield, stack...)

	case *ast.BasicLit:
		var xx = (x).(*ast.BasicLit)
		if _, ok := c.skippedbalits[xx]; ok {
			delete(c.skippedbalits, xx)
			break
		}
		if len(c.typefield) == 0 {
			break
		}
		var where = c.typefield[len(c.typefield)-1]
		c.AstTree[where] = []byte(xx.Value)
		c.typefield = c.typefield[0 : len(c.typefield)-1]

	case *ast.SelectorExpr:
		var xx = (x).(*ast.SelectorExpr)
		if len(c.typefield) == 0 {
			break
		}
		id1, ok1 := xx.X.(*ast.Ident)
		var ident1 []byte
		if ok1 {
			ident1 = []byte(id1.Name)
		}
		var ident2 = []byte(xx.Sel.Name)
		const ok2 = true
		var stack []uint64
		var where = c.typefield[len(c.typefield)-1]
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		c.AstTree[where] = mapast.ExpressionNode(mapast.ExpressionDot, 2)
		if ok1 {
			c.AstTree[o(where)] = ident1
		} else {
			stack = append([]uint64{o(where)}, stack...)
		}
		if ok2 {
			c.AstTree[o(where)+1] = ident2
		} else {
			stack = append([]uint64{o(where) + 1}, stack...)
		}
		c.typefield = append(c.typefield, stack...)

	case *ast.ReturnStmt:
		var xx = (x).(*ast.ReturnStmt)
		for len(c.substmts) > 0 && len(c.subblocks) > 0 && c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		if len(c.substmts) > 0 {
			c.substmts[len(c.substmts)-1]--
		}
		var stack []uint64
		var blk = c.nowblock[len(c.nowblock)-1]
		c.AstTree[blk] = mapast.ReturnStmt
		for i := range xx.Results {
			var ident []byte
			id2, ok2 := xx.Results[i].(*ast.Ident)
			if ok2 {
				ident = []byte(id2.Name)
			} else {
				id3, ok3 := xx.Results[i].(*ast.BasicLit)
				if ok3 {
					ok2 = true
					ident = []byte(id3.Value)
					c.skippedbalits[id3] = struct{}{}
				}
			}
			c.AstTree[o(blk)+uint64(i)] = mapast.ExpressionNode(mapast.ExpressionIdentifier, 1)
			if ok2 {
				c.AstTree[o(o(blk)+uint64(i))] = ident
			} else {
				stack = append([]uint64{(o(blk) + uint64(i))}, stack...)
			}
		}
		c.nowblock[len(c.nowblock)-1]++
		c.typefield = append(c.typefield, stack...)

	case *ast.UnaryExpr:
		var xx = (x).(*ast.UnaryExpr)
		if len(c.typefield) == 0 {
			break
		}
		var variant byte
		switch xx.Op {
		case token.ADD:
			variant = mapast.ExpressionPlus

		case token.SUB:
			variant = mapast.ExpressionMinus

		case token.XOR:
			variant = mapast.ExpressionXor

		case token.NOT:
			variant = mapast.ExpressionNot

		case token.AND:
			variant = mapast.ExpressionAnd

		case token.ARROW:
			variant = mapast.ExpressionArrow

		}
		id, ok := xx.X.(*ast.Ident)
		var ident []byte
		if ok {
			ident = []byte(id.Name)
		}
		var where = c.typefield[len(c.typefield)-1]
		c.AstTree[where] = mapast.ExpressionNode(variant, 1)
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		if ok {
			c.AstTree[o(where)] = ident
		} else {
			c.typefield = append(c.typefield, o(where))
		}

	case *ast.StarExpr:
		var xx = (x).(*ast.StarExpr)
		if len(c.typefield) == 0 {
			break
		}
		var where = c.typefield[len(c.typefield)-1]
		c.AstTree[where] = mapast.Expression[0 : 1+mapast.ExpressionMul : 1+mapast.ExpressionTotalCount]
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		id, ok := xx.X.(*ast.Ident)
		if ok {
			c.AstTree[o(where)] = []byte(id.Name)
		} else {
			c.typefield = append(c.typefield, o(where))
		}

	case *ast.ParenExpr:
		var xx = (x).(*ast.ParenExpr)
		if len(c.typefield) == 0 {
			break
		}
		id, ok := xx.X.(*ast.Ident)
		var ident []byte
		if ok {
			ident = []byte(id.Name)
		}
		var stack []uint64
		var where = c.typefield[len(c.typefield)-1]
		c.AstTree[where] = mapast.ExpressionNode(mapast.ExpressionBrackets, 1)
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		if ok {
			c.AstTree[o(where)] = ident
		} else {
			stack = append([]uint64{o(where)}, stack...)
		}
		c.typefield = append(c.typefield, stack...)

	case *ast.BinaryExpr:
		var xx = (x).(*ast.BinaryExpr)
		if len(c.typefield) == 0 {
			break
		}
		var variant byte
		switch xx.Op {
		case token.LAND:
			variant = mapast.ExpressionAndAnd

		case token.LOR:
			variant = mapast.ExpressionOrOr

		case token.EQL:
			variant = mapast.ExpressionEqual

		case token.NEQ:
			variant = mapast.ExpressionNotEq

		case token.LSS:
			variant = mapast.ExpressionLessThan

		case token.LEQ:
			variant = mapast.ExpressionLessEq

		case token.GEQ:
			variant = mapast.ExpressionGrtEq

		case token.GTR:
			variant = mapast.ExpressionGrtThan

		case token.ADD:
			variant = mapast.ExpressionPlus

		case token.SUB:
			variant = mapast.ExpressionMinus

		case token.OR:
			variant = mapast.ExpressionOr

		case token.XOR:
			variant = mapast.ExpressionXor

		case token.MUL:
			variant = mapast.ExpressionMul

		case token.QUO:
			variant = mapast.ExpressionDiv

		case token.REM:
			variant = mapast.ExpressionMod

		case token.AND:
			variant = mapast.ExpressionAnd

		case token.AND_NOT:
			variant = mapast.ExpressionAndNot

		case token.SHL:
			variant = mapast.ExpressionLSh

		case token.SHR:
			variant = mapast.ExpressionRSh

		}
		id1, ok1 := xx.X.(*ast.Ident)
		var ident1 []byte
		if ok1 {
			ident1 = []byte(id1.Name)
		}
		id2, ok2 := xx.Y.(*ast.Ident)
		var ident2 []byte
		if ok2 {
			ident2 = []byte(id2.Name)
		}
		var stack []uint64
		var where = c.typefield[len(c.typefield)-1]
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		c.AstTree[where] = mapast.ExpressionNode(variant, 2)
		if ok1 {
			c.AstTree[o(where)] = ident1
		} else {
			stack = append([]uint64{o(where)}, stack...)
		}
		if ok2 {
			c.AstTree[o(where)+1] = ident2
		} else {
			stack = append([]uint64{o(where) + 1}, stack...)
		}
		c.typefield = append(c.typefield, stack...)

	case *ast.CallExpr:
		var xx = (x).(*ast.CallExpr)
		if len(c.typefield) == 0 {
			break
		}
		var variant byte
		if xx.Ellipsis == 0 {
			variant = mapast.ExpressionCall
		} else {
			variant = mapast.ExpressionCallDotDotDot
		}
		id, ok := xx.Fun.(*ast.Ident)
		var ident []byte
		if ok {
			ident = []byte(id.Name)
		}
		var stack []uint64
		var where = c.typefield[len(c.typefield)-1]
		if ok {
			c.AstTree[o(where)] = ident
		} else {
			stack = append([]uint64{o(where)}, stack...)
		}
		c.AstTree[where] = mapast.ExpressionNode(variant, 1+uint64(len(xx.Args)))
		for i := range xx.Args {
			var ident []byte
			id2, ok2 := xx.Args[i].(*ast.Ident)
			if ok2 {
				ident = []byte(id2.Name)
			}
			c.AstTree[o(where)+uint64(i)+1] = mapast.ExpressionNode(mapast.ExpressionIdentifier, 1)
			if ok2 {
				c.AstTree[o(o(where)+uint64(i)+1)] = ident
			} else {
				stack = append([]uint64{(o(where) + uint64(i) + 1)}, stack...)
			}
		}
		if len(c.typefield) > 0 {
			c.typefield = c.typefield[0 : len(c.typefield)-1]
		}
		c.typefield = append(c.typefield, stack...)

	case *ast.IncDecStmt:
		var xx = (x).(*ast.IncDecStmt)
		if c.skippedincdecs > 0 {
			c.skippedincdecs--
			break
		}
		if _, ok := c.deadincdecs[xx]; ok {
			break
		}
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.substmts[len(c.substmts)-1]--
		id, ok := xx.X.(*ast.Ident)
		var ident []byte
		if ok {
			ident = []byte(id.Name)
		}
		var stack []uint64
		var blk = c.nowblock[len(c.nowblock)-1]
		if xx.Tok == token.INC {
			c.AstTree[blk] = mapast.IncDecStmtNode(mapast.IncDecStmtPlusPlus)
		} else {
			c.AstTree[blk] = mapast.IncDecStmtNode(mapast.IncDecStmtMinusMinus)
		}
		if ok {
			c.AstTree[(o(blk))] = ident
		} else {
			stack = append([]uint64{(o(blk))}, stack...)
		}
		c.nowblock[len(c.nowblock)-1]++
		c.typefield = append(c.typefield, stack...)

	case *ast.GoStmt:
		for len(c.substmts) > 0 && len(c.subblocks) > 0 && c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		if len(c.substmts) > 0 {
			c.substmts[len(c.substmts)-1]--
		}
		var stack []uint64
		var blk = c.nowblock[len(c.nowblock)-1]
		c.AstTree[blk] = mapast.GoDferStmtNode(mapast.GoDferStmtGo)
		stack = append([]uint64{(o(blk))}, stack...)
		c.nowblock[len(c.nowblock)-1]++
		c.typefield = append(c.typefield, stack...)

	case *ast.DeferStmt:
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.substmts[len(c.substmts)-1]--
		var stack []uint64
		var blk = c.nowblock[len(c.nowblock)-1]
		c.AstTree[blk] = mapast.GoDferStmtNode(mapast.GoDferStmtDefer)
		stack = append([]uint64{(o(blk))}, stack...)
		c.nowblock[len(c.nowblock)-1]++
		c.typefield = append(c.typefield, stack...)

	case *ast.SendStmt:
		var xx = (x).(*ast.SendStmt)
		if c.skippedsends > 0 {
			c.skippedsends--
			break
		}
		if _, ok := c.deadsends[xx]; ok {
			break
		}
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.substmts[len(c.substmts)-1]--
		id1, ok1 := xx.Chan.(*ast.Ident)
		var ident1 []byte
		if ok1 {
			ident1 = []byte(id1.Name)
		}
		id2, ok2 := xx.Value.(*ast.Ident)
		var ident2 []byte
		if ok2 {
			ident2 = []byte(id2.Name)
		}
		_ = ok1
		_ = ok2
		_ = ident1
		_ = ident2
		var stack []uint64
		var blk = c.nowblock[len(c.nowblock)-1]
		c.AstTree[blk] = mapast.ExpressionNode(mapast.ExpressionArrow, 2)
		if ok1 {
			c.AstTree[(o(blk))] = ident1
		} else {
			stack = append([]uint64{(o(blk))}, stack...)
		}
		if ok2 {
			c.AstTree[(o(blk) + 1)] = ident2
		} else {
			stack = append([]uint64{(o(blk) + 1)}, stack...)
		}
		c.nowblock[len(c.nowblock)-1]++
		c.typefield = append(c.typefield, stack...)

	case *ast.LabeledStmt:
		var xx = (x).(*ast.LabeledStmt)
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.substmts[len(c.substmts)-1]--
		var blk = c.nowblock[len(c.nowblock)-1]
		c.AstTree[blk] = mapast.LblGotoCntNode(mapast.LblGotoCntLabel)
		c.AstTree[o(blk)] = []byte(xx.Label.Name)
		c.nowblock[len(c.nowblock)-1]++

	case *ast.AssignStmt:
		var xx = (x).(*ast.AssignStmt)
		if c.skippedassignments > 0 {
			c.skippedassignments--
			break
		}
		if _, ok := c.deadassignments[xx]; ok {
			break
		}
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.substmts[len(c.substmts)-1]--
		var variant byte
		switch xx.Tok {
		case token.ASSIGN:
			variant = mapast.AssignStmtEqual

		case token.DEFINE:
			variant = mapast.AssignStmtColonEq

		case token.ADD_ASSIGN:
			variant = mapast.AssignStmtAdd

		case token.SUB_ASSIGN:
			variant = mapast.AssignStmtSub

		case token.MUL_ASSIGN:
			variant = mapast.AssignStmtMul

		case token.QUO_ASSIGN:
			variant = mapast.AssignStmtQuo

		case token.REM_ASSIGN:
			variant = mapast.AssignStmtRem

		case token.AND_ASSIGN:
			variant = mapast.AssignStmtAnd

		case token.OR_ASSIGN:
			variant = mapast.AssignStmtOr

		case token.XOR_ASSIGN:
			variant = mapast.AssignStmtXor

		case token.SHL_ASSIGN:
			variant = mapast.AssignStmtShl

		case token.SHR_ASSIGN:
			variant = mapast.AssignStmtShr

		case token.AND_NOT_ASSIGN:
			variant = mapast.AssignStmtAndNot

		}
		if len(xx.Lhs) != len(xx.Rhs) {
			variant += mapast.AssignStmtMoreEqual
		}
		var stack []uint64
		var blk = c.nowblock[len(c.nowblock)-1]
		c.AstTree[blk] = mapast.AssignStmtNode(variant, uint64(len(xx.Lhs)+len(xx.Rhs)))
		for i := range xx.Lhs {
			var ident []byte
			id2, ok2 := xx.Lhs[i].(*ast.Ident)
			if ok2 {
				ident = []byte(id2.Name)
			}
			c.AstTree[o(blk)+uint64(i)] = mapast.ExpressionNode(mapast.ExpressionIdentifier, 1)
			if ok2 {
				c.AstTree[o(o(blk)+uint64(i))] = ident
			} else {
				stack = append([]uint64{(o(blk) + uint64(i))}, stack...)
			}
		}
		for i := range xx.Rhs {
			var ident []byte
			id2, ok2 := xx.Rhs[i].(*ast.Ident)
			if ok2 {
				ident = []byte(id2.Name)
			}
			c.AstTree[o(blk)+uint64(i+len(xx.Lhs))] = mapast.ExpressionNode(mapast.ExpressionIdentifier, 1)
			if ok2 {
				c.AstTree[o(o(blk)+uint64(i+len(xx.Lhs)))] = ident
			} else {
				stack = append([]uint64{(o(blk) + uint64(i+len(xx.Lhs)))}, stack...)
			}
		}
		c.nowblock[len(c.nowblock)-1]++
		c.typefield = append(c.typefield, stack...)

	case *ast.IndexExpr:
		var xx = (x).(*ast.IndexExpr)
		if len(c.typefield) == 0 {
			break
		}
		id1, ok1 := xx.X.(*ast.Ident)
		var ident1 []byte
		if ok1 {
			ident1 = []byte(id1.Name)
		}
		id2, ok2 := xx.Index.(*ast.Ident)
		var ident2 []byte
		if ok2 {
			ident2 = []byte(id2.Name)
		}
		_ = ok1
		_ = ok2
		_ = ident1
		_ = ident2
		var stack []uint64
		var where = c.typefield[len(c.typefield)-1]
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		c.AstTree[where] = mapast.ExpressionNode(mapast.ExpressionIndex, 2)
		if ok1 {
			c.AstTree[o(where)] = ident1
		} else {
			stack = append([]uint64{o(where)}, stack...)
		}
		if ok2 {
			c.AstTree[o(where)+1] = ident2
		} else {
			stack = append([]uint64{o(where) + 1}, stack...)
		}
		c.typefield = append(c.typefield, stack...)

	case *ast.SliceExpr:
		var xx = (x).(*ast.SliceExpr)
		if len(c.typefield) == 0 {
			break
		}
		id1, ok1 := xx.X.(*ast.Ident)
		var ident1 []byte
		if ok1 {
			ident1 = []byte(id1.Name)
		}
		id2, ok2 := xx.Low.(*ast.Ident)
		var ident2 []byte
		if ok2 {
			ident2 = []byte(id2.Name)
		}
		if xx.Low == nil {
			ok2 = true
		}
		id3, ok3 := xx.High.(*ast.Ident)
		var ident3 []byte
		if ok3 {
			ident3 = []byte(id3.Name)
		}
		id4, ok4 := xx.Max.(*ast.Ident)
		var ident4 []byte
		if ok4 {
			ident4 = []byte(id4.Name)
		}
		_ = ident3
		_ = ident4
		var stack []uint64
		var where = c.typefield[len(c.typefield)-1]
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		if xx.Slice3 {
			c.AstTree[where] = mapast.ExpressionNode(mapast.ExpressionSlice, 4)
		} else if xx.High == nil {
			c.AstTree[where] = mapast.ExpressionNode(mapast.ExpressionSlice, 2)
		} else {
			c.AstTree[where] = mapast.ExpressionNode(mapast.ExpressionSlice, 3)
		}
		if ok1 {
			c.AstTree[o(where)] = ident1
		} else {
			stack = append([]uint64{o(where)}, stack...)
		}
		if ok2 {
			if xx.Low == nil {
				c.AstTree[o(where)+1] = []byte("0")
			} else {
				c.AstTree[o(where)+1] = ident2
			}
		} else {
			stack = append([]uint64{o(where) + 1}, stack...)
		}
		if xx.High != nil {
			if ok3 {
				c.AstTree[o(where)+2] = ident3
			} else {
				stack = append([]uint64{o(where) + 2}, stack...)
			}
		}
		if xx.Slice3 {
			if ok4 {
				c.AstTree[o(where)+3] = ident4
			} else {
				stack = append([]uint64{o(where) + 3}, stack...)
			}
		}
		c.typefield = append(c.typefield, stack...)

	case *ast.ArrayType:
		var xx = (x).(*ast.ArrayType)
		if len(c.typefield) == 0 {
			break
		}
		var l uint64
		var variant byte
		if xx.Len == nil {
			l = 1
			variant = mapast.ExpressionSliceType
		} else {
			l = 2
			variant = mapast.ExpressionArrayType
		}
		id1, ok1 := xx.Len.(*ast.Ident)
		var ident1 []byte
		if ok1 {
			ident1 = []byte(id1.Name)
		}
		id2, ok2 := xx.Elt.(*ast.Ident)
		var ident2 []byte
		if ok2 {
			ident2 = []byte(id2.Name)
		}
		_ = variant
		_ = ident2
		_ = ident1
		var stack []uint64
		var where = c.typefield[len(c.typefield)-1]
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		c.AstTree[where] = mapast.ExpressionNode(variant, l)
		if xx.Len != nil {
			if ok1 {
				c.AstTree[o(where)] = ident1
			} else {
				stack = append([]uint64{o(where)}, stack...)
			}
		}
		if ok2 {
			c.AstTree[o(where)+l-1] = ident2
		} else {
			stack = append([]uint64{o(where) + l - 1}, stack...)
		}
		c.typefield = append(c.typefield, stack...)

	case *ast.KeyValueExpr:
		var xx = (x).(*ast.KeyValueExpr)
		if len(c.typefield) == 0 {
			break
		}
		id1, ok1 := xx.Key.(*ast.Ident)
		var ident1 []byte
		if ok1 {
			ident1 = []byte(id1.Name)
		}
		id2, ok2 := xx.Value.(*ast.Ident)
		var ident2 []byte
		if ok2 {
			ident2 = []byte(id2.Name)
		}
		var stack []uint64
		var where = c.typefield[len(c.typefield)-1]
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		c.AstTree[where] = mapast.ExpressionNode(mapast.ExpressionKeyVal, 2)
		if ok1 {
			c.AstTree[o(where)] = ident1
		} else {
			stack = append([]uint64{o(where)}, stack...)
		}
		if ok2 {
			c.AstTree[o(where)+1] = ident2
		} else {
			stack = append([]uint64{o(where) + 1}, stack...)
		}
		c.typefield = append(c.typefield, stack...)

	case *ast.CompositeLit:
		var xx = (x).(*ast.CompositeLit)
		if len(c.typefield) == 0 {
			break
		}
		var numtypes uint64
		if xx.Type != nil {
			numtypes = 1
		}
		id1, ok1 := xx.Type.(*ast.Ident)
		var ident1 []byte
		if ok1 {
			ident1 = []byte(id1.Name)
		}
		var stack []uint64
		var where = c.typefield[len(c.typefield)-1]
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		if numtypes == 1 {
			c.AstTree[where] = mapast.ExpressionNode(mapast.ExpressionComposite, 1+uint64(len(xx.Elts)))
			c.AstTree[o(where)] = mapast.RootOfType
			if ok1 {
				c.AstTree[o(o(where))] = ident1
			} else {
				stack = append([]uint64{o(o(where))}, stack...)
			}
		} else {
			c.AstTree[where] = mapast.ExpressionNode(mapast.ExpressionComposed, 0+uint64(len(xx.Elts)))
		}
		for i := range xx.Elts {
			id2, ok2 := xx.Elts[i].(*ast.Ident)
			var ident2 []byte
			if ok2 {
				ident2 = []byte(id2.Name)
			}
			_ = ident2
			if ok2 {
				c.AstTree[o(where)+uint64(i)+numtypes] = ident2
			} else {
				stack = append([]uint64{o(where) + uint64(i) + numtypes}, stack...)
			}
		}
		c.typefield = append(c.typefield, stack...)

	case *ast.TypeAssertExpr:
		var xx = (x).(*ast.TypeAssertExpr)
		if len(c.typefield) == 0 {
			break
		}
		id1, ok1 := xx.X.(*ast.Ident)
		var ident1 []byte
		if ok1 {
			ident1 = []byte(id1.Name)
		}
		var stack []uint64
		var where = c.typefield[len(c.typefield)-1]
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		if xx.Type == nil {
			c.AstTree[where] = mapast.ExpressionNode(mapast.ExpressionType, 1)
			if ok1 {
				c.AstTree[o(where)] = ident1
			} else {
				stack = append([]uint64{o(where)}, stack...)
			}
		} else {
			c.AstTree[where] = mapast.ExpressionNode(mapast.ExpressionType, 2)
			id2, ok2 := xx.Type.(*ast.Ident)
			var ident2 []byte
			if ok2 {
				ident2 = []byte(id2.Name)
			}
			_ = ident2
			if ok1 {
				c.AstTree[o(where)] = ident1
			} else {
				stack = append([]uint64{o(where)}, stack...)
			}
			c.AstTree[o(where)+1] = mapast.RootOfType
			if ok2 {
				c.AstTree[o(o(where)+1)] = ident2
			} else {
				stack = append([]uint64{o(o(where) + 1)}, stack...)
			}
		}
		c.typefield = append(c.typefield, stack...)

	case *ast.StructType:
		var xx = (x).(*ast.StructType)
		if len(c.typefield) == 0 {
			break
		}
		_ = xx
		var where = c.typefield[len(c.typefield)-1]
		c.AstTree[where] = mapast.StructType
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		var fieldscount = uint64(len(xx.Fields.List))
		if fieldscount > 0 {
			c.structfield = append(c.structfield, [2]uint64{o(where), fieldscount})
		}

	case *ast.MapType:
		var xx = (x).(*ast.MapType)
		if len(c.typefield) == 0 {
			break
		}
		id1, ok1 := xx.Key.(*ast.Ident)
		var ident1 []byte
		if ok1 {
			ident1 = []byte(id1.Name)
		}
		id2, ok2 := xx.Value.(*ast.Ident)
		var ident2 []byte
		if ok2 {
			ident2 = []byte(id2.Name)
		}
		_ = ident2
		_ = ident1
		var stack []uint64
		var where = c.typefield[len(c.typefield)-1]
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		c.AstTree[where] = mapast.ExpressionNode(mapast.ExpressionMap, 2)
		if ok1 {
			c.AstTree[o(where)] = ident1
		} else {
			stack = append([]uint64{o(where)}, stack...)
		}
		if ok2 {
			c.AstTree[o(where)+1] = ident2
		} else {
			stack = append([]uint64{o(where) + 1}, stack...)
		}
		c.typefield = append(c.typefield, stack...)

	case *ast.FuncLit:
		var xx = (x).(*ast.FuncLit)
		if len(c.typefield) == 0 {
			break
		}
		var t = c.typefield[len(c.typefield)-1]
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		var results = 0
		if xx.Type.Results != nil {
			results = len(xx.Type.Results.List)
		}
		var dimension = uint64(len(xx.Type.Params.List) + results)
		if dimension > 0 {
			c.structfield = append(c.structfield, [2]uint64{(o(t)), dimension})
		}
		c.AstTree[t] = mapast.ClosureExpNode(uint64(len(xx.Type.Params.List)))
		c.AstTree[o(t)+dimension] = mapast.BlocOfCodeNode(mapast.BlocOfCodePlain, 0)
		c.blocksstmts[xx.Body] = o(o(t) + dimension)
		c.nowblock = append(c.nowblock, o(o(t)+dimension))
		c.subblocks = append(c.subblocks, how_many_subblocks_block(xx.Body))
		c.substmts = append(c.substmts, how_many_substmts_block(xx.Body))
		c.deadfunc[xx.Type] = struct{}{}

	case *ast.InterfaceType:
		var xx = (x).(*ast.InterfaceType)
		if len(c.typefield) == 0 {
			break
		}
		_ = xx
		var where = c.typefield[len(c.typefield)-1]
		c.AstTree[where] = mapast.IfceTypExp
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		var stack []uint64
		var structstack [][2]uint64
		for i := range xx.Methods.List {
			switch xxx := xx.Methods.List[i].Type.(type) {
			case *ast.FuncType:
				c.deadfunc[xxx] = struct{}{}
				var npars = 0
				var nrets = 0
				if xx.Methods.List[i].Type.(*ast.FuncType).Params != nil {
					npars = len(xx.Methods.List[i].Type.(*ast.FuncType).Params.List)
				}
				if xx.Methods.List[i].Type.(*ast.FuncType).Results != nil {
					nrets = len(xx.Methods.List[i].Type.(*ast.FuncType).Results.List)
				}
				c.AstTree[o(where)+uint64(i)] = mapast.IfceMethod[0 : npars+1]
				_ = nrets
				structstack = append([][2]uint64{{o(o(where) + uint64(i)), uint64(1 + npars + nrets)}}, structstack...)

			case *ast.Ident:
				c.AstTree[o(where)+uint64(i)] = mapast.RootOfType
				c.AstTree[o(o(where)+uint64(i))] = []byte(xx.Methods.List[i].Type.(*ast.Ident).Name)
				structstack = append([][2]uint64{{0, 0}}, structstack...)

			case *ast.SelectorExpr:
				c.AstTree[o(where)+uint64(i)] = mapast.RootOfType
				stack = append([]uint64{o(o(where) + uint64(i))}, stack...)
				structstack = append([][2]uint64{{0, 0}}, structstack...)

			default:

			}
		}
		c.structfield = append(c.structfield, structstack...)
		c.typefield = append(c.typefield, stack...)

	case *ast.SelectStmt:
		var xx = (x).(*ast.SelectStmt)
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.subblocks[len(c.subblocks)-1]--
		var t = c.nowblock[len(c.nowblock)-1]
		c.AstTree[t] = mapast.BlocOfCodeNode(mapast.BlocOfCodeSelect, 0)
		c.nowblock[len(c.nowblock)-1]++
		c.blocksstmts[xx.Body] = o(t)
		c.nowblock = append(c.nowblock, o(t))
		c.subblocks = append(c.subblocks, how_many_subblocks_block(xx.Body))
		c.substmts = append(c.substmts, 0)

	case *ast.CommClause:
		var xx = (x).(*ast.CommClause)
		for c.substmts[len(c.substmts)-1] <= 0 && c.subblocks[len(c.subblocks)-1] <= 0 {
			c.substmts = c.substmts[0 : len(c.substmts)-1]
			c.subblocks = c.subblocks[0 : len(c.subblocks)-1]
			c.nowblock = c.nowblock[0 : len(c.nowblock)-1]
		}
		c.subblocks[len(c.subblocks)-1]--
		var t = c.nowblock[len(c.nowblock)-1]
		c.nowblock[len(c.nowblock)-1]++
		var theadcount uint64
		var stack []uint64
		if xx.Comm == nil {
			c.AstTree[t] = mapast.BlocOfCodeNode(mapast.BlocOfCodeCommunicateDefault, 0)
		} else {
			c.AstTree[t] = mapast.BlocOfCodeNode(mapast.BlocOfCodeCommunicate, 1)
			switch xx.Comm.(type) {
			case *ast.SendStmt:
				xxx := xx.Comm.(*ast.SendStmt)
				id1, ok1 := xxx.Chan.(*ast.Ident)
				var ident1 []byte
				if ok1 {
					ident1 = []byte(id1.Name)
				}
				id2, ok2 := xxx.Value.(*ast.Ident)
				var ident2 []byte
				if ok2 {
					ident2 = []byte(id2.Name)
				}
				_ = ident1
				_ = ident2
				c.AstTree[o(t)+theadcount] = mapast.ExpressionNode(mapast.ExpressionArrow, 2)
				if ok1 {
					c.AstTree[o(o(t)+theadcount)] = ident1
				} else {
					stack = append([]uint64{o(o(t) + theadcount)}, stack...)
				}
				if ok2 {
					c.AstTree[o(o(t)+theadcount)+1] = ident2
				} else {
					stack = append([]uint64{o(o(t)+theadcount) + 1}, stack...)
				}
				theadcount++
				c.skippedsends++

			case *ast.ExprStmt:
				id, ok := xx.Comm.(*ast.ExprStmt).X.(*ast.Ident)
				var ident []byte
				if ok {
					ident = []byte(id.Name)
				}
				_ = ident
				c.AstTree[o(t)] = mapast.ExpressionNode(mapast.ExpressionBrackets, 1)
				if ok {
					c.AstTree[o(o(t))] = ident
				} else {
					stack = append([]uint64{o(o(t))}, stack...)
				}
				theadcount++
				c.skippedexpressions++

			case *ast.AssignStmt:
				xxx := xx.Comm.(*ast.AssignStmt)
				var variant byte
				switch xxx.Tok {
				case token.ASSIGN:
					variant = mapast.AssignStmtEqual

				case token.DEFINE:
					variant = mapast.AssignStmtColonEq

				case token.ADD_ASSIGN:
					variant = mapast.AssignStmtAdd

				case token.SUB_ASSIGN:
					variant = mapast.AssignStmtSub

				case token.MUL_ASSIGN:
					variant = mapast.AssignStmtMul

				case token.QUO_ASSIGN:
					variant = mapast.AssignStmtQuo

				case token.REM_ASSIGN:
					variant = mapast.AssignStmtRem

				case token.AND_ASSIGN:
					variant = mapast.AssignStmtAnd

				case token.OR_ASSIGN:
					variant = mapast.AssignStmtOr

				case token.XOR_ASSIGN:
					variant = mapast.AssignStmtXor

				case token.SHL_ASSIGN:
					variant = mapast.AssignStmtShl

				case token.SHR_ASSIGN:
					variant = mapast.AssignStmtShr

				case token.AND_NOT_ASSIGN:
					variant = mapast.AssignStmtAndNot

				}
				if len(xxx.Lhs) != len(xxx.Rhs) {
					variant += mapast.AssignStmtMoreEqual
				}
				var l = uint64(len(xxx.Lhs))
				c.AstTree[o(t)+theadcount] = mapast.AssignStmtNode(variant, uint64(len(xxx.Lhs)+len(xxx.Rhs)))
				for i := range xxx.Lhs {
					var ident []byte
					id, ok := xxx.Lhs[i].(*ast.Ident)
					if ok {
						ident = []byte(id.Name)
					}
					if ok {
						c.AstTree[(o(o(t)+theadcount) + uint64(i))] = ident
					} else {
						stack = append([]uint64{(o(o(t)+theadcount) + uint64(i))}, stack...)
					}
				}
				for i := range xxx.Rhs {
					var ident []byte
					id, ok := xxx.Rhs[i].(*ast.Ident)
					if ok {
						ident = []byte(id.Name)
					}
					if ok {
						c.AstTree[(o(o(t)+theadcount) + uint64(i) + l)] = ident
					} else {
						stack = append([]uint64{(o(o(t)+theadcount) + uint64(i) + l)}, stack...)
					}
				}
				theadcount++
				c.skippedassignments++

			}
		}
		c.typefield = append(c.typefield, stack...)
		var subs = how_many_subblocks_stmt_list(xx.Body)
		var sus = how_many_substmts_stmt_list(xx.Body)
		c.nowblock = append(c.nowblock, o(t)+(theadcount))
		c.subblocks = append(c.subblocks, subs)
		c.substmts = append(c.substmts, sus)

	case *ast.ChanType:
		var xx = (x).(*ast.ChanType)
		if len(c.typefield) == 0 {
			break
		}
		id, ok := xx.Value.(*ast.Ident)
		var ident []byte
		var variant = mapast.ExpressionChan
		switch xx.Dir {
		case 1:
			variant = mapast.ExpressionInChan

		case 2:
			variant = mapast.ExpressionOutChan

		}
		if ok {
			ident = []byte(id.Name)
		}
		var where = c.typefield[len(c.typefield)-1]
		c.AstTree[where] = mapast.ExpressionNode(variant, 1)
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		if ok {
			c.AstTree[o(where)] = ident
		} else {
			c.typefield = append(c.typefield, o(where))
		}

	case *ast.FuncType:
		var xx = (x).(*ast.FuncType)
		if len(c.typefield) == 0 {
			break
		}
		_, ok := c.deadfunc[xx]
		if ok {
			break
		}
		var t = c.typefield[len(c.typefield)-1]
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		var results = 0
		if xx.Results != nil {
			results = len(xx.Results.List)
		}
		var dimension = uint64(len(xx.Params.List) + results)
		if dimension > 0 {
			c.structfield = append(c.structfield, [2]uint64{(o(t)), dimension})
		}
		c.AstTree[t] = mapast.ClosureExpNode(uint64(len(xx.Params.List)))

	case *ast.Ellipsis:
		if c.skippedellipsis > 0 {
			c.skippedellipsis--
			break
		}
		if len(c.typefield) == 0 {
			break
		}
		var t = c.typefield[len(c.typefield)-1]
		c.typefield = c.typefield[0 : len(c.typefield)-1]
		c.AstTree[t] = []byte("...")

	case *ast.VoidType:
		var xx = (x).(*ast.VoidType)
		var where = c.typefield[len(c.typefield)-1]
		if xx.Under {
			c.AstTree[where] = mapast.GenericExp[0:2]
		} else {
			c.AstTree[where] = mapast.GenericExp[0:1]
		}
		c.typefield = c.typefield[0 : len(c.typefield)-1]

	default:

	}
	return c
}
