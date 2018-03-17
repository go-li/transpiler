// Copyright 2015 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package importer provides access to export data importers.
package importer

import (
	"github.com/go-li/transpiler/internal/srcimporter"
	"github.com/go-li/transpiler/types"
	"go/build"
	"go/token"
	"io"
	"runtime"
)
import "github.com/gopherjs/gopherjs/js"

// A Lookup function returns a reader to access package data for
// a given import path, or an error if no matching package is found.
type Lookup func(path string) (io.ReadCloser, error)

// For returns an Importer for importing from installed packages
// for the compilers "gc" and "gccgo", or for importing directly
// from the source if the compiler argument is "source". In this
// latter case, importing may fail under circumstances where the
// exported API is not entirely defined in pure Go source code
// (if the package API depends on cgo-defined entities, the type
// checker won't have access to those).
//
// If lookup is nil, the default package lookup mechanism for the
// given compiler is used, and the resulting importer attempts
// to resolve relative and absolute import paths to canonical
// import path IDs before finding the imported file.
//
// If lookup is non-nil, then the returned importer calls lookup
// each time it needs to resolve an import path. In this mode
// the importer can only be invoked with canonical import paths
// (not relative or absolute ones); it is assumed that the translation
// to canonical import paths is being done by the client of the
// importer.
func For(compiler string, lookup Lookup) types.Importer {
	if lookup != nil {
		panic("source importer for custom import path lookup not supported (issue #13847).")
	}

	return srcimporter.New(&build.Default, token.NewFileSet(), make(map[string]*types.Package))
}

func Path2Name(n string) string {
	var j = -1
	for i := range n {
		if n[i] == '/' {
			j = i
		}
	}
	return n[j+1:]
}

type Foo struct{}

func (f *Foo) Import(path string) (*types.Package, error) {

	pkg := types.NewPackage(path, Path2Name(path))
	pkg.MarkComplete()
	pkg.MarkFake()

	return pkg, nil
}

// Default returns an Importer for the compiler that built the running binary.
// If available, the result implements types.ImporterFrom.
func Default() types.Importer {
	if js.Global != nil {
		return &Foo{}
	} else {
		return For(runtime.Compiler, nil)
	}
}
