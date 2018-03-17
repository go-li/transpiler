// Copyright 2015 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package importer provides access to export data importers.
package importer

import (
	"github.com/go-li/transpiler/types"
)

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
	return &Foo{}
}
