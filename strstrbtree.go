package main

type StrStrBinTree struct {
	key string
	val string
	lnk [2]*StrStrBinTree
}

func StrStrBinTreeInsert(tree **StrStrBinTree, key string, val string) byte {
	if *tree == nil {
		*tree = &StrStrBinTree{key: key, val: val}
		return 0
	}
	var ptr = *tree
	var run int
	for {
		if (*ptr).key == key {
			if (*ptr).val == val {
				return 2
			} else {
				(*ptr).val = val
				return 3
			}
		} else if (*ptr).key < key {
			run = 0
		} else {
			run = 1
		}
		if (*ptr).lnk[run&1] == nil {
			(*ptr).lnk[run&1] = &StrStrBinTree{key: key, val: val}
			return 1
		}
		ptr = (*ptr).lnk[run]
	}
}

func StrStrBinTreeSelect(tree *StrStrBinTree, key string) string {
	for {
		if tree == nil {
			return ""
		}
		if tree.key == key {
			return tree.val
		}
		if tree.key < key {
			tree = tree.lnk[0]
		} else {
			tree = tree.lnk[1]
		}
	}
}

func StrStrBinTreeInorder(tree *StrStrBinTree, fun func(string, string)) {
	for {
		if tree == nil {
			return
		}
		StrStrBinTreeInorder(tree.lnk[0], fun)
		fun(tree.key, tree.val)
		tree = tree.lnk[1]
	}
}

func StrStrBinTreePreorder(tree *StrStrBinTree, fun func(string, string)) {
	for {
		if tree == nil {
			return
		}
		fun(tree.key, tree.val)
		StrStrBinTreeInorder(tree.lnk[0], fun)
		tree = tree.lnk[1]
	}
}
