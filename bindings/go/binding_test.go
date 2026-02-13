package tree_sitter_koka_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_koka "github.com/koka-community/tree-sitter-koka/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_koka.Language())
	if language == nil {
		t.Errorf("Error loading Koka grammar")
	}
}
