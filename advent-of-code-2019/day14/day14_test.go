package day14

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParseLine(t *testing.T) {
	lhs, rhs := parseLine("7 A, 1 B => 1 C")
	assert.Len(t, lhs, 2)
	assert.Equal(t, rhs, Reagant{"C", 1})
}
