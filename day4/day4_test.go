package day4

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestValidPassword(t *testing.T) {
	assert.True(t, isValidPassword("111111", 0, 999999))
	assert.False(t, isValidPassword("223450", 0, 999999))
}
