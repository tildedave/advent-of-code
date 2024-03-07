package day4

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestValidPassword(t *testing.T) {
	assert.True(t, isValidPassword("111111", false))
	assert.True(t, isValidPassword("111122", false))
	assert.False(t, isValidPassword("223450", false))
}

func TestValidPasswordPart2(t *testing.T) {
	assert.False(t, isValidPassword("111111", true))
	assert.True(t, isValidPassword("112233", true))
	assert.False(t, isValidPassword("123444", true))
	assert.True(t, isValidPassword("111122", true))
	assert.False(t, isValidPassword("123789", true))
	assert.True(t, isValidPassword("467889", true))
}
