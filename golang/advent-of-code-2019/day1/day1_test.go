package day1

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestTotalFuel(t *testing.T) {
	assert.Equal(t, 2, TotalFuel(14))
	assert.Equal(t, 966, TotalFuel(1969))
	assert.Equal(t, 50346, TotalFuel(100756))
}
