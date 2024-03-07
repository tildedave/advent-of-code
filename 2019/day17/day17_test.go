package day17

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCompression(t *testing.T) {
	// Example from reddit.
	str := "R,4,R,12,R,10,L,12,L,12,R,4,R,12,L,12,R,4,R,12,L,12,L,8,R,10,L,12,L,8,R,10,R,4,R,12,R,10,L,12,L,12,R,4,R,12,L,12,R,4,R,12,L,12,L,8,R,10,R,4,R,12,R,10,L,12"
	result, _, _ := Compress(str)
	assert.True(t, result)

	str = "R,4,L,12,L,8,R,4,L,8,R,10,R,10,R,6,R,4,L,12,L,8,R,4,R,4,R,10,L,12,R,4,L,12,L,8,R,4,L,8,R,10,R,10,R,6,R,4,L,12,L,8,R,4,R,4,R,10,L,12,L,8,R,10,R,10,R,6,R,4,R,10,L,12"
	result, main, programs := Compress(str)
	fmt.Println(result, main, programs)
	assert.True(t, result)

}
