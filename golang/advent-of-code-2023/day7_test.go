package main

import (
	"testing"
)

func TestGetType(t *testing.T) {
	test := []string{"9QQJ9", "JJTA2", "JJJJJ"}
	results := []int{FULL_HOUSE, THREE_OF_A_KIND, FIVE_OF_A_KIND}
	for i := 0; i < len(test); i++ {
		if getType(test[i]) != results[i] {
			t.Fatalf("%s should have been %d", test[i], results[i])
		}
	}
}
