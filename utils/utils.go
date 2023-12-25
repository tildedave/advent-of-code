package utils

import "strconv"

func ParseNumberList(strlist []string) ([]int, error) {
	ret := make([]int, 0)
	for _, nStr := range strlist {
		n, err := strconv.ParseInt(nStr, 10, 64)
		if err != nil {
			return []int{}, err
		}
		ret = append(ret, int(n))
	}

	return ret, nil
}
