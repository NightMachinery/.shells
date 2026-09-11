package main

import (
	"reflect"
	"testing"
)

func TestResolveMaps(t *testing.T) {
	cases := []struct {
		name   string
		in     map[int]int
		termux bool
		want   map[int]int
	}{
		{
			name:   "termux alone",
			termux: true,
			want:   map[int]int{1003: 1002},
		},
		{
			name:   "termux adds to other maps",
			in:     map[int]int{1015: 1006},
			termux: true,
			want:   map[int]int{1015: 1006, 1003: 1002},
		},
		{
			name:   "explicit map for the same mode wins",
			in:     map[int]int{1003: 1000},
			termux: true,
			want:   map[int]int{1003: 1000},
		},
		{
			name: "without termux nothing is added",
			in:   map[int]int{1015: 1006},
			want: map[int]int{1015: 1006},
		},
		{
			name: "no maps at all stays nil",
			want: nil,
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			got := resolveMaps(c.in, c.termux)
			if !reflect.DeepEqual(got, c.want) {
				t.Fatalf("resolveMaps(%v, %v) = %v, want %v", c.in, c.termux, got, c.want)
			}
		})
	}
}
