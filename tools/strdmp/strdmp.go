package main

import (
	"flag"
	"fmt"
	"io/ioutil"
)

var m = map[rune]byte{
	'|':  0x01, // line break
	'&':  0xcb,
	'/':  0xcc,
	':':  0xcd,
	'#':  0xce,
	'C':  0xcf,
	'0':  0xd0,
	'1':  0xd1,
	'2':  0xd2,
	'3':  0xd3,
	'4':  0xd4,
	'5':  0xd5,
	'6':  0xd6,
	'7':  0xd7,
	'8':  0xd8,
	'9':  0xd9,
	'a':  0xda,
	'b':  0xdb,
	'c':  0xdc,
	'd':  0xdd,
	'e':  0xde,
	'f':  0xdf,
	'g':  0xe0,
	'h':  0xe1,
	'i':  0xe2,
	'j':  0xe3,
	'k':  0xe4,
	'l':  0xe5,
	'm':  0xe6,
	'n':  0xe7,
	'o':  0xe8,
	'p':  0xe9,
	'q':  0xea,
	'r':  0xeb,
	's':  0xec,
	't':  0xed,
	'u':  0xee,
	'v':  0xef,
	'w':  0xf0,
	'x':  0xf1,
	'y':  0xf2,
	'z':  0xf3,
	'.':  0xf4,
	',':  0xf5,
	'\'': 0xf6,
	'^':  0xf7, // "
	'$':  0xf8,
	'(':  0xf9,
	')':  0xfa,
	'!':  0xfb,
	'?':  0xfc,
	'-':  0xfd,
	' ':  0xff}

func main() {

	str := flag.String("s", "", "input string")
	out := flag.String("o", "", "output file")

	flag.Parse()

	var data []byte

	for _, c := range *str {

		id, found := m[c]

		if !found {
			panic("unknown")
		}

		data = append(data, id)
	}

	data = append(data, 0)

	if len(*out) == 0 {

		fmt.Print(".db ")

		for i, b := range data {
			fmt.Printf("$%02x", b)
			if i != len(data)-1 {
				fmt.Print(", ")
			}
		}

		return
	}

	if err := ioutil.WriteFile(*out, data, 0644); err != nil {
		panic(err)
	}
}
