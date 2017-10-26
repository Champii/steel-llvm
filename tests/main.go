package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"strings"

	"github.com/fatih/color"
)

func filterByExtension(files []os.FileInfo, ext string) []string {
	var res []string

	for _, item := range files {
		splitted := strings.Split(item.Name(), ".")
		if splitted[len(splitted)-1] == ext {
			res = append(res, item.Name())
		}
	}

	return res
}

func doesOutMatch(o1, o2 []byte) bool {
	if len(o1) != len(o2) {
		return false
	}

	for i, b := range o1 {
		if b != o2[i] {
			return false
		}
	}

	return true
}

func main() {
	files, err := ioutil.ReadDir("./scenarios")

	if err != nil {
		log.Fatal("Cannot read dir ", err)
	}

	scripts := filterByExtension(files, "st")
	outs := filterByExtension(files, "out")

	if len(scripts) != len(outs) {
		log.Fatal("Some scripts lack an out file")
	}

	failed := 0

	for i, filename := range scripts {

		fmt.Printf("   %s\r", filename)

		errCmpl := exec.Command("../steel", "-C", "scenarios/"+filename).Run()

		if errCmpl != nil {
			color.Red("KO")
			fmt.Println("Compilation error ", errCmpl)
			failed++
			continue
		}

		cmd := exec.Command("./a.out")

		out, err := cmd.Output()

		if err != nil {
			color.Red("KO")
			fmt.Println("Runtime error ", err)
			failed++
			continue
		}

		expectedOut, err2 := ioutil.ReadFile("./scenarios/" + outs[i])

		if err2 != nil {
			color.Red("KO")
			fmt.Println(err2)
			failed++
			continue
		}

		if doesOutMatch(out, expectedOut) {
			color.Green("OK")
		} else {
			color.Red("KO")
			fmt.Println("---\nExpected: ", expectedOut, "\nGot:      ", out, "\n---")
			failed++
		}
	}

	fmt.Print("\nSucceed: ")
	color.Green("%d", len(scripts)-failed)
	fmt.Print("Failed:  ")
	color.Red("%d", failed)

	fmt.Print("\n---\nResult:  ")
	if failed > 0 {
		color.Red("Failed")
	} else {
		color.Green("Success")
	}

}
