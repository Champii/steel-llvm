package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"steel/lib"
	"strings"

	"llvm.org/llvm/bindings/go/llvm"
)

var typeAssoc map[string]llvm.Type

func getFile(name string) string {
	file, err := os.Open(os.Args[1])

	if err != nil {
		log.Panic(err)
	}

	buffer, err := ioutil.ReadAll(file)

	if err != nil {
		log.Panic(err)
	}

	file.Close()

	return string(buffer)

}

func main() {
	// test()
	// return

	typeAssoc = make(map[string]llvm.Type)

	typeAssoc["void"] = llvm.VoidType()
	typeAssoc["int"] = llvm.Int32Type()
	typeAssoc["int32"] = llvm.Int32Type()
	typeAssoc["int16"] = llvm.Int16Type()
	typeAssoc["int8"] = llvm.Int8Type()
	typeAssoc["int1"] = llvm.Int1Type()
	// typeAssoc["string"] = llvm.Int8Type()
	typeAssoc["string"] = llvm.PointerType(llvm.Int8Type(), 0)

	if len(os.Args) < 2 {
		fmt.Printf("Usage: %v FILE\n", os.Args[0])
		os.Exit(1)
	}

	file := getFile(os.Args[1])

	destFile, err := os.Create(strings.Split(os.Args[1], ".")[0] + ".o")

	if err != nil {
		panic(err)
	}

	s := steel.Steel{}

	mb, err := s.CompileFile(file, steel.CompilerOptions{})

	if err != nil {
		log.Panic(err)
	}

	destFile.Write(mb)

	cmd := exec.Command("clang", "-o", "test/test", "test/test.o")
	err = cmd.Run()

	if err != nil {
		log.Panic(err)
	}
}
