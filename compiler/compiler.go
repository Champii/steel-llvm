package compiler

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"steel/lib"
	"strings"

	"github.com/fatih/color"

	"llvm.org/llvm/bindings/go/llvm"
)

var typeAssoc map[string]llvm.Type

func getFile(name string) string {
	file, err := os.Open(name)

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

func linkFiles(filenames []string, opts steel.SteelOptions) {
	if opts.Verbose {
		fmt.Printf("---\n      Linking: %s\r", opts.Output)
	}

	cmd := exec.Command("clang", filenames...)

	err := cmd.Run()

	if err != nil {
		if opts.Verbose {
			color.Red("FAIL")
		}

		log.Panic(err, filenames)
	}

	if opts.Verbose {
		color.Green("OK")
		color.White("---")
	}

	if opts.Clean {
		cleanup(filenames[2:], opts)
	}
}

func expandFolder(filename string) []string {
	var res []string

	dir, err := ioutil.ReadDir(filename)

	if err != nil {
		log.Fatal("Cannot read directory ", filename)
	}

	for _, f := range dir {
		newFilename := filename + "/" + f.Name()
		stat, err := os.Stat(newFilename)

		if err != nil {
			log.Fatal("Cannot stat ", newFilename)
		}

		if stat.IsDir() {
			res = append(res, expandFolder(newFilename)...)
		} else {
			arr := strings.Split(newFilename, ".")
			if arr[len(arr)-1] == "st" || arr[len(arr)-1] == "o" {
				if arr[len(arr)-1] == "st" && hasFile(strings.Join(arr[:len(arr)-1], ".")+".o", res) {
					res = removeFile(strings.Join(arr[:len(arr)-1], ".")+".o", res)
				}
				res = append(res, newFilename)
			}
		}
	}

	return res
}

func hasFile(toFind string, filenames []string) bool {
	for _, file := range filenames {
		if toFind == file {
			return true
		}
	}

	return false
}

func removeFile(file string, filenames []string) []string {
	idx := -1

	for i, item := range filenames {
		if file == item {
			idx = i
		}
	}

	if idx > -1 {
		return append(filenames[:idx], filenames[idx+1:]...)
	}

	return filenames
}

func filterValidFiles(filenames []string) []string {
	var res []string

	for _, filename := range filenames {
		stat, err := os.Stat(filename)

		if err != nil {
			log.Fatal("Cannot stat ", filename)
		}

		if stat.IsDir() {
			res = append(res, expandFolder(filename)...)
		} else {
			arr := strings.Split(filename, ".")
			if arr[len(arr)-1] == "st" || arr[len(arr)-1] == "o" {
				if arr[len(arr)-1] == "st" && hasFile(strings.Join(arr[:len(arr)-1], ".")+".o", res) {
					res = removeFile(strings.Join(arr[:len(arr)-1], ".")+".o", res)
				}

				res = append(res, filename)
			}
		}

	}

	return res
}

func toFiles(filenames []string, opts steel.SteelOptions) {
	linkArgs := []string{
		"-o",
		opts.Output,
	}

	for _, filename := range filenames {
		arr := strings.Split(filename, ".")

		if arr[len(arr)-1] == "o" {
			if opts.Verbose {
				fmt.Printf("      %s\r", filename)
				color.Blue("USING ")
			}
			linkArgs = append(linkArgs, filename)
			continue
		}

		file := getFile(filename)

		objNameArr := strings.Split(filename, ".")
		objName := strings.Join(objNameArr[:len(objNameArr)-1], ".") + ".o"
		linkArgs = append(linkArgs, objName)

		destFile := os.Stdout
		recompile := true

		statSrcFile, err := os.Stat(filename)

		if err != nil {
			log.Fatal("Cannot stat file", filename)
		}

		srcDate := statSrcFile.ModTime()

		if !opts.ToStdOut {
			var err error

			err = nil
			statObjFile, err2 := os.Stat(objName)

			if err2 != nil {
				destFile, err = os.Create(objName)

				if err != nil {
					panic(err)
				}
			} else {
				objDate := statObjFile.ModTime()
				destFile, err = os.OpenFile(objName, os.O_RDWR, 0777)

				if err != nil {
					panic(err)
				}
				if srcDate.Equal(objDate) {
					recompile = false
				}
			}
		}

		if opts.Verbose {
			fmt.Printf("      %s -> %s\r", filename, objName)
		}

		if recompile {
			s := steel.Steel{Module: llvm.NewModule("steel" + filename)}

			mb, err := s.CompileFile(file, opts)

			if err != nil {
				color.Red("FAIL")
				log.Panic(err)
			}

			if opts.Verbose {
				color.Green("OK")
			}

			_, err = destFile.Write(mb)
			if err != nil {
				log.Fatal(err)
			}

			err = os.Chtimes(destFile.Name(), srcDate, srcDate)
			if err != nil {
				log.Fatal(err)
			}
			if destFile != os.Stdout {
				err = destFile.Close()
				if err != nil {
					log.Fatal(err)
				}
			}
		} else if opts.Verbose {
			color.Yellow("CACHE")
		}
	}

	if !opts.Compile {
		return
	}

	linkFiles(linkArgs, opts)
}

func toRun(filenames []string, opts steel.SteelOptions) {
	s := steel.Steel{Module: llvm.NewModule("steel")}

	for _, filename := range filenames {
		file := getFile(filename)

		if opts.Verbose {
			fmt.Printf("     %s\r", filename)
		}

		_, err := s.CompileFile(file, opts)

		if err != nil {
			if opts.Verbose {
				color.Red("FAIL")
			}

			log.Panic(err)
		}

		if opts.Verbose {
			color.Green("OK")
		}
	}
	if opts.Verbose {
		fmt.Print("---\n")
	}

	os.Exit(s.Run())
}

func compileFiles(_filenames []string, opts steel.SteelOptions) {
	filenames := filterValidFiles(_filenames)

	if opts.ToStdOut {
		opts.Verbose = false
	}

	opts.Run = false
	if !opts.ToObj && !opts.ToIr && !opts.ToStdOut && !opts.Compile {
		opts.Run = true
	}

	if opts.Output != "a.out" || opts.Clean {
		opts.Run = false
		opts.Compile = true
	}

	if opts.Run {
		toRun(filenames, opts)
	} else {
		toFiles(filenames, opts)
	}
}

func cleanup(files []string, opts steel.SteelOptions) {
	if opts.Verbose {
		fmt.Print("      Cleaning...\r")
	}
	for _, file := range files {
		if err := os.Remove(file); err != nil {
			color.Yellow("Cannot delete object file", file)
		}
	}

	if opts.Verbose {
		color.Green("OK")
		color.White("---")
	}
}

func Run() {
	manageArgs()
}
