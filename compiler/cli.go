package compiler

import (
	"os"
	"steel/lib"
	"time"

	"github.com/urfave/cli"
)

func prepareArgs() *cli.App {
	cli.AppHelpTemplate = `NAME:
	{{.Name}} - {{.Usage}}

USAGE:
	{{if .VisibleFlags}}{{.HelpName}} [options] [files]

	* Use without argument to run instead of compiling it
	* Use '-c' to compile to native binary.
	* Use '-o' or '-i' to compile to ASM or ELF Objects.
	* Files must be steel source file and end with '.st'{{end}}
	{{if len .Authors}}
AUTHOR:
	{{range .Authors}}{{ . }}{{end}}
	{{end}}{{if .Commands}}
VERSION:
	{{.Version}}

OPTIONS:
	{{range .VisibleFlags}}{{.}}
	{{end}}{{end}}{{if .Copyright }}

COPYRIGHT:
	{{.Copyright}}
	{{end}}{{if .Version}}
	{{end}}`

	cli.VersionFlag = cli.BoolFlag{
		Name:  "V, version",
		Usage: "Print version",
	}

	cli.HelpFlag = cli.BoolFlag{
		Name:  "h, help",
		Usage: "Print help",
	}

	app := cli.NewApp()

	app.Name = "Steel"
	app.Version = "0.0.1"
	app.Compiled = time.Now()

	app.Usage = "Strongly Typed Experimental Expressive Language"

	app.Flags = []cli.Flag{
		cli.StringFlag{
			Name:  "O, output",
			Value: "a.out",
			Usage: "Output program `name` (add -c)",
		},
		cli.BoolFlag{
			Name:  "c, compile",
			Usage: "Compile and link to native binary",
		},
		cli.BoolFlag{
			Name:  "C, clean",
			Usage: "Same as -c but clean .o file after link. Disallow for Cache",
		},
		cli.BoolFlag{
			Name:  "o, to-obj",
			Usage: "Compile to separate Object files (.o). Do not link",
		},
		cli.BoolFlag{
			Name:  "i, to-ir",
			Usage: "Compile to separate LLVM IR. Do not link (SOON)",
		},
		cli.BoolFlag{
			Name:  "p, print",
			Usage: "Print instead of write on disk. Do not link. (must use with -o or -i) ",
		},
		cli.BoolFlag{
			Name:  "a, ast",
			Usage: "Print AST",
		},
		cli.BoolFlag{
			Name:  "q, quiet",
			Usage: "Quiet",
		},
		cli.BoolFlag{
			Name:  "v, verbose",
			Usage: "Verbose",
		},
	}

	app.UsageText = "steel [options] files"

	return app
}

func manageArgs() {
	app := prepareArgs()

	app.Action = func(c *cli.Context) error {
		files := c.Args()

		if len(files) == 0 {
			return cli.NewExitError("Usage: steel [options] files", 1)
		}

		compileFiles(files, steel.SteelOptions{
			ToObj:    c.Bool("o"),
			ToIr:     c.Bool("i"),
			ToStdOut: c.Bool("p"),
			Clean:    c.Bool("C"),
			PrintAst: c.Bool("a"),
			Quiet:    c.Bool("q"),
			Verbose:  c.Bool("v"),
			Output:   c.String("O"),
			Compile:  c.Bool("c"),
		})

		return nil
	}

	app.Run(os.Args)
}
