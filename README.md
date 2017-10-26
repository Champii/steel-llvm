# steel-llvm
LLVM version of Steel

## Build
```
go build
```

## Exemple

```livescript
external
  puts([int8]): void

class Exemple
  a: int
  b: int

add = (i: int, j: int) -> i + j

main = ->
  str = "A String"
  puts(str)
  struct =
    a: 1
    b: 2
  c = add(struct.a, struct.b)
  0
```

## Usage

```
NAME:
  Steel - Strongly Typed Experimental Expressive Language

USAGE:
  steel [options] [files]

  * Use without argument to run instead of compiling it
  * Use '-c' to compile to native binary.
  * Use '-o' or '-i' to compile to ASM or ELF Objects.
  * Files must be steel source file and end with '.st'

VERSION:
  0.0.1

OPTIONS:

  -O name, --output name  Output program name (add -c) (default: "a.out")
  -c, --compile           Compile and link to native binary
  -C, --clean             Same as -c but clean .o file after link. Disallow for Cache
  -o, --to-obj            Compile to separate Object files (.o). Do not link
  -i, --to-ir             Compile to separate LLVM IR. Do not link (SOON)
  -p, --print             Print instead of write on disk. Do not link. (must use with -o or -i)
  -a, --ast               Print AST
  -q, --quiet             Quiet
  -v, --verbose           Verbose
  -h, --help              Print help
  -V, --version           Print version
```

## Tests

```
cd ./tests
go build
./tests
```