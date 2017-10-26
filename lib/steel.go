package steel

import (
	"log"

	"llvm.org/llvm/bindings/go/llvm"
)

type Steel struct {
	Module llvm.Module
}

type SteelOptions struct {
	ToObj    bool
	ToIr     bool
	ToStdOut bool
	Clean    bool
	PrintAst bool
	Quiet    bool
	Verbose  bool
	Output   string
	Compile  bool
	Run      bool
}

var typeAssoc map[string]llvm.Type

// func (this *Steel) CompileFile(file string, opts CompilerOptions) ([]byte, error) {

func (this *Steel) CompileFile(file string, opts SteelOptions) ([]byte, error) {
	typeAssoc = make(map[string]llvm.Type)

	typeAssoc["void"] = llvm.VoidType()
	typeAssoc["int"] = llvm.Int32Type()
	typeAssoc["int32"] = llvm.Int32Type()
	typeAssoc["int16"] = llvm.Int16Type()
	typeAssoc["int8"] = llvm.Int8Type()
	typeAssoc["int1"] = llvm.Int1Type()
	// typeAssoc["string"] = llvm.Int8Type()
	typeAssoc["string"] = llvm.PointerType(llvm.Int8Type(), 0)

	preproc := Preproc(file)

	parsed := &SteelParser{Buffer: preproc}

	newAst := getAst(parsed)

	if opts.PrintAst {
		parsed.PrintSyntaxTree()
	}

	functionsType := make(map[string]FuncDef)
	variablesType := make(map[string]TypeDef)
	classesType := make(map[string]ClassDef)

	preparser := Preparser{}

	preparser.Parse(newAst)

	typeChecker := TypeChecker{
		functions: functionsType,
		variables: variablesType,
		classes:   classesType,
	}

	typeChecker.Parse(newAst)

	parser := Parser{
		builder:       llvm.NewBuilder(),
		module:        this.Module,
		steel:         parsed,
		variables:     make(map[string]llvm.Value),
		functionsType: functionsType,
		variablesType: variablesType,
		classesType:   classesType,
	}

	parser.AddMemcpy()
	parser.Parse(newAst)

	if opts.ToIr {
		parser.module.Dump()
	}

	if err := llvm.VerifyModule(parser.module, llvm.ReturnStatusAction); err != nil {
		return []byte{}, err
	}

	llvm.InitializeAllTargetInfos()
	llvm.InitializeAllTargets()
	llvm.InitializeAllTargetMCs()
	llvm.InitializeAllAsmParsers()
	llvm.InitializeAllAsmPrinters()

	passManager := llvm.NewFunctionPassManagerForModule(parser.module)
	passManager.InitializeFunc()
	passManager.AddConstantMergePass()
	passManager.AddDeadArgEliminationPass()
	passManager.AddDeadStoreEliminationPass()
	passManager.AddInstructionCombiningPass()
	passManager.AddMemorySanitizerPass()
	passManager.AddReassociatePass()

	passManager.FinalizeFunc()
	passManager.AddVerifierPass()
	triple := llvm.DefaultTargetTriple()

	target, err := llvm.GetTargetFromTriple(triple)

	if err != nil {
		return []byte{}, err
	}

	targetMachine := target.CreateTargetMachine(triple, "generic", "", llvm.CodeGenLevelNone, llvm.RelocDefault, llvm.CodeModelDefault)

	targetMachine.AddAnalysisPasses(passManager)

	mb, err := targetMachine.EmitToMemoryBuffer(parser.module, llvm.ObjectFile)

	if err != nil {
		return []byte{}, err
	}

	return mb.Bytes(), nil
}

func (this *Steel) CompileFiles(files []string, opts SteelOptions) ([][]byte, error) {
	var res [][]byte

	for _, file := range files {
		obj, err := this.CompileFile(file, opts)

		if err != nil {
			return [][]byte{}, err
		}

		res = append(res, obj)
	}

	return res, nil
}

func (this *Steel) Run() int {
	engine, err := llvm.NewExecutionEngine(this.Module)
	if err != nil {
		log.Panic(err)
	}
	// engine.
	res := engine.RunFunction(this.Module.NamedFunction("main"), []llvm.GenericValue{})

	return int(res.Int(false))
}
