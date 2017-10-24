package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
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

func getAst(steel *Steel) *Node {
	steel.Init()
	steel.Pretty = true

	if err := steel.Parse(); err != nil {
		log.Panic(err)
	}

	return getNewAst(steel)
}

func parse(steel *Steel, newAst *Node) Parser {
	steel.PrintSyntaxTree()

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
		module:        llvm.NewModule("steel"),
		steel:         steel,
		variables:     make(map[string]llvm.Value),
		functionsType: functionsType,
		variablesType: variablesType,
		classesType:   classesType,
	}

	passManager := llvm.NewFunctionPassManagerForModule(parser.module)
	passManager.InitializeFunc()
	passManager.AddConstantMergePass()
	passManager.AddDeadArgEliminationPass()
	passManager.AddDeadStoreEliminationPass()
	passManager.AddInstructionCombiningPass()
	passManager.AddMemorySanitizerPass()
	passManager.AddReassociatePass()

	parser.AddMemcpy()
	parser.Parse(newAst)

	passManager.FinalizeFunc()
	passManager.AddVerifierPass()

	parser.module.Dump()

	return parser
}

func writeToDisk(parser Parser) {
	if ok := llvm.VerifyModule(parser.module, llvm.ReturnStatusAction); ok != nil {
		fmt.Println(ok.Error())
	} else {
		fmt.Println("Ok")
	}

	destFile, err := os.Create(os.Args[1] + ".bc")

	if err != nil {
		log.Panic(err)
	}

	llvm.WriteBitcodeToFile(parser.module, destFile)
	// build(steel)
}

func test() {
	builder := llvm.NewBuilder()
	module := llvm.NewModule("steel")

	f := llvm.FunctionType(llvm.VoidType(), []llvm.Type{}, false)

	fu := llvm.AddFunction(module, "main", f)

	block := llvm.AddBasicBlock(fu, "entry")

	builder.SetInsertPoint(block, block.FirstInstruction())

	funcPtrType := llvm.PointerType(f, 0)

	sType := llvm.StructType([]llvm.Type{llvm.Int32Type(), funcPtrType}, false)

	sPtr := builder.CreateAlloca(sType, "")
	// test := builder.CreateAlloca(llvm.Int32Type(), "")

	zero := llvm.ConstInt(llvm.Int32Type(), uint64(0), false)
	ptr := builder.CreateStructGEP(sPtr, 0, "")

	builder.CreateStore(zero, ptr)

	ptr2 := builder.CreateStructGEP(sPtr, 1, "")

	builder.CreateStore(fu, ptr2)

	lol := builder.CreateLoad(ptr2, "")

	builder.CreateCall(lol, []llvm.Value{}, "")

	// gptr := llvm.AddGlobal(module, llvm.ArrayType(llvm.Int32Type(), 3), "")

	// ca := llvm.ConstArray(llvm.Int32Type(), []llvm.Value{
	// 	llvm.ConstInt(llvm.Int32Type(), 1, false),
	// 	llvm.ConstInt(llvm.Int32Type(), 2, false),
	// 	llvm.ConstInt(llvm.Int32Type(), 3, false),
	// })

	// gptr.SetInitializer(ca)

	// arrAlloc := builder.CreateArrayAlloca(llvm.Int32Type(), llvm.ConstInt(llvm.Int32Type(), 3, false), "")
	// zero := llvm.ConstInt(llvm.Int32Type(), uint64(0), false)
	// ptr := builder.CreateGEP(arrAlloc, []llvm.Value{zero}, "")

	// ftMemcpy := llvm.FunctionType(llvm.VoidType(), []llvm.Type{
	// 	llvm.PointerType(llvm.Int8Type(), 0),
	// 	llvm.PointerType(llvm.Int8Type(), 0),
	// 	llvm.Int32Type(),
	// 	llvm.Int32Type(),
	// 	llvm.Int1Type(),
	// }, false)

	// memcpy := llvm.AddFunction(module, "llvm.memcpy.p0i8.p0i8.i32", ftMemcpy)

	// ptr8 := builder.CreateBitCast(ptr, llvm.PointerType(llvm.Int8Type(), 0), "")
	// gptr8 := builder.CreateBitCast(gptr, llvm.PointerType(llvm.Int8Type(), 0), "")

	// builder.CreateCall(memcpy, []llvm.Value{
	// 	ptr8, gptr8, llvm.ConstInt(llvm.Int32Type(), uint64(3*4), false),
	// 	llvm.ConstInt(llvm.Int32Type(), 1, false),
	// 	llvm.ConstInt(llvm.Int1Type(), 1, false),
	// }, "")

	builder.CreateRetVoid()

	module.Dump()
	if ok := llvm.VerifyModule(module, llvm.ReturnStatusAction); ok != nil {
		fmt.Println(ok.Error())
	} else {
		fmt.Println("Ok")
	}

	destFile, err := os.Create(os.Args[1] + ".bc")

	if err != nil {
		log.Panic(err)
	}
	llvm.WriteBitcodeToFile(module, destFile)
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

	preproc := Preproc(file)

	fmt.Println(preproc)

	steel := &Steel{Buffer: preproc}

	newAst := getAst(steel)

	parser := parse(steel, newAst)

	writeToDisk(parser)

}

func parseNode(steel *Steel, node *node32) *Node {
	newNode := new(Node)

	newNode.token = node.pegRule
	newNode.value = string(([]rune(steel.buffer)[node.begin:node.end]))

	return newNode
}

func recurParse(line int, steel *Steel, parent *Node, node *node32) {
	for node != nil {
		newNode := parseNode(steel, node)
		newNode.line = line
		parent.AddChildren(newNode)
		if node.up != nil {
			recurParse(strings.Count(steel.Buffer[:node.end], "\n")+1, steel, newNode, node.up)
		}
		node = node.next
	}
}

func getNewAst(steel *Steel) *Node {
	oldRoot := steel.AST()
	newRoot := new(Node)
	line := 1

	recurParse(line, steel, newRoot, oldRoot)

	// fmt.Println(newRoot.children[0].token)
	return newRoot
}

func getChild(node *Node, t pegRule) *Node {
	for _, child := range node.children {
		if child.token == t {
			return child
		}
	}

	return nil
}

func getTerminal(node *Node) *Node {
	if len(node.children) == 0 {
		return node
	}

	return getTerminal(node.children[0])
}

type INode interface {
	New(string, string)
	AddChildren(node *Node)
}

type Node struct {
	INode
	token    pegRule
	value    string
	parent   *Node
	children []*Node
	t        TypeDef
	line     int
}

func (n *Node) AddChildren(node *Node) {
	node.parent = n
	n.children = append(n.children, node)
}
