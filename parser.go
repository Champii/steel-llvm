package main

import (
	"fmt"
	"log"
	"strconv"

	"llvm.org/llvm/bindings/go/llvm"
)

type Parser struct {
	builder   llvm.Builder
	module    llvm.Module
	variables map[string]llvm.Value
	steel     *Steel
}

func (p *Parser) Parse(node *Node) []interface{} {
	var res []interface{}

	for _, child := range node.children {
		var toAdd interface{}

		switch child.token {
		case ruleIdentifier:
			toAdd = p.Identifier(child).(string)
		case ruleAssignation:
			toAdd = p.Assignation(child).(llvm.Value)
		case ruleAssignable:
			toAdd = p.Assignable(child)
		case ruleFunctionDeclaration:
			toAdd = p.FunctionDeclaration(child)
		case ruleArgument:
			toAdd = p.Argument(child)
		case ruleArgumentDecl:
			toAdd = p.ArgumentDecl(child)
		case ruleFunctionCall:
			toAdd = p.FunctionCall(child)
		case ruleReturn:
			toAdd = p.Return(child)
		case ruleExternal:
			toAdd = p.External(child)
		case ruleExternalBlock:
			toAdd = p.ExternalBlock(child)
		case ruleExternalDef:
			toAdd = p.ExternalDef(child)
		case ruleExternalArgument:
			toAdd = p.ExternalArgument(child)
		case ruleLiteral:
			toAdd = p.Literal(child)
		case ruleDecimal:
			toAdd = p.Decimal(child)
		case ruleString:
			toAdd = p.String(child)
		case ruleBlock:
			toAdd = p.Block(child)
		case ruleArrayType:
			toAdd = p.ArrayType(child)
		case ruleType:
			toAdd = p.Type(child)
		case ruleVarUse:
			toAdd = p.VarUse(child)
		case ruleOperation:
			toAdd = p.Operation(child)
		case ruleOperand:
			toAdd = p.Operand(child)
		case ruleOperator:
			toAdd = p.Operator(child)
		case ruleArray:
			toAdd = p.Array(child)
		case ruleArrayElem:
			toAdd = p.ArrayElem(child)

		default:
			p.Parse(child)

		}
		res = append(res, toAdd)
	}

	return res
}

func (p *Parser) getVariableValue(name string) llvm.Value {
	t, ok := p.variables[name]
	if !ok {
		log.Fatal("Unknown Var ", name)
	}

	return t
}

func (p *Parser) getType(name string) llvm.Type {
	if name[0] == '[' {
		innerTypeName := name[1 : len(name)-1]
		innerType := p.getType(innerTypeName)

		return llvm.PointerType(innerType, 0)
	} else {
		t, ok := typeAssoc[name]

		if !ok {
			log.Fatal("Parser: Unknown Type ", name)
		}

		return t
	}
}

func (p *Parser) getArgDeclType(node *Node) []llvm.Type {
	var res []llvm.Type

	for _, arg := range node.children {
		// last := getTerminal(arg.children[1])
		// p.variables[last.value] =
		// fmt.Println("Types?", last.value)
		res = append(res, p.getType(arg.children[1].value))
	}

	return res
}

func (p *Parser) declArgs(node *Node, params []llvm.Value) {
	for i, arg := range node.children {
		last := getTerminal(arg.children[0])
		p.variables[last.value] = params[i]
	}
}

func (p *Parser) FunctionDeclaration(node *Node) interface{} {
	funcName := "func"

	if node.parent.parent.token == ruleAssignation {
		funcName = node.parent.parent.children[0].value
	}

	typeElem := getChild(node, ruleType)
	retType := llvm.VoidType()

	argsElem := getChild(node, ruleArguments)

	var argsType []llvm.Type

	if argsElem != nil {
		argsType = p.getArgDeclType(argsElem)
	}

	if typeElem != nil {
		retType = p.getType(typeElem.children[0].value)
	}

	f := llvm.FunctionType(retType, argsType, false)

	fu := llvm.AddFunction(p.module, funcName, f)

	// fmt.Println("lol", fu.Params())
	if argsElem != nil {
		p.declArgs(argsElem, fu.Params())
	}

	p.variables[funcName] = fu

	block := llvm.AddBasicBlock(p.module.NamedFunction(funcName), "entry")

	oldBuilder := p.builder
	p.builder = llvm.NewBuilder()

	p.builder.SetInsertPoint(block, block.FirstInstruction())

	p.Parse(node)

	noRet := getChild(node, ruleNoReturn)
	if noRet != nil {
		p.builder.CreateRetVoid()
	}

	p.builder = oldBuilder

	return fu
}

func (p *Parser) Assignable(node *Node) interface{} {
	return p.Parse(node)[0]
}

func (p *Parser) ArrayType(node *Node) interface{} {
	return p.Parse(node)[0]
}

func (p *Parser) Type(node *Node) interface{} {
	// return p.Parse(node)[0]
	return node.value
}

func (p *Parser) Return(node *Node) interface{} {
	arr := p.Parse(node)

	var res llvm.Value
	if len(arr) > 0 {
		res = p.builder.CreateRet(arr[0].(llvm.Value))
	} else {
		res = p.builder.CreateRetVoid()
	}

	return res
}

func (p *Parser) FunctionCall(node *Node) interface{} {
	_, exists := p.variables[node.children[0].value]

	arr := p.Parse(node)

	if !exists {
		log.Fatal("Unknown function ", node.children[0].value)
	}

	a := p.variables[node.children[0].value]
	args := p.getArgsValue(node)

	// args = arr[1].([]llvm.Value)

	fmt.Println("Args", arr)

	return p.builder.CreateCall(a, args, "")
}

// func (p *Parser) getArgsValue(arr []interface{}) []llvm.Value {
// 	var res []llvm.Value

// 	for _, item := range arr {
// 		res = append(res, item.(llvm.Value))
// 	}

// 	return res
// }

func (p *Parser) getArgsValue(node *Node) []llvm.Value {
	var res []llvm.Value

	for _, arg := range node.children[1:] {
		assign := arg.children[0].children[0]
		if assign.token == ruleVarUse {
			if assign.t[0] != '[' && assign.t != "string" {
				load := p.builder.CreateLoad(p.variables[assign.value], p.variables[assign.value].Name())
				res = append(res, load)
			} else {
				res = append(res, p.variables[assign.value])
			}
		} else {
			val, _ := strconv.Atoi(assign.value)
			load := llvm.ConstInt(llvm.Int32Type(), uint64(val), false)
			res = append(res, load)
		}
	}

	return res
}

func (p *Parser) Argument(node *Node) interface{} {
	return p.Parse(node)[0]
}

func (p *Parser) ArgumentDecl(node *Node) interface{} {
	return p.Parse(node)
}

func (p *Parser) Block(node *Node) interface{} {
	p.Parse(node)

	return ""
}

func (p *Parser) External(node *Node) interface{} {
	return p.Parse(node)[0]
}

func (p *Parser) ExternalBlock(node *Node) interface{} {
	return p.Parse(node)
}

func (p *Parser) ExternalDef(node *Node) interface{} {
	arr := p.Parse(node)

	var types []llvm.Type

	id := node.children[0]
	ret := node.children[len(node.children)-1]

	// for i := 1; i <= len(node.children)-2; i++ {
	// 	types = append(types, p.getType(node.children[i].value))
	// }
	if len(arr) > 2 {
		types = arr[1].([]llvm.Type)
	}

	retType := p.getType(ret.value)

	extFuncType := llvm.FunctionType(retType, types, false)
	fu := llvm.AddFunction(p.module, id.value, extFuncType)

	p.variables[id.value] = fu

	return fu
}

func (p *Parser) ExternalArgument(node *Node) interface{} {
	args := p.Parse(node)

	var res []llvm.Type

	if len(args) > 1 {
		res = append(args[1].([]llvm.Type), p.getType(args[0].(string)))
		copy(res[1:], res[0:])
		res[0] = p.getType(args[0].(string))
	} else {
		res = append(res, p.getType(args[0].(string)))
	}

	return res
}

func (p *Parser) Assignation(node *Node) interface{} {
	arr := p.Parse(node)
	var res llvm.Value

	idChild := getChild(node, ruleIdentifier)
	assChild := getChild(node, ruleAssignable)
	typeChild := getChild(node, ruleType)

	if assChild.children[0].token != ruleFunctionDeclaration {
		id := 2

		if typeChild == nil {
			id = 1
		}

		assValue := arr[id].(llvm.Value)

		if assChild.children[0].t[0] != '[' && assChild.children[0].t != "string" {
			t := p.getType(assChild.t)

			v := p.builder.CreateAlloca(t, idChild.value)

			p.variables[idChild.value] = v

			res = p.builder.CreateStore(assValue, v)
		} else {
			res = assValue
			p.variables[idChild.value] = assValue
		}
	} else {
		res = arr[1].(llvm.Value)
	}

	return res
}

func (p *Parser) Identifier(node *Node) interface{} {
	return node.value
}

func (p *Parser) VarUse(node *Node) interface{} {
	va := p.getVariableValue(node.value)
	// load := p.builder.CreateLoad(va, va.Name())

	return va
}

func (p *Parser) Literal(node *Node) interface{} {
	return p.Parse(node)[0]
}

func (p *Parser) Decimal(node *Node) interface{} {
	val, _ := strconv.Atoi(node.value)
	ret := llvm.ConstInt(llvm.Int32Type(), uint64(val), false)

	return ret
}

func (p *Parser) AddMemcpy() {
	ftMemcpy := llvm.FunctionType(llvm.VoidType(), []llvm.Type{
		llvm.PointerType(llvm.IntType(8), 0),
		llvm.PointerType(llvm.IntType(8), 0),
		llvm.IntType(32),
		llvm.IntType(32),
		llvm.IntType(1),
	}, false)
	memcpy := llvm.AddFunction(p.module, "llvm.memcpy.p0i8.p0i8.i32", ftMemcpy)
	p.variables["memcpy"] = memcpy
}

func (p *Parser) String(node *Node) interface{} {
	realStr := node.value[1 : len(node.value)-1]
	str := p.builder.CreateGlobalStringPtr(realStr, "s")

	node.t = "int8"

	t := p.getType(node.t)

	v := p.builder.CreateArrayAlloca(t, llvm.ConstInt(llvm.Int32Type(), uint64(len(realStr)+1), false), "")

	zero := llvm.ConstInt(llvm.Int32Type(), uint64(0), false)
	ptr := p.builder.CreateGEP(v, []llvm.Value{zero}, "")

	ptr8 := p.builder.CreateBitCast(ptr, llvm.PointerType(llvm.Int8Type(), 0), "")
	gptr8 := p.builder.CreateBitCast(str, llvm.PointerType(llvm.Int8Type(), 0), "")

	p.builder.CreateCall(p.variables["memcpy"], []llvm.Value{
		ptr8, gptr8, llvm.ConstInt(llvm.Int32Type(), uint64(len(realStr)+1), false),
		llvm.ConstInt(llvm.IntType(32), 1, false),
		llvm.ConstInt(llvm.IntType(1), 1, false),
	}, "")
	// zero := llvm.ConstInt(llvm.Int32Type(), uint64(0), false)
	// ptr := p.builder.CreateGEP(str, []llvm.Value{zero}, "")
	// load := p.builder.CreateLoad(str, "")

	return ptr8
}

func (p *Parser) Operation(node *Node) interface{} {
	args := p.Parse(node)

	var op llvm.Value
	switch args[1] {
	case "+":
		op = p.builder.CreateAdd(args[0].(llvm.Value), args[2].(llvm.Value), "")
	case "-":
		op = p.builder.CreateSub(args[0].(llvm.Value), args[2].(llvm.Value), "")
	case "*":
		op = p.builder.CreateMul(args[0].(llvm.Value), args[2].(llvm.Value), "")
	case "/":
		op = p.builder.CreateUDiv(args[0].(llvm.Value), args[2].(llvm.Value), "")
	}

	return op
}

func (p *Parser) Operand(node *Node) interface{} {
	args := p.Parse(node)

	return args[0]
}

func (p *Parser) Operator(node *Node) interface{} {
	// args := p.Parse(node)

	return node.value
}

func (p *Parser) Array(node *Node) interface{} {
	args := p.Parse(node)

	t := p.getType(node.children[0].t)

	var res []llvm.Value

	for _, item := range args[0].([]llvm.Value) {
		res = append(res, item)
	}

	ca := llvm.ConstArray(t, res)
	gptr := llvm.AddGlobal(p.module, llvm.ArrayType(t, len(res)), "")
	gptr.SetInitializer(ca)

	v := p.builder.CreateArrayAlloca(t, llvm.ConstInt(llvm.Int32Type(), uint64(len(res)), false), "")

	zero := llvm.ConstInt(llvm.Int32Type(), uint64(0), false)
	ptr := p.builder.CreateGEP(v, []llvm.Value{zero}, "")

	ptr8 := p.builder.CreateBitCast(ptr, llvm.PointerType(llvm.Int8Type(), 0), "")
	gptr8 := p.builder.CreateBitCast(gptr, llvm.PointerType(llvm.Int8Type(), 0), "")

	p.builder.CreateCall(p.variables["memcpy"], []llvm.Value{
		ptr8, gptr8, llvm.ConstInt(llvm.Int32Type(), uint64(len(res)), false),
		llvm.ConstInt(llvm.IntType(32), 1, false),
		llvm.ConstInt(llvm.IntType(1), 1, false),
	}, "")

	return ptr
}

func (p *Parser) ArrayElem(node *Node) interface{} {
	args := p.Parse(node)

	var res []llvm.Value

	if len(args) > 1 {
		res = append(args[1].([]llvm.Value), args[0].(llvm.Value))
		copy(res[1:], res[0:])
		res[0] = args[0].(llvm.Value)
	} else {
		res = append(res, args[0].(llvm.Value))
	}

	return res
}
