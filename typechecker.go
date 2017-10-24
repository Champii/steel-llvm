package main

import (
	"log"

	"llvm.org/llvm/bindings/go/llvm"
)

type TypeDef struct {
	str string
	t   llvm.Type
}

type FuncDef struct {
	name     string
	argsType []TypeDef
	retType  TypeDef
}

type ClassDef struct {
	name          string
	keys          []string
	innerTypes    []TypeDef
	defaultValues []llvm.Value
	td            TypeDef
}

func (c ClassDef) GetKeyIdx(key string) int {
	for i, k := range c.keys {
		if k == key {
			return i
		}
	}

	return -1
}

// type ClassPropertyDef struct {
// 	key          string
// 	t            TypeDef
// 	defaultValue llvm.Value
// }

type TypeChecker struct {
	functions map[string]FuncDef
	variables map[string]TypeDef
	classes   map[string]ClassDef
}

func (p *TypeChecker) Parse(node *Node) []TypeDef {
	var res []TypeDef

	for _, child := range node.children {
		var toAdd TypeDef
		var arr []TypeDef

		switch child.token {
		case ruleFunctionDeclaration:
			toAdd = p.FunctionDeclaration(child)
		case ruleAssignation:
			toAdd = p.Assignation(child)
		case ruleFunctionCall:
			toAdd = p.FunctionCall(child)
		case ruleExternal:
			toAdd = p.External(child)
		case ruleExternalDef:
			toAdd = p.ExternalDef(child)
		case ruleExternalBlock:
			toAdd = p.ExternalBlock(child)
		case ruleArgumentDecl:
			toAdd = p.ArgumentDecl(child)

		case ruleAssignable:
			toAdd = p.Assignable(child)
		case ruleBlock:
			toAdd = p.Block(child)
		case ruleStatement:
			toAdd = p.Statement(child)
		case ruleExpression:
			toAdd = p.Expression(child)
		case ruleReturn:
			toAdd = p.Return(child)
		case ruleArgument:
			toAdd = p.Argument(child)
		case ruleIdentifier:
			toAdd = p.Identifier(child)
		case ruleType:
			toAdd = p.Type(child)
		case ruleArrayType:
			toAdd = p.ArrayType(child)
		case ruleVarUse:
			toAdd = p.VarUse(child)
		case ruleDecimal:
			toAdd = p.Decimal(child)
		case ruleLiteral:
			toAdd = p.Literal(child)
		case ruleString:
			toAdd = p.String(child)
		case ruleOperation:
			toAdd = p.Operation(child)
		case ruleOperand:
			toAdd = p.Operand(child)
		// case ruleOperator:
		// 	toAdd = p.Operator(child)
		case ruleArray:
			toAdd = p.Array(child)
		case ruleArrayElem:
			toAdd = p.ArrayElem(child)
		case ruleComputedProperty:
			toAdd = p.ComputedProperty(child)
		case ruleBraceComputedProperty:
			toAdd = p.BraceComputedProperty(child)
		case ruleDotComputedProperty:
			toAdd = p.DotComputedProperty(child)
		case ruleComputedPropertyUnderef:
			toAdd = p.ComputedProperty(child)
		case ruleBraceComputedPropertyUnderef:
			toAdd = p.BraceComputedProperty(child)
		case ruleDotComputedPropertyUnderef:
			toAdd = p.DotComputedProperty(child)
		case ruleClass:
			toAdd = p.Class(child)
		case ruleClassEntry:
			toAdd = p.ClassEntry(child)
		case ruleObject:
			toAdd = p.Object(child)
		default:

			switch child.token {
			case ruleExternalArgument:
				arr = p.ExternalArgument(child)
			case ruleArguments:
				arr = p.Arguments(child)
			case ruleClassBlock:
				arr = p.ClassBlock(child)
			case ruleObjectProperty:
				arr = p.ObjectProperty(child)
			case ruleObjectBlock:
				arr = p.ObjectBlock(child)
			case ruleObjectInline:
				arr = p.ObjectInline(child)
			default:
				arr = p.Parse(child)
			}

			if len(arr) > 0 {
				child.t = arr[0]
			}

			for _, item := range arr {
				res = append(res, item)
			}
		}

		if len(arr) == 0 {
			child.t = toAdd
			res = append(res, toAdd)
		}
	}

	return res
}

func (p *TypeChecker) getType(name string) llvm.Type {
	if name[0] == '[' {
		innerTypeName := name[1 : len(name)-1]
		innerType := p.getType(innerTypeName)

		return llvm.PointerType(innerType, 0)
	} else {
		t, ok := typeAssoc[name]

		if !ok {

			t, ok := p.classes[name]
			if !ok {
				log.Panic("TypeChecker: Unknown Type ", name)
			}

			return t.td.t
		}

		return t
	}
}

func createTypeNode(val string) *Node {
	idNode := &Node{
		token: ruleIdentifier,
		value: val,
	}

	node := &Node{
		token: ruleType,
		value: val,
	}

	node.children = append(node.children, idNode)
	idNode.parent = node

	return node
}

func hasKeyType(class ClassDef, key string, t TypeDef) bool {
	for i, innerT := range class.innerTypes {
		if innerT.t == t.t && class.keys[i] == key {
			return true
		}
	}

	return false
}

func (p *TypeChecker) getObjectFromSignature(keys []string, types []TypeDef) *ClassDef {
	for _, class := range p.classes {
		isGood := true

		for i, t := range types {
			if !hasKeyType(class, keys[i], t) {
				isGood = false
				break
			}
		}

		if isGood == true {
			return &class
		}
	}

	return nil
}

func (p *TypeChecker) Assignation(node *Node) TypeDef {
	arr := p.Parse(node)

	idChild := getChild(node, ruleComputedPropertyUnderef)
	assChild := getChild(node, ruleAssignable)
	// typeChild := getChild(node, ruleType)

	if assChild.children[0].token != ruleFunctionDeclaration {
		if len(arr) == 2 {
			typeNode := createTypeNode(node.children[0].value)

			node.children = append(node.children, node.children[len(node.children)-1])
			typeNode.parent = node
			node.children[1] = typeNode
			p.variables[idChild.value] = arr[1]
		} else {
			p.variables[idChild.value] = arr[1]
		}
	}

	if len(arr) == 3 && arr[1] != arr[2] {
		log.Panic("TypeChecker: Invalid type for property "+node.children[0].value+". Expected: ", arr[1].str, ", Found: ", arr[2].str)
	}

	return p.variables[idChild.value]
}

func (p *TypeChecker) External(node *Node) TypeDef {
	p.Parse(node)

	return TypeDef{}
}

func (p *TypeChecker) ExternalBlock(node *Node) TypeDef {
	p.Parse(node)

	return TypeDef{}
}

func (p *TypeChecker) ExternalDef(node *Node) TypeDef {
	arr := p.Parse(node)

	argsType := []TypeDef{}

	if len(arr) > 2 {
		argsType = arr[1 : len(arr)-1]
	}

	retType := arr[len(arr)-1]

	p.functions[node.children[0].value] = FuncDef{
		name:     node.children[0].value,
		argsType: argsType,
		retType:  retType,
	}

	return TypeDef{}
}

func (p *TypeChecker) ExternalArgument(node *Node) []TypeDef {
	args := p.Parse(node)

	var res []TypeDef

	if len(args) > 1 {
		res = append(args, args[0])
		copy(res[1:], res[0:])
		res[0] = args[0]
	} else {
		res = append(res, args[0])
	}

	return res
}

func (p *TypeChecker) FunctionDeclaration(node *Node) TypeDef {
	arr := p.Parse(node)

	args := getChild(node, ruleArguments)
	ret := getChild(node, ruleType)

	var argsType []TypeDef

	if args != nil {
		argsType = arr[:len(args.children[0].children)-1]
	}

	retType := TypeDef{str: "void", t: llvm.VoidType()}
	id := 1

	if node.children[0].token != ruleArguments {
		id = 0
	}

	if ret != nil {
		retType = arr[id]
	} else {
		typeNode := createTypeNode(arr[len(arr)-1].str)

		node.children = append(node.children, &Node{})
		copy(node.children[id+1:], node.children[id:])
		typeNode.parent = node
		node.children[id] = typeNode

		retType = arr[len(arr)-1]
	}

	p.functions[node.parent.parent.children[0].value] = FuncDef{
		name:     node.parent.parent.children[0].value,
		argsType: argsType,
		retType:  retType,
	}

	return retType
}

func (p *TypeChecker) ArgumentDecl(node *Node) TypeDef {
	args := p.Parse(node)
	name := node.children[0].value
	t := node.children[1].children[0].value

	p.variables[name] = TypeDef{str: t, t: p.getType(t)}

	return args[1]
}

func (p *TypeChecker) FunctionCall(node *Node) TypeDef {
	arr := p.Parse(node)

	name := node.children[0].value
	args := arr[1:]

	f, ok := p.functions[node.children[0].value]

	if !ok {
		log.Panic("Unknown function: ", name)
	}

	if len(args) != len(f.argsType) {
		log.Panic("Wrong number of arguments: ", name, " ", len(args), len(f.argsType))
	}

	for i, arg := range f.argsType {
		if arg.str != args[i].str {
			log.Panic("Argument ", i+1, " type mismatch signature for function: ", name, ". Found: ", args[i].str, " , Expected: ", arg.str)
		}
	}

	return f.retType
}

func (p *TypeChecker) Operation(node *Node) TypeDef {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) Operand(node *Node) TypeDef {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) Arguments(node *Node) []TypeDef {
	args := p.Parse(node)

	return args
}

func (p *TypeChecker) Block(node *Node) TypeDef {
	args := p.Parse(node)

	return args[len(args)-1]
}

func (p *TypeChecker) Argument(node *Node) TypeDef {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) Assignable(node *Node) TypeDef {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) Identifier(node *Node) TypeDef {
	return p.variables[node.value]
}

func (p *TypeChecker) VarUse(node *Node) TypeDef {
	id, ok := p.variables[node.value]
	if !ok {
		id, ok := p.functions[node.value]

		if !ok {
			id, ok := p.classes[node.value]

			if !ok {
				log.Panic("Unknown identifier: ", node.value)
			}

			return id.td
		}

		return id.retType
	}

	return id
}

func (p *TypeChecker) ArrayType(node *Node) TypeDef {
	return TypeDef{str: node.value, t: llvm.ArrayType(p.getType(node.value), 0)}
}

func (p *TypeChecker) Type(node *Node) TypeDef {
	args := p.Parse(node)

	if node.children[0].token == ruleIdentifier {
		return TypeDef{str: node.value, t: p.getType(node.value)}
	}

	return args[0]
}

func (p *TypeChecker) Decimal(node *Node) TypeDef {
	return TypeDef{str: "int", t: llvm.Int32Type()}
}

func (p *TypeChecker) Literal(node *Node) TypeDef {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) String(node *Node) TypeDef {
	return TypeDef{str: "[int8]", t: llvm.ArrayType(llvm.Int8Type(), len(node.value))}
}

func (p *TypeChecker) Statement(node *Node) TypeDef {
	args := p.Parse(node)

	if len(args) > 0 {
		return args[0]
	}

	return TypeDef{}
}

func (p *TypeChecker) Expression(node *Node) TypeDef {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) Return(node *Node) TypeDef {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) Array(node *Node) TypeDef {
	args := p.Parse(node)

	td := args[0]

	for _, child := range args {
		if child.t != td.t {
			log.Panic("Arrays must be type consistant")
		}
	}

	return TypeDef{str: "[" + args[0].str + "]", t: llvm.ArrayType(td.t, len(args))}
}

func (p *TypeChecker) ArrayElem(node *Node) TypeDef {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) ComputedProperty(node *Node) TypeDef {
	v := p.variables[node.children[0].value]

	// if !ok {
	// 	log.Panic("TypeChecker: Unknown Type ", node.children[0].value)
	// }

	args := p.Parse(node)

	if len(args) > 1 {
		return args[1]
	}

	return v
}

func (p *TypeChecker) BraceComputedProperty(node *Node) TypeDef {
	args := p.Parse(node)

	v := p.variables[node.parent.children[0].value]

	if len(v.str) > 0 && v.str[0] != '[' {
		log.Panic("Error: ", node.parent.children[0].value, " is not an array")
	}

	return args[0]
}

func (p *TypeChecker) DotComputedProperty(node *Node) TypeDef {
	v := p.variables[node.parent.children[0].value]

	t := p.classes[v.str]

	isIn := false
	for _, k := range t.keys {
		if k == node.children[0].children[0].value {
			isIn = true
		}
	}

	if !isIn {
		log.Panic("TypeChecker: [Class " + t.name + "] has no property " + node.children[0].children[0].value + ".")
	}

	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) Class(node *Node) TypeDef {
	args := p.Parse(node)

	keys := []string{}
	types := []llvm.Type{}

	for i, item := range args[1:] {
		types = append(types, item.t)
		keys = append(keys, node.children[1].children[i].children[0].value)
	}

	p.classes[node.children[0].value] = ClassDef{
		name:       node.children[0].value,
		keys:       keys,
		innerTypes: args[1:],
		td: TypeDef{
			str: node.children[0].value,
			t:   llvm.PointerType(llvm.StructType(types, false), 0),
		},
	}

	return p.classes[node.children[0].value].td
}

func (p *TypeChecker) ClassBlock(node *Node) []TypeDef {
	args := p.Parse(node)

	return args
}

func (p *TypeChecker) ClassEntry(node *Node) TypeDef {
	arr := p.Parse(node)

	idChild := getChild(node, ruleIdentifier)

	if node.children[1].token == ruleAssignable {
		typeNode := createTypeNode(arr[1].str)

		node.children = append(node.children, node.children[len(node.children)-1])
		typeNode.parent = node
		node.children[1] = typeNode
	}

	if len(arr) == 3 && arr[1] != arr[2] {
		log.Panic("TypeChecker: [Class "+node.parent.parent.children[0].value+"] Invalid type for property "+node.children[0].value+". Expected: ", arr[1].str, ", Found: ", arr[2].str)
	}

	p.variables[idChild.value] = arr[1]

	return arr[1]
}

func getObjectKeys(node *Node) []string {
	var res []string

	id := node.children[0].value

	if len(node.children) > 2 {
		ass := getObjectKeys(node.children[2])
		res = append(ass, id)
		copy(res[1:], res[0:])
		res[0] = id
	} else {
		res = append(res, id)
	}

	return res
}

func (p *TypeChecker) Object(node *Node) TypeDef {
	args := p.Parse(node)
	keys := getObjectKeys(node.children[0].children[0])

	class := p.getObjectFromSignature(keys, args)

	if class == nil {
		log.Panic("Cannot guess object type", keys)
	}

	return class.td
}

func (p *TypeChecker) ObjectInline(node *Node) []TypeDef {
	args := p.Parse(node)

	return args
}

func (p *TypeChecker) ObjectBlock(node *Node) []TypeDef {
	args := p.Parse(node)

	return args
}

func (p *TypeChecker) ObjectProperty(node *Node) []TypeDef {
	args := p.Parse(node)

	var res []TypeDef

	if len(args) > 2 {
		res = append(args[2:], args[1])
		copy(res[1:], res[0:])
		res[0] = args[1]
	} else {
		res = append(res, args[1])
	}

	return res
}
