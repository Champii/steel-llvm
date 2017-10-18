package main

import (
	"fmt"
	"log"
)

type TypeChecker struct {
	functions map[string]FuncDef
	variables map[string]string
}

type FuncDef struct {
	name     string
	argsType []string
	retType  string
}

func (p *TypeChecker) Parse(node *Node) []interface{} {
	var res []interface{}

	for _, child := range node.children {
		var toAdd interface{}

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
		case ruleExternalArgument:
			toAdd = p.ExternalArgument(child)
		case ruleExternalBlock:
			toAdd = p.ExternalBlock(child)
		case ruleArgumentDecl:
			toAdd = p.ArgumentDecl(child)
		case ruleArguments:
			toAdd = p.Arguments(child)
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
		case ruleOperator:
			toAdd = p.Operator(child)
		case ruleArray:
			toAdd = p.Array(child)
		case ruleArrayElem:
			toAdd = p.ArrayElem(child)
		default:
			arr := p.Parse(child)

			if len(arr) > 1 {
				toAdd = arr[0]
			}
		}

		switch toAdd.(type) {
		case string:
			child.t = toAdd.(string)
		}
		// if toAdd != nil {
		// }

		res = append(res, toAdd)
	}

	return res
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

func (p *TypeChecker) Assignation(node *Node) interface{} {
	arr := p.Parse(node)

	idChild := getChild(node, ruleIdentifier)
	assChild := getChild(node, ruleAssignable)
	typeChild := getChild(node, ruleType)

	if assChild.children[0].token != ruleFunctionDeclaration {
		if len(arr) == 2 {
			typeNode := createTypeNode(arr[1].(string))

			node.children = append(node.children, node.children[len(node.children)-1])
			typeNode.parent = node
			node.children[1] = typeNode
			p.variables[idChild.value] = typeNode.value
		} else {
			p.variables[idChild.value] = typeChild.children[0].value
		}
	}

	return nil
}

func (p *TypeChecker) External(node *Node) interface{} {
	p.Parse(node)
	return nil
}

func (p *TypeChecker) ExternalBlock(node *Node) interface{} {
	p.Parse(node)
	return nil
}

func (p *TypeChecker) ExternalDef(node *Node) interface{} {
	// arr := p.Parse(node)

	var types []string

	for i := 1; i <= len(node.children)-2; i++ {
		types = append(types, node.children[i].value)
	}

	retType := node.children[len(node.children)-1].value

	p.functions[node.children[0].value] = FuncDef{
		name:     node.children[0].value,
		argsType: types,
		retType:  retType,
	}

	return nil
}

func (p *TypeChecker) ExternalArgument(node *Node) interface{} {
	return p.Parse(node)[0]
}

func (p *TypeChecker) FunctionDeclaration(node *Node) interface{} {
	arr := p.Parse(node)

	args := getChild(node, ruleArguments)
	ret := getChild(node, ruleType)

	var argsType []string

	if args != nil {
		// argsTypeI := arr[0].([]interface{})

		for _, item := range args.children {
			argsType = append(argsType, item.children[1].children[0].value)
		}

	}

	retType := "void"

	if ret != nil {
		retType = ret.children[0].value
	} else {
		typeNode := createTypeNode(arr[len(arr)-1].(string))

		id := 1

		if node.children[0].token != ruleArgument {
			id = 0
		}

		node.children = append(node.children, &Node{})
		copy(node.children[id+1:], node.children[id:])
		typeNode.parent = node
		node.children[id] = typeNode

		retType = arr[len(arr)-1].(string)
	}

	p.functions[node.parent.parent.children[0].value] = FuncDef{
		name:     node.parent.parent.children[0].value,
		argsType: argsType,
		retType:  retType,
	}

	return retType
}

func (p *TypeChecker) FunctionCall(node *Node) interface{} {
	arr := p.Parse(node)

	name := node.children[0].value
	args := arr[1:]

	f, ok := p.functions[node.children[0].value]

	if !ok {
		log.Fatal("Unknown function: ", name)
	}

	if len(args) != len(f.argsType) {
		log.Fatal("Wrong number of arguments: ", name, " ", len(args), len(f.argsType))
	}

	for i, arg := range f.argsType {
		if arg != args[i] {
			log.Fatal("Argument ", i+1, " type mismatch signature for function: ", name, ". Found: ", args[i], " , Expected: ", arg)
		}
	}

	return f.retType
}

func (p *TypeChecker) ArgumentDecl(node *Node) interface{} {
	args := p.Parse(node)
	name := node.children[0].value
	t := node.children[1].children[0].value
	fmt.Println("Type", args[1].(string))

	p.variables[name] = t

	return args[1].(string)
}

func (p *TypeChecker) Operation(node *Node) interface{} {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) Operand(node *Node) interface{} {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) Operator(node *Node) interface{} {
	// args := p.Parse(node)

	return node.value
}

func (p *TypeChecker) Arguments(node *Node) interface{} {
	args := p.Parse(node)

	return args
}

func (p *TypeChecker) Block(node *Node) interface{} {
	args := p.Parse(node)

	return args[len(args)-1]
}

func (p *TypeChecker) Argument(node *Node) interface{} {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) Assignable(node *Node) interface{} {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) Identifier(node *Node) interface{} {
	return p.variables[node.value]
}

func (p *TypeChecker) VarUse(node *Node) interface{} {
	id, ok := p.variables[node.value]
	if !ok {
		id, ok := p.functions[node.value]

		if !ok {
			log.Fatal("Unknown identifier: ", node.value)
		}

		return id.retType
	}

	return id
}

func (p *TypeChecker) ArrayType(node *Node) interface{} {
	// args := p.Parse(node)

	return node.value
}

func (p *TypeChecker) Type(node *Node) interface{} {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) Decimal(node *Node) interface{} {
	return "int"
}

func (p *TypeChecker) Literal(node *Node) interface{} {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) String(node *Node) interface{} {
	return "string"
}

func (p *TypeChecker) Statement(node *Node) interface{} {
	args := p.Parse(node)
	res := ""

	if len(args) > 0 && args[0] != nil {
		res = args[0].(string)
	}

	return res
}

func (p *TypeChecker) Expression(node *Node) interface{} {
	args := p.Parse(node)

	return args[0]
}
func (p *TypeChecker) Return(node *Node) interface{} {
	args := p.Parse(node)

	return args[0]
}

func (p *TypeChecker) Array(node *Node) interface{} {
	args := p.Parse(node)

	t := args[0]

	for _, child := range args {
		if child != t {
			log.Fatal("Arrays must be type consistant")
		}
	}

	return "[" + args[0].(string) + "]"
}

func (p *TypeChecker) ArrayElem(node *Node) interface{} {
	args := p.Parse(node)

	return args[0]
}
