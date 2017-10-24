package main

import (
	"fmt"
	"math"
	"sort"
	"strconv"
)

const endSymbol rune = 1114112

/* The rule types inferred from the grammar are below. */
type pegRule uint8

const (
	ruleUnknown pegRule = iota
	ruleProgram
	ruleStatement
	ruleCom
	ruleExternal
	ruleExternalBlock
	ruleExternalDef
	ruleExternalArgument
	ruleExpression
	ruleReturn
	ruleAssignation
	ruleAssignable
	ruleNew
	ruleComputedProperty
	ruleBraceComputedProperty
	ruleDotComputedProperty
	ruleObject
	ruleObjectInline
	ruleObjectBlock
	ruleObjectProperty
	ruleIf
	ruleElseIf
	ruleElse
	ruleBoolTest
	ruleClass
	ruleClassBlock
	ruleClassEntry
	ruleArray
	ruleArrayElem
	ruleOperation
	ruleOperand
	ruleOperator
	ruleFunctionDeclaration
	ruleArguments
	ruleArgumentDecl
	ruleNoReturn
	ruleBlock
	ruleFunctionCall
	ruleArgument
	ruleIdentifier
	ruleVarUse
	ruleType
	ruleArrayType
	ruleFuncType
	ruleFuncTypeArg
	ruleLiteral
	ruleDecimal
	ruleString
)

var rul3s = [...]string{
	"Unknown",
	"Program",
	"Statement",
	"Com",
	"External",
	"ExternalBlock",
	"ExternalDef",
	"ExternalArgument",
	"Expression",
	"Return",
	"Assignation",
	"Assignable",
	"New",
	"ComputedProperty",
	"BraceComputedProperty",
	"DotComputedProperty",
	"Object",
	"ObjectInline",
	"ObjectBlock",
	"ObjectProperty",
	"If",
	"ElseIf",
	"Else",
	"BoolTest",
	"Class",
	"ClassBlock",
	"ClassEntry",
	"Array",
	"ArrayElem",
	"Operation",
	"Operand",
	"Operator",
	"FunctionDeclaration",
	"Arguments",
	"ArgumentDecl",
	"NoReturn",
	"Block",
	"FunctionCall",
	"Argument",
	"Identifier",
	"VarUse",
	"Type",
	"ArrayType",
	"FuncType",
	"FuncTypeArg",
	"Literal",
	"Decimal",
	"String",
}

type token32 struct {
	pegRule
	begin, end uint32
}

func (t *token32) String() string {
	return fmt.Sprintf("\x1B[34m%v\x1B[m %v %v", rul3s[t.pegRule], t.begin, t.end)
}

type node32 struct {
	token32
	up, next *node32
}

func (node *node32) print(pretty bool, buffer string) {
	var print func(node *node32, depth int)
	print = func(node *node32, depth int) {
		for node != nil {
			for c := 0; c < depth; c++ {
				fmt.Printf(" ")
			}
			rule := rul3s[node.pegRule]
			quote := strconv.Quote(string(([]rune(buffer)[node.begin:node.end])))
			if !pretty {
				fmt.Printf("%v %v\n", rule, quote)
			} else {
				fmt.Printf("\x1B[34m%v\x1B[m %v\n", rule, quote)
			}
			if node.up != nil {
				print(node.up, depth+1)
			}
			node = node.next
		}
	}
	print(node, 0)
}

func (node *node32) Print(buffer string) {
	node.print(false, buffer)
}

func (node *node32) PrettyPrint(buffer string) {
	node.print(true, buffer)
}

type tokens32 struct {
	tree []token32
}

func (t *tokens32) Trim(length uint32) {
	t.tree = t.tree[:length]
}

func (t *tokens32) Print() {
	for _, token := range t.tree {
		fmt.Println(token.String())
	}
}

func (t *tokens32) AST() *node32 {
	type element struct {
		node *node32
		down *element
	}
	tokens := t.Tokens()
	var stack *element
	for _, token := range tokens {
		if token.begin == token.end {
			continue
		}
		node := &node32{token32: token}
		for stack != nil && stack.node.begin >= token.begin && stack.node.end <= token.end {
			stack.node.next = node.up
			node.up = stack.node
			stack = stack.down
		}
		stack = &element{node: node, down: stack}
	}
	if stack != nil {
		return stack.node
	}
	return nil
}

func (t *tokens32) PrintSyntaxTree(buffer string) {
	t.AST().Print(buffer)
}

func (t *tokens32) PrettyPrintSyntaxTree(buffer string) {
	t.AST().PrettyPrint(buffer)
}

func (t *tokens32) Add(rule pegRule, begin, end, index uint32) {
	if tree := t.tree; int(index) >= len(tree) {
		expanded := make([]token32, 2*len(tree))
		copy(expanded, tree)
		t.tree = expanded
	}
	t.tree[index] = token32{
		pegRule: rule,
		begin:   begin,
		end:     end,
	}
}

func (t *tokens32) Tokens() []token32 {
	return t.tree
}

type Steel struct {
	Buffer string
	buffer []rune
	rules  [48]func() bool
	parse  func(rule ...int) error
	reset  func()
	Pretty bool
	tokens32
}

func (p *Steel) Parse(rule ...int) error {
	return p.parse(rule...)
}

func (p *Steel) Reset() {
	p.reset()
}

type textPosition struct {
	line, symbol int
}

type textPositionMap map[int]textPosition

func translatePositions(buffer []rune, positions []int) textPositionMap {
	length, translations, j, line, symbol := len(positions), make(textPositionMap, len(positions)), 0, 1, 0
	sort.Ints(positions)

search:
	for i, c := range buffer {
		if c == '\n' {
			line, symbol = line+1, 0
		} else {
			symbol++
		}
		if i == positions[j] {
			translations[positions[j]] = textPosition{line, symbol}
			for j++; j < length; j++ {
				if i != positions[j] {
					continue search
				}
			}
			break search
		}
	}

	return translations
}

type parseError struct {
	p   *Steel
	max token32
}

func (e *parseError) Error() string {
	tokens, error := []token32{e.max}, "\n"
	positions, p := make([]int, 2*len(tokens)), 0
	for _, token := range tokens {
		positions[p], p = int(token.begin), p+1
		positions[p], p = int(token.end), p+1
	}
	translations := translatePositions(e.p.buffer, positions)
	format := "parse error near %v (line %v symbol %v - line %v symbol %v):\n%v\n"
	if e.p.Pretty {
		format = "parse error near \x1B[34m%v\x1B[m (line %v symbol %v - line %v symbol %v):\n%v\n"
	}
	for _, token := range tokens {
		begin, end := int(token.begin), int(token.end)
		error += fmt.Sprintf(format,
			rul3s[token.pegRule],
			translations[begin].line, translations[begin].symbol,
			translations[end].line, translations[end].symbol,
			strconv.Quote(string(e.p.buffer[begin:end])))
	}

	return error
}

func (p *Steel) PrintSyntaxTree() {
	if p.Pretty {
		p.tokens32.PrettyPrintSyntaxTree(p.Buffer)
	} else {
		p.tokens32.PrintSyntaxTree(p.Buffer)
	}
}

func (p *Steel) Init() {
	var (
		max                  token32
		position, tokenIndex uint32
		buffer               []rune
	)
	p.reset = func() {
		max = token32{}
		position, tokenIndex = 0, 0

		p.buffer = []rune(p.Buffer)
		if len(p.buffer) == 0 || p.buffer[len(p.buffer)-1] != endSymbol {
			p.buffer = append(p.buffer, endSymbol)
		}
		buffer = p.buffer
	}
	p.reset()

	_rules := p.rules
	tree := tokens32{tree: make([]token32, math.MaxInt16)}
	p.parse = func(rule ...int) error {
		r := 1
		if len(rule) > 0 {
			r = rule[0]
		}
		matches := p.rules[r]()
		p.tokens32 = tree
		if matches {
			p.Trim(tokenIndex)
			return nil
		}
		return &parseError{p, max}
	}

	add := func(rule pegRule, begin uint32) {
		tree.Add(rule, begin, position, tokenIndex)
		tokenIndex++
		if begin != position && position > max.end {
			max = token32{rule, begin, position}
		}
	}

	matchDot := func() bool {
		if buffer[position] != endSymbol {
			position++
			return true
		}
		return false
	}

	/*matchChar := func(c byte) bool {
		if buffer[position] == c {
			position++
			return true
		}
		return false
	}*/

	/*matchRange := func(lower byte, upper byte) bool {
		if c := buffer[position]; c >= lower && c <= upper {
			position++
			return true
		}
		return false
	}*/

	_rules = [...]func() bool{
		nil,
		/* 0 Program <- <Statement+> */
		func() bool {
			position0, tokenIndex0 := position, tokenIndex
			{
				position1 := position
				if !_rules[ruleStatement]() {
					goto l0
				}
			l2:
				{
					position3, tokenIndex3 := position, tokenIndex
					if !_rules[ruleStatement]() {
						goto l3
					}
					goto l2
				l3:
					position, tokenIndex = position3, tokenIndex3
				}
				add(ruleProgram, position1)
			}
			return true
		l0:
			position, tokenIndex = position0, tokenIndex0
			return false
		},
		/* 1 Statement <- <((Com / External / Expression)? Com? '\n')> */
		func() bool {
			position4, tokenIndex4 := position, tokenIndex
			{
				position5 := position
				{
					position6, tokenIndex6 := position, tokenIndex
					{
						position8, tokenIndex8 := position, tokenIndex
						if !_rules[ruleCom]() {
							goto l9
						}
						goto l8
					l9:
						position, tokenIndex = position8, tokenIndex8
						if !_rules[ruleExternal]() {
							goto l10
						}
						goto l8
					l10:
						position, tokenIndex = position8, tokenIndex8
						if !_rules[ruleExpression]() {
							goto l6
						}
					}
				l8:
					goto l7
				l6:
					position, tokenIndex = position6, tokenIndex6
				}
			l7:
				{
					position11, tokenIndex11 := position, tokenIndex
					if !_rules[ruleCom]() {
						goto l11
					}
					goto l12
				l11:
					position, tokenIndex = position11, tokenIndex11
				}
			l12:
				if buffer[position] != rune('\n') {
					goto l4
				}
				position++
				add(ruleStatement, position5)
			}
			return true
		l4:
			position, tokenIndex = position4, tokenIndex4
			return false
		},
		/* 2 Com <- <(' '* '#' (!'\n' .)*)> */
		func() bool {
			position13, tokenIndex13 := position, tokenIndex
			{
				position14 := position
			l15:
				{
					position16, tokenIndex16 := position, tokenIndex
					if buffer[position] != rune(' ') {
						goto l16
					}
					position++
					goto l15
				l16:
					position, tokenIndex = position16, tokenIndex16
				}
				if buffer[position] != rune('#') {
					goto l13
				}
				position++
			l17:
				{
					position18, tokenIndex18 := position, tokenIndex
					{
						position19, tokenIndex19 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l19
						}
						position++
						goto l18
					l19:
						position, tokenIndex = position19, tokenIndex19
					}
					if !matchDot() {
						goto l18
					}
					goto l17
				l18:
					position, tokenIndex = position18, tokenIndex18
				}
				add(ruleCom, position14)
			}
			return true
		l13:
			position, tokenIndex = position13, tokenIndex13
			return false
		},
		/* 3 External <- <(('e' / 'E') ('x' / 'X') ('t' / 'T') ('e' / 'E') ('r' / 'R') ('n' / 'N') ('a' / 'A') ('l' / 'L') ExternalBlock)> */
		func() bool {
			position20, tokenIndex20 := position, tokenIndex
			{
				position21 := position
				{
					position22, tokenIndex22 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l23
					}
					position++
					goto l22
				l23:
					position, tokenIndex = position22, tokenIndex22
					if buffer[position] != rune('E') {
						goto l20
					}
					position++
				}
			l22:
				{
					position24, tokenIndex24 := position, tokenIndex
					if buffer[position] != rune('x') {
						goto l25
					}
					position++
					goto l24
				l25:
					position, tokenIndex = position24, tokenIndex24
					if buffer[position] != rune('X') {
						goto l20
					}
					position++
				}
			l24:
				{
					position26, tokenIndex26 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l27
					}
					position++
					goto l26
				l27:
					position, tokenIndex = position26, tokenIndex26
					if buffer[position] != rune('T') {
						goto l20
					}
					position++
				}
			l26:
				{
					position28, tokenIndex28 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l29
					}
					position++
					goto l28
				l29:
					position, tokenIndex = position28, tokenIndex28
					if buffer[position] != rune('E') {
						goto l20
					}
					position++
				}
			l28:
				{
					position30, tokenIndex30 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l31
					}
					position++
					goto l30
				l31:
					position, tokenIndex = position30, tokenIndex30
					if buffer[position] != rune('R') {
						goto l20
					}
					position++
				}
			l30:
				{
					position32, tokenIndex32 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l33
					}
					position++
					goto l32
				l33:
					position, tokenIndex = position32, tokenIndex32
					if buffer[position] != rune('N') {
						goto l20
					}
					position++
				}
			l32:
				{
					position34, tokenIndex34 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l35
					}
					position++
					goto l34
				l35:
					position, tokenIndex = position34, tokenIndex34
					if buffer[position] != rune('A') {
						goto l20
					}
					position++
				}
			l34:
				{
					position36, tokenIndex36 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l37
					}
					position++
					goto l36
				l37:
					position, tokenIndex = position36, tokenIndex36
					if buffer[position] != rune('L') {
						goto l20
					}
					position++
				}
			l36:
				if !_rules[ruleExternalBlock]() {
					goto l20
				}
				add(ruleExternal, position21)
			}
			return true
		l20:
			position, tokenIndex = position20, tokenIndex20
			return false
		},
		/* 4 ExternalBlock <- <('@' '{' '@' '\n' ExternalDef+ ('@' '}' '@'))> */
		func() bool {
			position38, tokenIndex38 := position, tokenIndex
			{
				position39 := position
				if buffer[position] != rune('@') {
					goto l38
				}
				position++
				if buffer[position] != rune('{') {
					goto l38
				}
				position++
				if buffer[position] != rune('@') {
					goto l38
				}
				position++
				if buffer[position] != rune('\n') {
					goto l38
				}
				position++
				if !_rules[ruleExternalDef]() {
					goto l38
				}
			l40:
				{
					position41, tokenIndex41 := position, tokenIndex
					if !_rules[ruleExternalDef]() {
						goto l41
					}
					goto l40
				l41:
					position, tokenIndex = position41, tokenIndex41
				}
				if buffer[position] != rune('@') {
					goto l38
				}
				position++
				if buffer[position] != rune('}') {
					goto l38
				}
				position++
				if buffer[position] != rune('@') {
					goto l38
				}
				position++
				add(ruleExternalBlock, position39)
			}
			return true
		l38:
			position, tokenIndex = position38, tokenIndex38
			return false
		},
		/* 5 ExternalDef <- <(Identifier '(' ExternalArgument? ')' ':' Type '\n')> */
		func() bool {
			position42, tokenIndex42 := position, tokenIndex
			{
				position43 := position
				if !_rules[ruleIdentifier]() {
					goto l42
				}
				if buffer[position] != rune('(') {
					goto l42
				}
				position++
				{
					position44, tokenIndex44 := position, tokenIndex
					if !_rules[ruleExternalArgument]() {
						goto l44
					}
					goto l45
				l44:
					position, tokenIndex = position44, tokenIndex44
				}
			l45:
				if buffer[position] != rune(')') {
					goto l42
				}
				position++
				if buffer[position] != rune(':') {
					goto l42
				}
				position++
				if !_rules[ruleType]() {
					goto l42
				}
				if buffer[position] != rune('\n') {
					goto l42
				}
				position++
				add(ruleExternalDef, position43)
			}
			return true
		l42:
			position, tokenIndex = position42, tokenIndex42
			return false
		},
		/* 6 ExternalArgument <- <(Type (',' ExternalArgument)?)> */
		func() bool {
			position46, tokenIndex46 := position, tokenIndex
			{
				position47 := position
				if !_rules[ruleType]() {
					goto l46
				}
				{
					position48, tokenIndex48 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l48
					}
					position++
					if !_rules[ruleExternalArgument]() {
						goto l48
					}
					goto l49
				l48:
					position, tokenIndex = position48, tokenIndex48
				}
			l49:
				add(ruleExternalArgument, position47)
			}
			return true
		l46:
			position, tokenIndex = position46, tokenIndex46
			return false
		},
		/* 7 Expression <- <(Class / Return / If / Assignation / Assignable)> */
		func() bool {
			position50, tokenIndex50 := position, tokenIndex
			{
				position51 := position
				{
					position52, tokenIndex52 := position, tokenIndex
					if !_rules[ruleClass]() {
						goto l53
					}
					goto l52
				l53:
					position, tokenIndex = position52, tokenIndex52
					if !_rules[ruleReturn]() {
						goto l54
					}
					goto l52
				l54:
					position, tokenIndex = position52, tokenIndex52
					if !_rules[ruleIf]() {
						goto l55
					}
					goto l52
				l55:
					position, tokenIndex = position52, tokenIndex52
					if !_rules[ruleAssignation]() {
						goto l56
					}
					goto l52
				l56:
					position, tokenIndex = position52, tokenIndex52
					if !_rules[ruleAssignable]() {
						goto l50
					}
				}
			l52:
				add(ruleExpression, position51)
			}
			return true
		l50:
			position, tokenIndex = position50, tokenIndex50
			return false
		},
		/* 8 Return <- <(('r' / 'R') ('e' / 'E') ('t' / 'T') ('u' / 'U') ('r' / 'R') ('n' / 'N') Assignable)> */
		func() bool {
			position57, tokenIndex57 := position, tokenIndex
			{
				position58 := position
				{
					position59, tokenIndex59 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l60
					}
					position++
					goto l59
				l60:
					position, tokenIndex = position59, tokenIndex59
					if buffer[position] != rune('R') {
						goto l57
					}
					position++
				}
			l59:
				{
					position61, tokenIndex61 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l62
					}
					position++
					goto l61
				l62:
					position, tokenIndex = position61, tokenIndex61
					if buffer[position] != rune('E') {
						goto l57
					}
					position++
				}
			l61:
				{
					position63, tokenIndex63 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l64
					}
					position++
					goto l63
				l64:
					position, tokenIndex = position63, tokenIndex63
					if buffer[position] != rune('T') {
						goto l57
					}
					position++
				}
			l63:
				{
					position65, tokenIndex65 := position, tokenIndex
					if buffer[position] != rune('u') {
						goto l66
					}
					position++
					goto l65
				l66:
					position, tokenIndex = position65, tokenIndex65
					if buffer[position] != rune('U') {
						goto l57
					}
					position++
				}
			l65:
				{
					position67, tokenIndex67 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l68
					}
					position++
					goto l67
				l68:
					position, tokenIndex = position67, tokenIndex67
					if buffer[position] != rune('R') {
						goto l57
					}
					position++
				}
			l67:
				{
					position69, tokenIndex69 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l70
					}
					position++
					goto l69
				l70:
					position, tokenIndex = position69, tokenIndex69
					if buffer[position] != rune('N') {
						goto l57
					}
					position++
				}
			l69:
				if !_rules[ruleAssignable]() {
					goto l57
				}
				add(ruleReturn, position58)
			}
			return true
		l57:
			position, tokenIndex = position57, tokenIndex57
			return false
		},
		/* 9 Assignation <- <(Identifier (':' Type)? '=' Assignable)> */
		func() bool {
			position71, tokenIndex71 := position, tokenIndex
			{
				position72 := position
				if !_rules[ruleIdentifier]() {
					goto l71
				}
				{
					position73, tokenIndex73 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l73
					}
					position++
					if !_rules[ruleType]() {
						goto l73
					}
					goto l74
				l73:
					position, tokenIndex = position73, tokenIndex73
				}
			l74:
				if buffer[position] != rune('=') {
					goto l71
				}
				position++
				if !_rules[ruleAssignable]() {
					goto l71
				}
				add(ruleAssignation, position72)
			}
			return true
		l71:
			position, tokenIndex = position71, tokenIndex71
			return false
		},
		/* 10 Assignable <- <(New / Operation / Object / FunctionDeclaration / Array / FunctionCall / ComputedProperty / Literal)> */
		func() bool {
			position75, tokenIndex75 := position, tokenIndex
			{
				position76 := position
				{
					position77, tokenIndex77 := position, tokenIndex
					if !_rules[ruleNew]() {
						goto l78
					}
					goto l77
				l78:
					position, tokenIndex = position77, tokenIndex77
					if !_rules[ruleOperation]() {
						goto l79
					}
					goto l77
				l79:
					position, tokenIndex = position77, tokenIndex77
					if !_rules[ruleObject]() {
						goto l80
					}
					goto l77
				l80:
					position, tokenIndex = position77, tokenIndex77
					if !_rules[ruleFunctionDeclaration]() {
						goto l81
					}
					goto l77
				l81:
					position, tokenIndex = position77, tokenIndex77
					if !_rules[ruleArray]() {
						goto l82
					}
					goto l77
				l82:
					position, tokenIndex = position77, tokenIndex77
					if !_rules[ruleFunctionCall]() {
						goto l83
					}
					goto l77
				l83:
					position, tokenIndex = position77, tokenIndex77
					if !_rules[ruleComputedProperty]() {
						goto l84
					}
					goto l77
				l84:
					position, tokenIndex = position77, tokenIndex77
					if !_rules[ruleLiteral]() {
						goto l75
					}
				}
			l77:
				add(ruleAssignable, position76)
			}
			return true
		l75:
			position, tokenIndex = position75, tokenIndex75
			return false
		},
		/* 11 New <- <(('n' / 'N') ('e' / 'E') ('w' / 'W') Identifier '(' Argument* ')')> */
		func() bool {
			position85, tokenIndex85 := position, tokenIndex
			{
				position86 := position
				{
					position87, tokenIndex87 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l88
					}
					position++
					goto l87
				l88:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('N') {
						goto l85
					}
					position++
				}
			l87:
				{
					position89, tokenIndex89 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l90
					}
					position++
					goto l89
				l90:
					position, tokenIndex = position89, tokenIndex89
					if buffer[position] != rune('E') {
						goto l85
					}
					position++
				}
			l89:
				{
					position91, tokenIndex91 := position, tokenIndex
					if buffer[position] != rune('w') {
						goto l92
					}
					position++
					goto l91
				l92:
					position, tokenIndex = position91, tokenIndex91
					if buffer[position] != rune('W') {
						goto l85
					}
					position++
				}
			l91:
				if !_rules[ruleIdentifier]() {
					goto l85
				}
				if buffer[position] != rune('(') {
					goto l85
				}
				position++
			l93:
				{
					position94, tokenIndex94 := position, tokenIndex
					if !_rules[ruleArgument]() {
						goto l94
					}
					goto l93
				l94:
					position, tokenIndex = position94, tokenIndex94
				}
				if buffer[position] != rune(')') {
					goto l85
				}
				position++
				add(ruleNew, position86)
			}
			return true
		l85:
			position, tokenIndex = position85, tokenIndex85
			return false
		},
		/* 12 ComputedProperty <- <(Identifier (BraceComputedProperty / DotComputedProperty)?)> */
		func() bool {
			position95, tokenIndex95 := position, tokenIndex
			{
				position96 := position
				if !_rules[ruleIdentifier]() {
					goto l95
				}
				{
					position97, tokenIndex97 := position, tokenIndex
					{
						position99, tokenIndex99 := position, tokenIndex
						if !_rules[ruleBraceComputedProperty]() {
							goto l100
						}
						goto l99
					l100:
						position, tokenIndex = position99, tokenIndex99
						if !_rules[ruleDotComputedProperty]() {
							goto l97
						}
					}
				l99:
					goto l98
				l97:
					position, tokenIndex = position97, tokenIndex97
				}
			l98:
				add(ruleComputedProperty, position96)
			}
			return true
		l95:
			position, tokenIndex = position95, tokenIndex95
			return false
		},
		/* 13 BraceComputedProperty <- <('[' (Operation / FunctionCall / ComputedProperty / Literal) ']' (BraceComputedProperty / DotComputedProperty)?)> */
		func() bool {
			position101, tokenIndex101 := position, tokenIndex
			{
				position102 := position
				if buffer[position] != rune('[') {
					goto l101
				}
				position++
				{
					position103, tokenIndex103 := position, tokenIndex
					if !_rules[ruleOperation]() {
						goto l104
					}
					goto l103
				l104:
					position, tokenIndex = position103, tokenIndex103
					if !_rules[ruleFunctionCall]() {
						goto l105
					}
					goto l103
				l105:
					position, tokenIndex = position103, tokenIndex103
					if !_rules[ruleComputedProperty]() {
						goto l106
					}
					goto l103
				l106:
					position, tokenIndex = position103, tokenIndex103
					if !_rules[ruleLiteral]() {
						goto l101
					}
				}
			l103:
				if buffer[position] != rune(']') {
					goto l101
				}
				position++
				{
					position107, tokenIndex107 := position, tokenIndex
					{
						position109, tokenIndex109 := position, tokenIndex
						if !_rules[ruleBraceComputedProperty]() {
							goto l110
						}
						goto l109
					l110:
						position, tokenIndex = position109, tokenIndex109
						if !_rules[ruleDotComputedProperty]() {
							goto l107
						}
					}
				l109:
					goto l108
				l107:
					position, tokenIndex = position107, tokenIndex107
				}
			l108:
				add(ruleBraceComputedProperty, position102)
			}
			return true
		l101:
			position, tokenIndex = position101, tokenIndex101
			return false
		},
		/* 14 DotComputedProperty <- <('.' ComputedProperty)> */
		func() bool {
			position111, tokenIndex111 := position, tokenIndex
			{
				position112 := position
				if buffer[position] != rune('.') {
					goto l111
				}
				position++
				if !_rules[ruleComputedProperty]() {
					goto l111
				}
				add(ruleDotComputedProperty, position112)
			}
			return true
		l111:
			position, tokenIndex = position111, tokenIndex111
			return false
		},
		/* 15 Object <- <(ObjectInline / ObjectBlock)> */
		func() bool {
			position113, tokenIndex113 := position, tokenIndex
			{
				position114 := position
				{
					position115, tokenIndex115 := position, tokenIndex
					if !_rules[ruleObjectInline]() {
						goto l116
					}
					goto l115
				l116:
					position, tokenIndex = position115, tokenIndex115
					if !_rules[ruleObjectBlock]() {
						goto l113
					}
				}
			l115:
				add(ruleObject, position114)
			}
			return true
		l113:
			position, tokenIndex = position113, tokenIndex113
			return false
		},
		/* 16 ObjectInline <- <(('{' ObjectProperty? '}') / ObjectProperty)> */
		func() bool {
			position117, tokenIndex117 := position, tokenIndex
			{
				position118 := position
				{
					position119, tokenIndex119 := position, tokenIndex
					if buffer[position] != rune('{') {
						goto l120
					}
					position++
					{
						position121, tokenIndex121 := position, tokenIndex
						if !_rules[ruleObjectProperty]() {
							goto l121
						}
						goto l122
					l121:
						position, tokenIndex = position121, tokenIndex121
					}
				l122:
					if buffer[position] != rune('}') {
						goto l120
					}
					position++
					goto l119
				l120:
					position, tokenIndex = position119, tokenIndex119
					if !_rules[ruleObjectProperty]() {
						goto l117
					}
				}
			l119:
				add(ruleObjectInline, position118)
			}
			return true
		l117:
			position, tokenIndex = position117, tokenIndex117
			return false
		},
		/* 17 ObjectBlock <- <(('@' '{' '@' '\n' ObjectProperty? ('\n' '@' '}' '@')) / ('{' '@' '{' '@' '\n' ObjectProperty? ('\n' '@' '}' '@' '\n' '}')))> */
		func() bool {
			position123, tokenIndex123 := position, tokenIndex
			{
				position124 := position
				{
					position125, tokenIndex125 := position, tokenIndex
					if buffer[position] != rune('@') {
						goto l126
					}
					position++
					if buffer[position] != rune('{') {
						goto l126
					}
					position++
					if buffer[position] != rune('@') {
						goto l126
					}
					position++
					if buffer[position] != rune('\n') {
						goto l126
					}
					position++
					{
						position127, tokenIndex127 := position, tokenIndex
						if !_rules[ruleObjectProperty]() {
							goto l127
						}
						goto l128
					l127:
						position, tokenIndex = position127, tokenIndex127
					}
				l128:
					if buffer[position] != rune('\n') {
						goto l126
					}
					position++
					if buffer[position] != rune('@') {
						goto l126
					}
					position++
					if buffer[position] != rune('}') {
						goto l126
					}
					position++
					if buffer[position] != rune('@') {
						goto l126
					}
					position++
					goto l125
				l126:
					position, tokenIndex = position125, tokenIndex125
					if buffer[position] != rune('{') {
						goto l123
					}
					position++
					if buffer[position] != rune('@') {
						goto l123
					}
					position++
					if buffer[position] != rune('{') {
						goto l123
					}
					position++
					if buffer[position] != rune('@') {
						goto l123
					}
					position++
					if buffer[position] != rune('\n') {
						goto l123
					}
					position++
					{
						position129, tokenIndex129 := position, tokenIndex
						if !_rules[ruleObjectProperty]() {
							goto l129
						}
						goto l130
					l129:
						position, tokenIndex = position129, tokenIndex129
					}
				l130:
					if buffer[position] != rune('\n') {
						goto l123
					}
					position++
					if buffer[position] != rune('@') {
						goto l123
					}
					position++
					if buffer[position] != rune('}') {
						goto l123
					}
					position++
					if buffer[position] != rune('@') {
						goto l123
					}
					position++
					if buffer[position] != rune('\n') {
						goto l123
					}
					position++
					if buffer[position] != rune('}') {
						goto l123
					}
					position++
				}
			l125:
				add(ruleObjectBlock, position124)
			}
			return true
		l123:
			position, tokenIndex = position123, tokenIndex123
			return false
		},
		/* 18 ObjectProperty <- <(Identifier ':' Assignable (','? '\n'? ObjectProperty)?)> */
		func() bool {
			position131, tokenIndex131 := position, tokenIndex
			{
				position132 := position
				if !_rules[ruleIdentifier]() {
					goto l131
				}
				if buffer[position] != rune(':') {
					goto l131
				}
				position++
				if !_rules[ruleAssignable]() {
					goto l131
				}
				{
					position133, tokenIndex133 := position, tokenIndex
					{
						position135, tokenIndex135 := position, tokenIndex
						if buffer[position] != rune(',') {
							goto l135
						}
						position++
						goto l136
					l135:
						position, tokenIndex = position135, tokenIndex135
					}
				l136:
					{
						position137, tokenIndex137 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l137
						}
						position++
						goto l138
					l137:
						position, tokenIndex = position137, tokenIndex137
					}
				l138:
					if !_rules[ruleObjectProperty]() {
						goto l133
					}
					goto l134
				l133:
					position, tokenIndex = position133, tokenIndex133
				}
			l134:
				add(ruleObjectProperty, position132)
			}
			return true
		l131:
			position, tokenIndex = position131, tokenIndex131
			return false
		},
		/* 19 If <- <(('i' / 'I') ('f' / 'F') BoolTest (Block / ('=' '>' Expression)) ElseIf? Else?)> */
		func() bool {
			position139, tokenIndex139 := position, tokenIndex
			{
				position140 := position
				{
					position141, tokenIndex141 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l142
					}
					position++
					goto l141
				l142:
					position, tokenIndex = position141, tokenIndex141
					if buffer[position] != rune('I') {
						goto l139
					}
					position++
				}
			l141:
				{
					position143, tokenIndex143 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l144
					}
					position++
					goto l143
				l144:
					position, tokenIndex = position143, tokenIndex143
					if buffer[position] != rune('F') {
						goto l139
					}
					position++
				}
			l143:
				if !_rules[ruleBoolTest]() {
					goto l139
				}
				{
					position145, tokenIndex145 := position, tokenIndex
					if !_rules[ruleBlock]() {
						goto l146
					}
					goto l145
				l146:
					position, tokenIndex = position145, tokenIndex145
					if buffer[position] != rune('=') {
						goto l139
					}
					position++
					if buffer[position] != rune('>') {
						goto l139
					}
					position++
					if !_rules[ruleExpression]() {
						goto l139
					}
				}
			l145:
				{
					position147, tokenIndex147 := position, tokenIndex
					if !_rules[ruleElseIf]() {
						goto l147
					}
					goto l148
				l147:
					position, tokenIndex = position147, tokenIndex147
				}
			l148:
				{
					position149, tokenIndex149 := position, tokenIndex
					if !_rules[ruleElse]() {
						goto l149
					}
					goto l150
				l149:
					position, tokenIndex = position149, tokenIndex149
				}
			l150:
				add(ruleIf, position140)
			}
			return true
		l139:
			position, tokenIndex = position139, tokenIndex139
			return false
		},
		/* 20 ElseIf <- <(('e' / 'E') ('l' / 'L') ('s' / 'S') ('e' / 'E') ('i' / 'I') ('f' / 'F') BoolTest (Block / ('=' '>' Expression)) ElseIf? Else?)> */
		func() bool {
			position151, tokenIndex151 := position, tokenIndex
			{
				position152 := position
				{
					position153, tokenIndex153 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l154
					}
					position++
					goto l153
				l154:
					position, tokenIndex = position153, tokenIndex153
					if buffer[position] != rune('E') {
						goto l151
					}
					position++
				}
			l153:
				{
					position155, tokenIndex155 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l156
					}
					position++
					goto l155
				l156:
					position, tokenIndex = position155, tokenIndex155
					if buffer[position] != rune('L') {
						goto l151
					}
					position++
				}
			l155:
				{
					position157, tokenIndex157 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l158
					}
					position++
					goto l157
				l158:
					position, tokenIndex = position157, tokenIndex157
					if buffer[position] != rune('S') {
						goto l151
					}
					position++
				}
			l157:
				{
					position159, tokenIndex159 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l160
					}
					position++
					goto l159
				l160:
					position, tokenIndex = position159, tokenIndex159
					if buffer[position] != rune('E') {
						goto l151
					}
					position++
				}
			l159:
				{
					position161, tokenIndex161 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l162
					}
					position++
					goto l161
				l162:
					position, tokenIndex = position161, tokenIndex161
					if buffer[position] != rune('I') {
						goto l151
					}
					position++
				}
			l161:
				{
					position163, tokenIndex163 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l164
					}
					position++
					goto l163
				l164:
					position, tokenIndex = position163, tokenIndex163
					if buffer[position] != rune('F') {
						goto l151
					}
					position++
				}
			l163:
				if !_rules[ruleBoolTest]() {
					goto l151
				}
				{
					position165, tokenIndex165 := position, tokenIndex
					if !_rules[ruleBlock]() {
						goto l166
					}
					goto l165
				l166:
					position, tokenIndex = position165, tokenIndex165
					if buffer[position] != rune('=') {
						goto l151
					}
					position++
					if buffer[position] != rune('>') {
						goto l151
					}
					position++
					if !_rules[ruleExpression]() {
						goto l151
					}
				}
			l165:
				{
					position167, tokenIndex167 := position, tokenIndex
					if !_rules[ruleElseIf]() {
						goto l167
					}
					goto l168
				l167:
					position, tokenIndex = position167, tokenIndex167
				}
			l168:
				{
					position169, tokenIndex169 := position, tokenIndex
					if !_rules[ruleElse]() {
						goto l169
					}
					goto l170
				l169:
					position, tokenIndex = position169, tokenIndex169
				}
			l170:
				add(ruleElseIf, position152)
			}
			return true
		l151:
			position, tokenIndex = position151, tokenIndex151
			return false
		},
		/* 21 Else <- <(('e' / 'E') ('l' / 'L') ('s' / 'S') ('e' / 'E') (Block / ('=' '>' Expression)))> */
		func() bool {
			position171, tokenIndex171 := position, tokenIndex
			{
				position172 := position
				{
					position173, tokenIndex173 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l174
					}
					position++
					goto l173
				l174:
					position, tokenIndex = position173, tokenIndex173
					if buffer[position] != rune('E') {
						goto l171
					}
					position++
				}
			l173:
				{
					position175, tokenIndex175 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l176
					}
					position++
					goto l175
				l176:
					position, tokenIndex = position175, tokenIndex175
					if buffer[position] != rune('L') {
						goto l171
					}
					position++
				}
			l175:
				{
					position177, tokenIndex177 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l178
					}
					position++
					goto l177
				l178:
					position, tokenIndex = position177, tokenIndex177
					if buffer[position] != rune('S') {
						goto l171
					}
					position++
				}
			l177:
				{
					position179, tokenIndex179 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l180
					}
					position++
					goto l179
				l180:
					position, tokenIndex = position179, tokenIndex179
					if buffer[position] != rune('E') {
						goto l171
					}
					position++
				}
			l179:
				{
					position181, tokenIndex181 := position, tokenIndex
					if !_rules[ruleBlock]() {
						goto l182
					}
					goto l181
				l182:
					position, tokenIndex = position181, tokenIndex181
					if buffer[position] != rune('=') {
						goto l171
					}
					position++
					if buffer[position] != rune('>') {
						goto l171
					}
					position++
					if !_rules[ruleExpression]() {
						goto l171
					}
				}
			l181:
				add(ruleElse, position172)
			}
			return true
		l171:
			position, tokenIndex = position171, tokenIndex171
			return false
		},
		/* 22 BoolTest <- <(Assignable '<' (BoolTest / Assignable))> */
		func() bool {
			position183, tokenIndex183 := position, tokenIndex
			{
				position184 := position
				if !_rules[ruleAssignable]() {
					goto l183
				}
				if buffer[position] != rune('<') {
					goto l183
				}
				position++
				{
					position185, tokenIndex185 := position, tokenIndex
					if !_rules[ruleBoolTest]() {
						goto l186
					}
					goto l185
				l186:
					position, tokenIndex = position185, tokenIndex185
					if !_rules[ruleAssignable]() {
						goto l183
					}
				}
			l185:
				add(ruleBoolTest, position184)
			}
			return true
		l183:
			position, tokenIndex = position183, tokenIndex183
			return false
		},
		/* 23 Class <- <(('c' / 'C') ('l' / 'L') ('a' / 'A') ('s' / 'S') ('s' / 'S') Identifier ClassBlock)> */
		func() bool {
			position187, tokenIndex187 := position, tokenIndex
			{
				position188 := position
				{
					position189, tokenIndex189 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l190
					}
					position++
					goto l189
				l190:
					position, tokenIndex = position189, tokenIndex189
					if buffer[position] != rune('C') {
						goto l187
					}
					position++
				}
			l189:
				{
					position191, tokenIndex191 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l192
					}
					position++
					goto l191
				l192:
					position, tokenIndex = position191, tokenIndex191
					if buffer[position] != rune('L') {
						goto l187
					}
					position++
				}
			l191:
				{
					position193, tokenIndex193 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l194
					}
					position++
					goto l193
				l194:
					position, tokenIndex = position193, tokenIndex193
					if buffer[position] != rune('A') {
						goto l187
					}
					position++
				}
			l193:
				{
					position195, tokenIndex195 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l196
					}
					position++
					goto l195
				l196:
					position, tokenIndex = position195, tokenIndex195
					if buffer[position] != rune('S') {
						goto l187
					}
					position++
				}
			l195:
				{
					position197, tokenIndex197 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l198
					}
					position++
					goto l197
				l198:
					position, tokenIndex = position197, tokenIndex197
					if buffer[position] != rune('S') {
						goto l187
					}
					position++
				}
			l197:
				if !_rules[ruleIdentifier]() {
					goto l187
				}
				if !_rules[ruleClassBlock]() {
					goto l187
				}
				add(ruleClass, position188)
			}
			return true
		l187:
			position, tokenIndex = position187, tokenIndex187
			return false
		},
		/* 24 ClassBlock <- <('@' '{' '@' '\n' ClassEntry+ ('@' '}' '@'))> */
		func() bool {
			position199, tokenIndex199 := position, tokenIndex
			{
				position200 := position
				if buffer[position] != rune('@') {
					goto l199
				}
				position++
				if buffer[position] != rune('{') {
					goto l199
				}
				position++
				if buffer[position] != rune('@') {
					goto l199
				}
				position++
				if buffer[position] != rune('\n') {
					goto l199
				}
				position++
				if !_rules[ruleClassEntry]() {
					goto l199
				}
			l201:
				{
					position202, tokenIndex202 := position, tokenIndex
					if !_rules[ruleClassEntry]() {
						goto l202
					}
					goto l201
				l202:
					position, tokenIndex = position202, tokenIndex202
				}
				if buffer[position] != rune('@') {
					goto l199
				}
				position++
				if buffer[position] != rune('}') {
					goto l199
				}
				position++
				if buffer[position] != rune('@') {
					goto l199
				}
				position++
				add(ruleClassBlock, position200)
			}
			return true
		l199:
			position, tokenIndex = position199, tokenIndex199
			return false
		},
		/* 25 ClassEntry <- <(Identifier (':' Type)? ('=' Assignable)? '\n')> */
		func() bool {
			position203, tokenIndex203 := position, tokenIndex
			{
				position204 := position
				if !_rules[ruleIdentifier]() {
					goto l203
				}
				{
					position205, tokenIndex205 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l205
					}
					position++
					if !_rules[ruleType]() {
						goto l205
					}
					goto l206
				l205:
					position, tokenIndex = position205, tokenIndex205
				}
			l206:
				{
					position207, tokenIndex207 := position, tokenIndex
					if buffer[position] != rune('=') {
						goto l207
					}
					position++
					if !_rules[ruleAssignable]() {
						goto l207
					}
					goto l208
				l207:
					position, tokenIndex = position207, tokenIndex207
				}
			l208:
				if buffer[position] != rune('\n') {
					goto l203
				}
				position++
				add(ruleClassEntry, position204)
			}
			return true
		l203:
			position, tokenIndex = position203, tokenIndex203
			return false
		},
		/* 26 Array <- <('[' ArrayElem ']')> */
		func() bool {
			position209, tokenIndex209 := position, tokenIndex
			{
				position210 := position
				if buffer[position] != rune('[') {
					goto l209
				}
				position++
				if !_rules[ruleArrayElem]() {
					goto l209
				}
				if buffer[position] != rune(']') {
					goto l209
				}
				position++
				add(ruleArray, position210)
			}
			return true
		l209:
			position, tokenIndex = position209, tokenIndex209
			return false
		},
		/* 27 ArrayElem <- <(Assignable (',' ArrayElem)?)> */
		func() bool {
			position211, tokenIndex211 := position, tokenIndex
			{
				position212 := position
				if !_rules[ruleAssignable]() {
					goto l211
				}
				{
					position213, tokenIndex213 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l213
					}
					position++
					if !_rules[ruleArrayElem]() {
						goto l213
					}
					goto l214
				l213:
					position, tokenIndex = position213, tokenIndex213
				}
			l214:
				add(ruleArrayElem, position212)
			}
			return true
		l211:
			position, tokenIndex = position211, tokenIndex211
			return false
		},
		/* 28 Operation <- <(Operand Operator (Operation / Operand))> */
		func() bool {
			position215, tokenIndex215 := position, tokenIndex
			{
				position216 := position
				if !_rules[ruleOperand]() {
					goto l215
				}
				if !_rules[ruleOperator]() {
					goto l215
				}
				{
					position217, tokenIndex217 := position, tokenIndex
					if !_rules[ruleOperation]() {
						goto l218
					}
					goto l217
				l218:
					position, tokenIndex = position217, tokenIndex217
					if !_rules[ruleOperand]() {
						goto l215
					}
				}
			l217:
				add(ruleOperation, position216)
			}
			return true
		l215:
			position, tokenIndex = position215, tokenIndex215
			return false
		},
		/* 29 Operand <- <(FunctionCall / ComputedProperty / Decimal)> */
		func() bool {
			position219, tokenIndex219 := position, tokenIndex
			{
				position220 := position
				{
					position221, tokenIndex221 := position, tokenIndex
					if !_rules[ruleFunctionCall]() {
						goto l222
					}
					goto l221
				l222:
					position, tokenIndex = position221, tokenIndex221
					if !_rules[ruleComputedProperty]() {
						goto l223
					}
					goto l221
				l223:
					position, tokenIndex = position221, tokenIndex221
					if !_rules[ruleDecimal]() {
						goto l219
					}
				}
			l221:
				add(ruleOperand, position220)
			}
			return true
		l219:
			position, tokenIndex = position219, tokenIndex219
			return false
		},
		/* 30 Operator <- <('+' / '-' / '/' / '*')> */
		func() bool {
			position224, tokenIndex224 := position, tokenIndex
			{
				position225 := position
				{
					position226, tokenIndex226 := position, tokenIndex
					if buffer[position] != rune('+') {
						goto l227
					}
					position++
					goto l226
				l227:
					position, tokenIndex = position226, tokenIndex226
					if buffer[position] != rune('-') {
						goto l228
					}
					position++
					goto l226
				l228:
					position, tokenIndex = position226, tokenIndex226
					if buffer[position] != rune('/') {
						goto l229
					}
					position++
					goto l226
				l229:
					position, tokenIndex = position226, tokenIndex226
					if buffer[position] != rune('*') {
						goto l224
					}
					position++
				}
			l226:
				add(ruleOperator, position225)
			}
			return true
		l224:
			position, tokenIndex = position224, tokenIndex224
			return false
		},
		/* 31 FunctionDeclaration <- <(Arguments? (':' Type)? NoReturn? ('-' '>') (Block / Expression))> */
		func() bool {
			position230, tokenIndex230 := position, tokenIndex
			{
				position231 := position
				{
					position232, tokenIndex232 := position, tokenIndex
					if !_rules[ruleArguments]() {
						goto l232
					}
					goto l233
				l232:
					position, tokenIndex = position232, tokenIndex232
				}
			l233:
				{
					position234, tokenIndex234 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l234
					}
					position++
					if !_rules[ruleType]() {
						goto l234
					}
					goto l235
				l234:
					position, tokenIndex = position234, tokenIndex234
				}
			l235:
				{
					position236, tokenIndex236 := position, tokenIndex
					if !_rules[ruleNoReturn]() {
						goto l236
					}
					goto l237
				l236:
					position, tokenIndex = position236, tokenIndex236
				}
			l237:
				if buffer[position] != rune('-') {
					goto l230
				}
				position++
				if buffer[position] != rune('>') {
					goto l230
				}
				position++
				{
					position238, tokenIndex238 := position, tokenIndex
					if !_rules[ruleBlock]() {
						goto l239
					}
					goto l238
				l239:
					position, tokenIndex = position238, tokenIndex238
					if !_rules[ruleExpression]() {
						goto l230
					}
				}
			l238:
				add(ruleFunctionDeclaration, position231)
			}
			return true
		l230:
			position, tokenIndex = position230, tokenIndex230
			return false
		},
		/* 32 Arguments <- <('(' ArgumentDecl* ')')> */
		func() bool {
			position240, tokenIndex240 := position, tokenIndex
			{
				position241 := position
				if buffer[position] != rune('(') {
					goto l240
				}
				position++
			l242:
				{
					position243, tokenIndex243 := position, tokenIndex
					if !_rules[ruleArgumentDecl]() {
						goto l243
					}
					goto l242
				l243:
					position, tokenIndex = position243, tokenIndex243
				}
				if buffer[position] != rune(')') {
					goto l240
				}
				position++
				add(ruleArguments, position241)
			}
			return true
		l240:
			position, tokenIndex = position240, tokenIndex240
			return false
		},
		/* 33 ArgumentDecl <- <(Identifier (':' Type)? ','?)> */
		func() bool {
			position244, tokenIndex244 := position, tokenIndex
			{
				position245 := position
				if !_rules[ruleIdentifier]() {
					goto l244
				}
				{
					position246, tokenIndex246 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l246
					}
					position++
					if !_rules[ruleType]() {
						goto l246
					}
					goto l247
				l246:
					position, tokenIndex = position246, tokenIndex246
				}
			l247:
				{
					position248, tokenIndex248 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l248
					}
					position++
					goto l249
				l248:
					position, tokenIndex = position248, tokenIndex248
				}
			l249:
				add(ruleArgumentDecl, position245)
			}
			return true
		l244:
			position, tokenIndex = position244, tokenIndex244
			return false
		},
		/* 34 NoReturn <- <'!'> */
		func() bool {
			position250, tokenIndex250 := position, tokenIndex
			{
				position251 := position
				if buffer[position] != rune('!') {
					goto l250
				}
				position++
				add(ruleNoReturn, position251)
			}
			return true
		l250:
			position, tokenIndex = position250, tokenIndex250
			return false
		},
		/* 35 Block <- <('@' '{' '@' '\n' Statement+ ('@' '}' '@'))> */
		func() bool {
			position252, tokenIndex252 := position, tokenIndex
			{
				position253 := position
				if buffer[position] != rune('@') {
					goto l252
				}
				position++
				if buffer[position] != rune('{') {
					goto l252
				}
				position++
				if buffer[position] != rune('@') {
					goto l252
				}
				position++
				if buffer[position] != rune('\n') {
					goto l252
				}
				position++
				if !_rules[ruleStatement]() {
					goto l252
				}
			l254:
				{
					position255, tokenIndex255 := position, tokenIndex
					if !_rules[ruleStatement]() {
						goto l255
					}
					goto l254
				l255:
					position, tokenIndex = position255, tokenIndex255
				}
				if buffer[position] != rune('@') {
					goto l252
				}
				position++
				if buffer[position] != rune('}') {
					goto l252
				}
				position++
				if buffer[position] != rune('@') {
					goto l252
				}
				position++
				add(ruleBlock, position253)
			}
			return true
		l252:
			position, tokenIndex = position252, tokenIndex252
			return false
		},
		/* 36 FunctionCall <- <(VarUse '(' Argument? ')')> */
		func() bool {
			position256, tokenIndex256 := position, tokenIndex
			{
				position257 := position
				if !_rules[ruleVarUse]() {
					goto l256
				}
				if buffer[position] != rune('(') {
					goto l256
				}
				position++
				{
					position258, tokenIndex258 := position, tokenIndex
					if !_rules[ruleArgument]() {
						goto l258
					}
					goto l259
				l258:
					position, tokenIndex = position258, tokenIndex258
				}
			l259:
				if buffer[position] != rune(')') {
					goto l256
				}
				position++
				add(ruleFunctionCall, position257)
			}
			return true
		l256:
			position, tokenIndex = position256, tokenIndex256
			return false
		},
		/* 37 Argument <- <(Assignable (',' Argument)?)> */
		func() bool {
			position260, tokenIndex260 := position, tokenIndex
			{
				position261 := position
				if !_rules[ruleAssignable]() {
					goto l260
				}
				{
					position262, tokenIndex262 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l262
					}
					position++
					if !_rules[ruleArgument]() {
						goto l262
					}
					goto l263
				l262:
					position, tokenIndex = position262, tokenIndex262
				}
			l263:
				add(ruleArgument, position261)
			}
			return true
		l260:
			position, tokenIndex = position260, tokenIndex260
			return false
		},
		/* 38 Identifier <- <(('_' / [a-z] / [A-Z]) ([a-z] / [A-Z] / [0-9] / '_')*)> */
		func() bool {
			position264, tokenIndex264 := position, tokenIndex
			{
				position265 := position
				{
					position266, tokenIndex266 := position, tokenIndex
					if buffer[position] != rune('_') {
						goto l267
					}
					position++
					goto l266
				l267:
					position, tokenIndex = position266, tokenIndex266
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l268
					}
					position++
					goto l266
				l268:
					position, tokenIndex = position266, tokenIndex266
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l264
					}
					position++
				}
			l266:
			l269:
				{
					position270, tokenIndex270 := position, tokenIndex
					{
						position271, tokenIndex271 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l272
						}
						position++
						goto l271
					l272:
						position, tokenIndex = position271, tokenIndex271
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l273
						}
						position++
						goto l271
					l273:
						position, tokenIndex = position271, tokenIndex271
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l274
						}
						position++
						goto l271
					l274:
						position, tokenIndex = position271, tokenIndex271
						if buffer[position] != rune('_') {
							goto l270
						}
						position++
					}
				l271:
					goto l269
				l270:
					position, tokenIndex = position270, tokenIndex270
				}
				add(ruleIdentifier, position265)
			}
			return true
		l264:
			position, tokenIndex = position264, tokenIndex264
			return false
		},
		/* 39 VarUse <- <(('_' / [a-z] / [A-Z]) ([a-z] / [A-Z] / [0-9] / '_')*)> */
		func() bool {
			position275, tokenIndex275 := position, tokenIndex
			{
				position276 := position
				{
					position277, tokenIndex277 := position, tokenIndex
					if buffer[position] != rune('_') {
						goto l278
					}
					position++
					goto l277
				l278:
					position, tokenIndex = position277, tokenIndex277
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l279
					}
					position++
					goto l277
				l279:
					position, tokenIndex = position277, tokenIndex277
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l275
					}
					position++
				}
			l277:
			l280:
				{
					position281, tokenIndex281 := position, tokenIndex
					{
						position282, tokenIndex282 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l283
						}
						position++
						goto l282
					l283:
						position, tokenIndex = position282, tokenIndex282
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l284
						}
						position++
						goto l282
					l284:
						position, tokenIndex = position282, tokenIndex282
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l285
						}
						position++
						goto l282
					l285:
						position, tokenIndex = position282, tokenIndex282
						if buffer[position] != rune('_') {
							goto l281
						}
						position++
					}
				l282:
					goto l280
				l281:
					position, tokenIndex = position281, tokenIndex281
				}
				add(ruleVarUse, position276)
			}
			return true
		l275:
			position, tokenIndex = position275, tokenIndex275
			return false
		},
		/* 40 Type <- <(FuncType / ArrayType / Identifier)> */
		func() bool {
			position286, tokenIndex286 := position, tokenIndex
			{
				position287 := position
				{
					position288, tokenIndex288 := position, tokenIndex
					if !_rules[ruleFuncType]() {
						goto l289
					}
					goto l288
				l289:
					position, tokenIndex = position288, tokenIndex288
					if !_rules[ruleArrayType]() {
						goto l290
					}
					goto l288
				l290:
					position, tokenIndex = position288, tokenIndex288
					if !_rules[ruleIdentifier]() {
						goto l286
					}
				}
			l288:
				add(ruleType, position287)
			}
			return true
		l286:
			position, tokenIndex = position286, tokenIndex286
			return false
		},
		/* 41 ArrayType <- <('[' Type ']')> */
		func() bool {
			position291, tokenIndex291 := position, tokenIndex
			{
				position292 := position
				if buffer[position] != rune('[') {
					goto l291
				}
				position++
				if !_rules[ruleType]() {
					goto l291
				}
				if buffer[position] != rune(']') {
					goto l291
				}
				position++
				add(ruleArrayType, position292)
			}
			return true
		l291:
			position, tokenIndex = position291, tokenIndex291
			return false
		},
		/* 42 FuncType <- <('(' FuncTypeArg ')' ':' Type)> */
		func() bool {
			position293, tokenIndex293 := position, tokenIndex
			{
				position294 := position
				if buffer[position] != rune('(') {
					goto l293
				}
				position++
				if !_rules[ruleFuncTypeArg]() {
					goto l293
				}
				if buffer[position] != rune(')') {
					goto l293
				}
				position++
				if buffer[position] != rune(':') {
					goto l293
				}
				position++
				if !_rules[ruleType]() {
					goto l293
				}
				add(ruleFuncType, position294)
			}
			return true
		l293:
			position, tokenIndex = position293, tokenIndex293
			return false
		},
		/* 43 FuncTypeArg <- <(Type (',' FuncTypeArg)?)> */
		func() bool {
			position295, tokenIndex295 := position, tokenIndex
			{
				position296 := position
				if !_rules[ruleType]() {
					goto l295
				}
				{
					position297, tokenIndex297 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l297
					}
					position++
					if !_rules[ruleFuncTypeArg]() {
						goto l297
					}
					goto l298
				l297:
					position, tokenIndex = position297, tokenIndex297
				}
			l298:
				add(ruleFuncTypeArg, position296)
			}
			return true
		l295:
			position, tokenIndex = position295, tokenIndex295
			return false
		},
		/* 44 Literal <- <(String / Decimal)> */
		func() bool {
			position299, tokenIndex299 := position, tokenIndex
			{
				position300 := position
				{
					position301, tokenIndex301 := position, tokenIndex
					if !_rules[ruleString]() {
						goto l302
					}
					goto l301
				l302:
					position, tokenIndex = position301, tokenIndex301
					if !_rules[ruleDecimal]() {
						goto l299
					}
				}
			l301:
				add(ruleLiteral, position300)
			}
			return true
		l299:
			position, tokenIndex = position299, tokenIndex299
			return false
		},
		/* 45 Decimal <- <('0' / ([1-9] [0-9]*))> */
		func() bool {
			position303, tokenIndex303 := position, tokenIndex
			{
				position304 := position
				{
					position305, tokenIndex305 := position, tokenIndex
					if buffer[position] != rune('0') {
						goto l306
					}
					position++
					goto l305
				l306:
					position, tokenIndex = position305, tokenIndex305
					if c := buffer[position]; c < rune('1') || c > rune('9') {
						goto l303
					}
					position++
				l307:
					{
						position308, tokenIndex308 := position, tokenIndex
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l308
						}
						position++
						goto l307
					l308:
						position, tokenIndex = position308, tokenIndex308
					}
				}
			l305:
				add(ruleDecimal, position304)
			}
			return true
		l303:
			position, tokenIndex = position303, tokenIndex303
			return false
		},
		/* 46 String <- <('"' (!'"' .)* '"')> */
		func() bool {
			position309, tokenIndex309 := position, tokenIndex
			{
				position310 := position
				if buffer[position] != rune('"') {
					goto l309
				}
				position++
			l311:
				{
					position312, tokenIndex312 := position, tokenIndex
					{
						position313, tokenIndex313 := position, tokenIndex
						if buffer[position] != rune('"') {
							goto l313
						}
						position++
						goto l312
					l313:
						position, tokenIndex = position313, tokenIndex313
					}
					if !matchDot() {
						goto l312
					}
					goto l311
				l312:
					position, tokenIndex = position312, tokenIndex312
				}
				if buffer[position] != rune('"') {
					goto l309
				}
				position++
				add(ruleString, position310)
			}
			return true
		l309:
			position, tokenIndex = position309, tokenIndex309
			return false
		},
	}
	p.rules = _rules
}
