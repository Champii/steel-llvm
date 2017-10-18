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
	rules  [31]func() bool
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
		/* 1 Statement <- <((Com / External / Expression)? '\n')> */
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
		/* 2 Com <- <('#' (!'\n' .)*)> */
		func() bool {
			position11, tokenIndex11 := position, tokenIndex
			{
				position12 := position
				if buffer[position] != rune('#') {
					goto l11
				}
				position++
			l13:
				{
					position14, tokenIndex14 := position, tokenIndex
					{
						position15, tokenIndex15 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l15
						}
						position++
						goto l14
					l15:
						position, tokenIndex = position15, tokenIndex15
					}
					if !matchDot() {
						goto l14
					}
					goto l13
				l14:
					position, tokenIndex = position14, tokenIndex14
				}
				add(ruleCom, position12)
			}
			return true
		l11:
			position, tokenIndex = position11, tokenIndex11
			return false
		},
		/* 3 External <- <(('e' / 'E') ('x' / 'X') ('t' / 'T') ('e' / 'E') ('r' / 'R') ('n' / 'N') ('a' / 'A') ('l' / 'L') ExternalBlock)> */
		func() bool {
			position16, tokenIndex16 := position, tokenIndex
			{
				position17 := position
				{
					position18, tokenIndex18 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l19
					}
					position++
					goto l18
				l19:
					position, tokenIndex = position18, tokenIndex18
					if buffer[position] != rune('E') {
						goto l16
					}
					position++
				}
			l18:
				{
					position20, tokenIndex20 := position, tokenIndex
					if buffer[position] != rune('x') {
						goto l21
					}
					position++
					goto l20
				l21:
					position, tokenIndex = position20, tokenIndex20
					if buffer[position] != rune('X') {
						goto l16
					}
					position++
				}
			l20:
				{
					position22, tokenIndex22 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l23
					}
					position++
					goto l22
				l23:
					position, tokenIndex = position22, tokenIndex22
					if buffer[position] != rune('T') {
						goto l16
					}
					position++
				}
			l22:
				{
					position24, tokenIndex24 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l25
					}
					position++
					goto l24
				l25:
					position, tokenIndex = position24, tokenIndex24
					if buffer[position] != rune('E') {
						goto l16
					}
					position++
				}
			l24:
				{
					position26, tokenIndex26 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l27
					}
					position++
					goto l26
				l27:
					position, tokenIndex = position26, tokenIndex26
					if buffer[position] != rune('R') {
						goto l16
					}
					position++
				}
			l26:
				{
					position28, tokenIndex28 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l29
					}
					position++
					goto l28
				l29:
					position, tokenIndex = position28, tokenIndex28
					if buffer[position] != rune('N') {
						goto l16
					}
					position++
				}
			l28:
				{
					position30, tokenIndex30 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l31
					}
					position++
					goto l30
				l31:
					position, tokenIndex = position30, tokenIndex30
					if buffer[position] != rune('A') {
						goto l16
					}
					position++
				}
			l30:
				{
					position32, tokenIndex32 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l33
					}
					position++
					goto l32
				l33:
					position, tokenIndex = position32, tokenIndex32
					if buffer[position] != rune('L') {
						goto l16
					}
					position++
				}
			l32:
				if !_rules[ruleExternalBlock]() {
					goto l16
				}
				add(ruleExternal, position17)
			}
			return true
		l16:
			position, tokenIndex = position16, tokenIndex16
			return false
		},
		/* 4 ExternalBlock <- <('@' '{' '@' '\n' ExternalDef+ ('@' '}' '@'))> */
		func() bool {
			position34, tokenIndex34 := position, tokenIndex
			{
				position35 := position
				if buffer[position] != rune('@') {
					goto l34
				}
				position++
				if buffer[position] != rune('{') {
					goto l34
				}
				position++
				if buffer[position] != rune('@') {
					goto l34
				}
				position++
				if buffer[position] != rune('\n') {
					goto l34
				}
				position++
				if !_rules[ruleExternalDef]() {
					goto l34
				}
			l36:
				{
					position37, tokenIndex37 := position, tokenIndex
					if !_rules[ruleExternalDef]() {
						goto l37
					}
					goto l36
				l37:
					position, tokenIndex = position37, tokenIndex37
				}
				if buffer[position] != rune('@') {
					goto l34
				}
				position++
				if buffer[position] != rune('}') {
					goto l34
				}
				position++
				if buffer[position] != rune('@') {
					goto l34
				}
				position++
				add(ruleExternalBlock, position35)
			}
			return true
		l34:
			position, tokenIndex = position34, tokenIndex34
			return false
		},
		/* 5 ExternalDef <- <(Identifier '(' ExternalArgument? ')' ':' Type '\n')> */
		func() bool {
			position38, tokenIndex38 := position, tokenIndex
			{
				position39 := position
				if !_rules[ruleIdentifier]() {
					goto l38
				}
				if buffer[position] != rune('(') {
					goto l38
				}
				position++
				{
					position40, tokenIndex40 := position, tokenIndex
					if !_rules[ruleExternalArgument]() {
						goto l40
					}
					goto l41
				l40:
					position, tokenIndex = position40, tokenIndex40
				}
			l41:
				if buffer[position] != rune(')') {
					goto l38
				}
				position++
				if buffer[position] != rune(':') {
					goto l38
				}
				position++
				if !_rules[ruleType]() {
					goto l38
				}
				if buffer[position] != rune('\n') {
					goto l38
				}
				position++
				add(ruleExternalDef, position39)
			}
			return true
		l38:
			position, tokenIndex = position38, tokenIndex38
			return false
		},
		/* 6 ExternalArgument <- <(Type (',' ExternalArgument)?)> */
		func() bool {
			position42, tokenIndex42 := position, tokenIndex
			{
				position43 := position
				if !_rules[ruleType]() {
					goto l42
				}
				{
					position44, tokenIndex44 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l44
					}
					position++
					if !_rules[ruleExternalArgument]() {
						goto l44
					}
					goto l45
				l44:
					position, tokenIndex = position44, tokenIndex44
				}
			l45:
				add(ruleExternalArgument, position43)
			}
			return true
		l42:
			position, tokenIndex = position42, tokenIndex42
			return false
		},
		/* 7 Expression <- <(Return / Assignation / Assignable)> */
		func() bool {
			position46, tokenIndex46 := position, tokenIndex
			{
				position47 := position
				{
					position48, tokenIndex48 := position, tokenIndex
					if !_rules[ruleReturn]() {
						goto l49
					}
					goto l48
				l49:
					position, tokenIndex = position48, tokenIndex48
					if !_rules[ruleAssignation]() {
						goto l50
					}
					goto l48
				l50:
					position, tokenIndex = position48, tokenIndex48
					if !_rules[ruleAssignable]() {
						goto l46
					}
				}
			l48:
				add(ruleExpression, position47)
			}
			return true
		l46:
			position, tokenIndex = position46, tokenIndex46
			return false
		},
		/* 8 Return <- <(('r' / 'R') ('e' / 'E') ('t' / 'T') ('u' / 'U') ('r' / 'R') ('n' / 'N') Assignable)> */
		func() bool {
			position51, tokenIndex51 := position, tokenIndex
			{
				position52 := position
				{
					position53, tokenIndex53 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l54
					}
					position++
					goto l53
				l54:
					position, tokenIndex = position53, tokenIndex53
					if buffer[position] != rune('R') {
						goto l51
					}
					position++
				}
			l53:
				{
					position55, tokenIndex55 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l56
					}
					position++
					goto l55
				l56:
					position, tokenIndex = position55, tokenIndex55
					if buffer[position] != rune('E') {
						goto l51
					}
					position++
				}
			l55:
				{
					position57, tokenIndex57 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l58
					}
					position++
					goto l57
				l58:
					position, tokenIndex = position57, tokenIndex57
					if buffer[position] != rune('T') {
						goto l51
					}
					position++
				}
			l57:
				{
					position59, tokenIndex59 := position, tokenIndex
					if buffer[position] != rune('u') {
						goto l60
					}
					position++
					goto l59
				l60:
					position, tokenIndex = position59, tokenIndex59
					if buffer[position] != rune('U') {
						goto l51
					}
					position++
				}
			l59:
				{
					position61, tokenIndex61 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l62
					}
					position++
					goto l61
				l62:
					position, tokenIndex = position61, tokenIndex61
					if buffer[position] != rune('R') {
						goto l51
					}
					position++
				}
			l61:
				{
					position63, tokenIndex63 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l64
					}
					position++
					goto l63
				l64:
					position, tokenIndex = position63, tokenIndex63
					if buffer[position] != rune('N') {
						goto l51
					}
					position++
				}
			l63:
				if !_rules[ruleAssignable]() {
					goto l51
				}
				add(ruleReturn, position52)
			}
			return true
		l51:
			position, tokenIndex = position51, tokenIndex51
			return false
		},
		/* 9 Assignation <- <(Identifier (':' Type)? '=' Assignable)> */
		func() bool {
			position65, tokenIndex65 := position, tokenIndex
			{
				position66 := position
				if !_rules[ruleIdentifier]() {
					goto l65
				}
				{
					position67, tokenIndex67 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l67
					}
					position++
					if !_rules[ruleType]() {
						goto l67
					}
					goto l68
				l67:
					position, tokenIndex = position67, tokenIndex67
				}
			l68:
				if buffer[position] != rune('=') {
					goto l65
				}
				position++
				if !_rules[ruleAssignable]() {
					goto l65
				}
				add(ruleAssignation, position66)
			}
			return true
		l65:
			position, tokenIndex = position65, tokenIndex65
			return false
		},
		/* 10 Assignable <- <(Operation / FunctionDeclaration / Array / FunctionCall / VarUse / Literal)> */
		func() bool {
			position69, tokenIndex69 := position, tokenIndex
			{
				position70 := position
				{
					position71, tokenIndex71 := position, tokenIndex
					if !_rules[ruleOperation]() {
						goto l72
					}
					goto l71
				l72:
					position, tokenIndex = position71, tokenIndex71
					if !_rules[ruleFunctionDeclaration]() {
						goto l73
					}
					goto l71
				l73:
					position, tokenIndex = position71, tokenIndex71
					if !_rules[ruleArray]() {
						goto l74
					}
					goto l71
				l74:
					position, tokenIndex = position71, tokenIndex71
					if !_rules[ruleFunctionCall]() {
						goto l75
					}
					goto l71
				l75:
					position, tokenIndex = position71, tokenIndex71
					if !_rules[ruleVarUse]() {
						goto l76
					}
					goto l71
				l76:
					position, tokenIndex = position71, tokenIndex71
					if !_rules[ruleLiteral]() {
						goto l69
					}
				}
			l71:
				add(ruleAssignable, position70)
			}
			return true
		l69:
			position, tokenIndex = position69, tokenIndex69
			return false
		},
		/* 11 Array <- <('[' ArrayElem ']')> */
		func() bool {
			position77, tokenIndex77 := position, tokenIndex
			{
				position78 := position
				if buffer[position] != rune('[') {
					goto l77
				}
				position++
				if !_rules[ruleArrayElem]() {
					goto l77
				}
				if buffer[position] != rune(']') {
					goto l77
				}
				position++
				add(ruleArray, position78)
			}
			return true
		l77:
			position, tokenIndex = position77, tokenIndex77
			return false
		},
		/* 12 ArrayElem <- <(Assignable (',' ArrayElem)?)> */
		func() bool {
			position79, tokenIndex79 := position, tokenIndex
			{
				position80 := position
				if !_rules[ruleAssignable]() {
					goto l79
				}
				{
					position81, tokenIndex81 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l81
					}
					position++
					if !_rules[ruleArrayElem]() {
						goto l81
					}
					goto l82
				l81:
					position, tokenIndex = position81, tokenIndex81
				}
			l82:
				add(ruleArrayElem, position80)
			}
			return true
		l79:
			position, tokenIndex = position79, tokenIndex79
			return false
		},
		/* 13 Operation <- <(Operand Operator (Operation / Operand))> */
		func() bool {
			position83, tokenIndex83 := position, tokenIndex
			{
				position84 := position
				if !_rules[ruleOperand]() {
					goto l83
				}
				if !_rules[ruleOperator]() {
					goto l83
				}
				{
					position85, tokenIndex85 := position, tokenIndex
					if !_rules[ruleOperation]() {
						goto l86
					}
					goto l85
				l86:
					position, tokenIndex = position85, tokenIndex85
					if !_rules[ruleOperand]() {
						goto l83
					}
				}
			l85:
				add(ruleOperation, position84)
			}
			return true
		l83:
			position, tokenIndex = position83, tokenIndex83
			return false
		},
		/* 14 Operand <- <(FunctionCall / VarUse / Decimal)> */
		func() bool {
			position87, tokenIndex87 := position, tokenIndex
			{
				position88 := position
				{
					position89, tokenIndex89 := position, tokenIndex
					if !_rules[ruleFunctionCall]() {
						goto l90
					}
					goto l89
				l90:
					position, tokenIndex = position89, tokenIndex89
					if !_rules[ruleVarUse]() {
						goto l91
					}
					goto l89
				l91:
					position, tokenIndex = position89, tokenIndex89
					if !_rules[ruleDecimal]() {
						goto l87
					}
				}
			l89:
				add(ruleOperand, position88)
			}
			return true
		l87:
			position, tokenIndex = position87, tokenIndex87
			return false
		},
		/* 15 Operator <- <('+' / '-' / '/' / '*')> */
		func() bool {
			position92, tokenIndex92 := position, tokenIndex
			{
				position93 := position
				{
					position94, tokenIndex94 := position, tokenIndex
					if buffer[position] != rune('+') {
						goto l95
					}
					position++
					goto l94
				l95:
					position, tokenIndex = position94, tokenIndex94
					if buffer[position] != rune('-') {
						goto l96
					}
					position++
					goto l94
				l96:
					position, tokenIndex = position94, tokenIndex94
					if buffer[position] != rune('/') {
						goto l97
					}
					position++
					goto l94
				l97:
					position, tokenIndex = position94, tokenIndex94
					if buffer[position] != rune('*') {
						goto l92
					}
					position++
				}
			l94:
				add(ruleOperator, position93)
			}
			return true
		l92:
			position, tokenIndex = position92, tokenIndex92
			return false
		},
		/* 16 FunctionDeclaration <- <(Arguments? (':' Type)? NoReturn? ('-' '>') (Expression / Block))> */
		func() bool {
			position98, tokenIndex98 := position, tokenIndex
			{
				position99 := position
				{
					position100, tokenIndex100 := position, tokenIndex
					if !_rules[ruleArguments]() {
						goto l100
					}
					goto l101
				l100:
					position, tokenIndex = position100, tokenIndex100
				}
			l101:
				{
					position102, tokenIndex102 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l102
					}
					position++
					if !_rules[ruleType]() {
						goto l102
					}
					goto l103
				l102:
					position, tokenIndex = position102, tokenIndex102
				}
			l103:
				{
					position104, tokenIndex104 := position, tokenIndex
					if !_rules[ruleNoReturn]() {
						goto l104
					}
					goto l105
				l104:
					position, tokenIndex = position104, tokenIndex104
				}
			l105:
				if buffer[position] != rune('-') {
					goto l98
				}
				position++
				if buffer[position] != rune('>') {
					goto l98
				}
				position++
				{
					position106, tokenIndex106 := position, tokenIndex
					if !_rules[ruleExpression]() {
						goto l107
					}
					goto l106
				l107:
					position, tokenIndex = position106, tokenIndex106
					if !_rules[ruleBlock]() {
						goto l98
					}
				}
			l106:
				add(ruleFunctionDeclaration, position99)
			}
			return true
		l98:
			position, tokenIndex = position98, tokenIndex98
			return false
		},
		/* 17 Arguments <- <('(' ArgumentDecl* ')')> */
		func() bool {
			position108, tokenIndex108 := position, tokenIndex
			{
				position109 := position
				if buffer[position] != rune('(') {
					goto l108
				}
				position++
			l110:
				{
					position111, tokenIndex111 := position, tokenIndex
					if !_rules[ruleArgumentDecl]() {
						goto l111
					}
					goto l110
				l111:
					position, tokenIndex = position111, tokenIndex111
				}
				if buffer[position] != rune(')') {
					goto l108
				}
				position++
				add(ruleArguments, position109)
			}
			return true
		l108:
			position, tokenIndex = position108, tokenIndex108
			return false
		},
		/* 18 ArgumentDecl <- <(Identifier (':' Type)? ','?)> */
		func() bool {
			position112, tokenIndex112 := position, tokenIndex
			{
				position113 := position
				if !_rules[ruleIdentifier]() {
					goto l112
				}
				{
					position114, tokenIndex114 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l114
					}
					position++
					if !_rules[ruleType]() {
						goto l114
					}
					goto l115
				l114:
					position, tokenIndex = position114, tokenIndex114
				}
			l115:
				{
					position116, tokenIndex116 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l116
					}
					position++
					goto l117
				l116:
					position, tokenIndex = position116, tokenIndex116
				}
			l117:
				add(ruleArgumentDecl, position113)
			}
			return true
		l112:
			position, tokenIndex = position112, tokenIndex112
			return false
		},
		/* 19 NoReturn <- <'!'> */
		func() bool {
			position118, tokenIndex118 := position, tokenIndex
			{
				position119 := position
				if buffer[position] != rune('!') {
					goto l118
				}
				position++
				add(ruleNoReturn, position119)
			}
			return true
		l118:
			position, tokenIndex = position118, tokenIndex118
			return false
		},
		/* 20 Block <- <('@' '{' '@' '\n' Statement+ ('@' '}' '@'))> */
		func() bool {
			position120, tokenIndex120 := position, tokenIndex
			{
				position121 := position
				if buffer[position] != rune('@') {
					goto l120
				}
				position++
				if buffer[position] != rune('{') {
					goto l120
				}
				position++
				if buffer[position] != rune('@') {
					goto l120
				}
				position++
				if buffer[position] != rune('\n') {
					goto l120
				}
				position++
				if !_rules[ruleStatement]() {
					goto l120
				}
			l122:
				{
					position123, tokenIndex123 := position, tokenIndex
					if !_rules[ruleStatement]() {
						goto l123
					}
					goto l122
				l123:
					position, tokenIndex = position123, tokenIndex123
				}
				if buffer[position] != rune('@') {
					goto l120
				}
				position++
				if buffer[position] != rune('}') {
					goto l120
				}
				position++
				if buffer[position] != rune('@') {
					goto l120
				}
				position++
				add(ruleBlock, position121)
			}
			return true
		l120:
			position, tokenIndex = position120, tokenIndex120
			return false
		},
		/* 21 FunctionCall <- <(VarUse '(' Argument* ')')> */
		func() bool {
			position124, tokenIndex124 := position, tokenIndex
			{
				position125 := position
				if !_rules[ruleVarUse]() {
					goto l124
				}
				if buffer[position] != rune('(') {
					goto l124
				}
				position++
			l126:
				{
					position127, tokenIndex127 := position, tokenIndex
					if !_rules[ruleArgument]() {
						goto l127
					}
					goto l126
				l127:
					position, tokenIndex = position127, tokenIndex127
				}
				if buffer[position] != rune(')') {
					goto l124
				}
				position++
				add(ruleFunctionCall, position125)
			}
			return true
		l124:
			position, tokenIndex = position124, tokenIndex124
			return false
		},
		/* 22 Argument <- <(Assignable ','?)> */
		func() bool {
			position128, tokenIndex128 := position, tokenIndex
			{
				position129 := position
				if !_rules[ruleAssignable]() {
					goto l128
				}
				{
					position130, tokenIndex130 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l130
					}
					position++
					goto l131
				l130:
					position, tokenIndex = position130, tokenIndex130
				}
			l131:
				add(ruleArgument, position129)
			}
			return true
		l128:
			position, tokenIndex = position128, tokenIndex128
			return false
		},
		/* 23 Identifier <- <(('_' / [a-z] / [A-Z]) ([a-z] / [A-Z] / [0-9] / '_')*)> */
		func() bool {
			position132, tokenIndex132 := position, tokenIndex
			{
				position133 := position
				{
					position134, tokenIndex134 := position, tokenIndex
					if buffer[position] != rune('_') {
						goto l135
					}
					position++
					goto l134
				l135:
					position, tokenIndex = position134, tokenIndex134
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l136
					}
					position++
					goto l134
				l136:
					position, tokenIndex = position134, tokenIndex134
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l132
					}
					position++
				}
			l134:
			l137:
				{
					position138, tokenIndex138 := position, tokenIndex
					{
						position139, tokenIndex139 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l140
						}
						position++
						goto l139
					l140:
						position, tokenIndex = position139, tokenIndex139
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l141
						}
						position++
						goto l139
					l141:
						position, tokenIndex = position139, tokenIndex139
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l142
						}
						position++
						goto l139
					l142:
						position, tokenIndex = position139, tokenIndex139
						if buffer[position] != rune('_') {
							goto l138
						}
						position++
					}
				l139:
					goto l137
				l138:
					position, tokenIndex = position138, tokenIndex138
				}
				add(ruleIdentifier, position133)
			}
			return true
		l132:
			position, tokenIndex = position132, tokenIndex132
			return false
		},
		/* 24 VarUse <- <(('_' / [a-z] / [A-Z]) ([a-z] / [A-Z] / [0-9] / '_')*)> */
		func() bool {
			position143, tokenIndex143 := position, tokenIndex
			{
				position144 := position
				{
					position145, tokenIndex145 := position, tokenIndex
					if buffer[position] != rune('_') {
						goto l146
					}
					position++
					goto l145
				l146:
					position, tokenIndex = position145, tokenIndex145
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l147
					}
					position++
					goto l145
				l147:
					position, tokenIndex = position145, tokenIndex145
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l143
					}
					position++
				}
			l145:
			l148:
				{
					position149, tokenIndex149 := position, tokenIndex
					{
						position150, tokenIndex150 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l151
						}
						position++
						goto l150
					l151:
						position, tokenIndex = position150, tokenIndex150
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l152
						}
						position++
						goto l150
					l152:
						position, tokenIndex = position150, tokenIndex150
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l153
						}
						position++
						goto l150
					l153:
						position, tokenIndex = position150, tokenIndex150
						if buffer[position] != rune('_') {
							goto l149
						}
						position++
					}
				l150:
					goto l148
				l149:
					position, tokenIndex = position149, tokenIndex149
				}
				add(ruleVarUse, position144)
			}
			return true
		l143:
			position, tokenIndex = position143, tokenIndex143
			return false
		},
		/* 25 Type <- <(ArrayType / Identifier)> */
		func() bool {
			position154, tokenIndex154 := position, tokenIndex
			{
				position155 := position
				{
					position156, tokenIndex156 := position, tokenIndex
					if !_rules[ruleArrayType]() {
						goto l157
					}
					goto l156
				l157:
					position, tokenIndex = position156, tokenIndex156
					if !_rules[ruleIdentifier]() {
						goto l154
					}
				}
			l156:
				add(ruleType, position155)
			}
			return true
		l154:
			position, tokenIndex = position154, tokenIndex154
			return false
		},
		/* 26 ArrayType <- <('[' Type ']')> */
		func() bool {
			position158, tokenIndex158 := position, tokenIndex
			{
				position159 := position
				if buffer[position] != rune('[') {
					goto l158
				}
				position++
				if !_rules[ruleType]() {
					goto l158
				}
				if buffer[position] != rune(']') {
					goto l158
				}
				position++
				add(ruleArrayType, position159)
			}
			return true
		l158:
			position, tokenIndex = position158, tokenIndex158
			return false
		},
		/* 27 Literal <- <(String / Decimal)> */
		func() bool {
			position160, tokenIndex160 := position, tokenIndex
			{
				position161 := position
				{
					position162, tokenIndex162 := position, tokenIndex
					if !_rules[ruleString]() {
						goto l163
					}
					goto l162
				l163:
					position, tokenIndex = position162, tokenIndex162
					if !_rules[ruleDecimal]() {
						goto l160
					}
				}
			l162:
				add(ruleLiteral, position161)
			}
			return true
		l160:
			position, tokenIndex = position160, tokenIndex160
			return false
		},
		/* 28 Decimal <- <('0' / ([1-9] [0-9]*))> */
		func() bool {
			position164, tokenIndex164 := position, tokenIndex
			{
				position165 := position
				{
					position166, tokenIndex166 := position, tokenIndex
					if buffer[position] != rune('0') {
						goto l167
					}
					position++
					goto l166
				l167:
					position, tokenIndex = position166, tokenIndex166
					if c := buffer[position]; c < rune('1') || c > rune('9') {
						goto l164
					}
					position++
				l168:
					{
						position169, tokenIndex169 := position, tokenIndex
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l169
						}
						position++
						goto l168
					l169:
						position, tokenIndex = position169, tokenIndex169
					}
				}
			l166:
				add(ruleDecimal, position165)
			}
			return true
		l164:
			position, tokenIndex = position164, tokenIndex164
			return false
		},
		/* 29 String <- <('"' (!'"' .)* '"')> */
		func() bool {
			position170, tokenIndex170 := position, tokenIndex
			{
				position171 := position
				if buffer[position] != rune('"') {
					goto l170
				}
				position++
			l172:
				{
					position173, tokenIndex173 := position, tokenIndex
					{
						position174, tokenIndex174 := position, tokenIndex
						if buffer[position] != rune('"') {
							goto l174
						}
						position++
						goto l173
					l174:
						position, tokenIndex = position174, tokenIndex174
					}
					if !matchDot() {
						goto l173
					}
					goto l172
				l173:
					position, tokenIndex = position173, tokenIndex173
				}
				if buffer[position] != rune('"') {
					goto l170
				}
				position++
				add(ruleString, position171)
			}
			return true
		l170:
			position, tokenIndex = position170, tokenIndex170
			return false
		},
	}
	p.rules = _rules
}
