package steel

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
	ruleComputedPropertyUnderef
	ruleBraceComputedPropertyUnderef
	ruleDotComputedPropertyUnderef
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
	"ComputedPropertyUnderef",
	"BraceComputedPropertyUnderef",
	"DotComputedPropertyUnderef",
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

type SteelParser struct {
	Buffer string
	buffer []rune
	rules  [51]func() bool
	parse  func(rule ...int) error
	reset  func()
	Pretty bool
	tokens32
}

func (p *SteelParser) Parse(rule ...int) error {
	return p.parse(rule...)
}

func (p *SteelParser) Reset() {
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
	p   *SteelParser
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

func (p *SteelParser) PrintSyntaxTree() {
	if p.Pretty {
		p.tokens32.PrettyPrintSyntaxTree(p.Buffer)
	} else {
		p.tokens32.PrintSyntaxTree(p.Buffer)
	}
}

func (p *SteelParser) Init() {
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
		/* 9 Assignation <- <(ComputedPropertyUnderef (':' Type)? '=' Assignable)> */
		func() bool {
			position71, tokenIndex71 := position, tokenIndex
			{
				position72 := position
				if !_rules[ruleComputedPropertyUnderef]() {
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
		/* 15 ComputedPropertyUnderef <- <(Identifier (BraceComputedPropertyUnderef / DotComputedPropertyUnderef)?)> */
		func() bool {
			position113, tokenIndex113 := position, tokenIndex
			{
				position114 := position
				if !_rules[ruleIdentifier]() {
					goto l113
				}
				{
					position115, tokenIndex115 := position, tokenIndex
					{
						position117, tokenIndex117 := position, tokenIndex
						if !_rules[ruleBraceComputedPropertyUnderef]() {
							goto l118
						}
						goto l117
					l118:
						position, tokenIndex = position117, tokenIndex117
						if !_rules[ruleDotComputedPropertyUnderef]() {
							goto l115
						}
					}
				l117:
					goto l116
				l115:
					position, tokenIndex = position115, tokenIndex115
				}
			l116:
				add(ruleComputedPropertyUnderef, position114)
			}
			return true
		l113:
			position, tokenIndex = position113, tokenIndex113
			return false
		},
		/* 16 BraceComputedPropertyUnderef <- <('[' (Operation / FunctionCall / ComputedPropertyUnderef / Literal) ']' (BraceComputedPropertyUnderef / DotComputedPropertyUnderef)?)> */
		func() bool {
			position119, tokenIndex119 := position, tokenIndex
			{
				position120 := position
				if buffer[position] != rune('[') {
					goto l119
				}
				position++
				{
					position121, tokenIndex121 := position, tokenIndex
					if !_rules[ruleOperation]() {
						goto l122
					}
					goto l121
				l122:
					position, tokenIndex = position121, tokenIndex121
					if !_rules[ruleFunctionCall]() {
						goto l123
					}
					goto l121
				l123:
					position, tokenIndex = position121, tokenIndex121
					if !_rules[ruleComputedPropertyUnderef]() {
						goto l124
					}
					goto l121
				l124:
					position, tokenIndex = position121, tokenIndex121
					if !_rules[ruleLiteral]() {
						goto l119
					}
				}
			l121:
				if buffer[position] != rune(']') {
					goto l119
				}
				position++
				{
					position125, tokenIndex125 := position, tokenIndex
					{
						position127, tokenIndex127 := position, tokenIndex
						if !_rules[ruleBraceComputedPropertyUnderef]() {
							goto l128
						}
						goto l127
					l128:
						position, tokenIndex = position127, tokenIndex127
						if !_rules[ruleDotComputedPropertyUnderef]() {
							goto l125
						}
					}
				l127:
					goto l126
				l125:
					position, tokenIndex = position125, tokenIndex125
				}
			l126:
				add(ruleBraceComputedPropertyUnderef, position120)
			}
			return true
		l119:
			position, tokenIndex = position119, tokenIndex119
			return false
		},
		/* 17 DotComputedPropertyUnderef <- <('.' ComputedPropertyUnderef)> */
		func() bool {
			position129, tokenIndex129 := position, tokenIndex
			{
				position130 := position
				if buffer[position] != rune('.') {
					goto l129
				}
				position++
				if !_rules[ruleComputedPropertyUnderef]() {
					goto l129
				}
				add(ruleDotComputedPropertyUnderef, position130)
			}
			return true
		l129:
			position, tokenIndex = position129, tokenIndex129
			return false
		},
		/* 18 Object <- <(ObjectInline / ObjectBlock)> */
		func() bool {
			position131, tokenIndex131 := position, tokenIndex
			{
				position132 := position
				{
					position133, tokenIndex133 := position, tokenIndex
					if !_rules[ruleObjectInline]() {
						goto l134
					}
					goto l133
				l134:
					position, tokenIndex = position133, tokenIndex133
					if !_rules[ruleObjectBlock]() {
						goto l131
					}
				}
			l133:
				add(ruleObject, position132)
			}
			return true
		l131:
			position, tokenIndex = position131, tokenIndex131
			return false
		},
		/* 19 ObjectInline <- <(('{' ObjectProperty? '}') / ObjectProperty)> */
		func() bool {
			position135, tokenIndex135 := position, tokenIndex
			{
				position136 := position
				{
					position137, tokenIndex137 := position, tokenIndex
					if buffer[position] != rune('{') {
						goto l138
					}
					position++
					{
						position139, tokenIndex139 := position, tokenIndex
						if !_rules[ruleObjectProperty]() {
							goto l139
						}
						goto l140
					l139:
						position, tokenIndex = position139, tokenIndex139
					}
				l140:
					if buffer[position] != rune('}') {
						goto l138
					}
					position++
					goto l137
				l138:
					position, tokenIndex = position137, tokenIndex137
					if !_rules[ruleObjectProperty]() {
						goto l135
					}
				}
			l137:
				add(ruleObjectInline, position136)
			}
			return true
		l135:
			position, tokenIndex = position135, tokenIndex135
			return false
		},
		/* 20 ObjectBlock <- <(('@' '{' '@' '\n' ObjectProperty? ('\n' '@' '}' '@')) / ('{' '@' '{' '@' '\n' ObjectProperty? ('\n' '@' '}' '@' '\n' '}')))> */
		func() bool {
			position141, tokenIndex141 := position, tokenIndex
			{
				position142 := position
				{
					position143, tokenIndex143 := position, tokenIndex
					if buffer[position] != rune('@') {
						goto l144
					}
					position++
					if buffer[position] != rune('{') {
						goto l144
					}
					position++
					if buffer[position] != rune('@') {
						goto l144
					}
					position++
					if buffer[position] != rune('\n') {
						goto l144
					}
					position++
					{
						position145, tokenIndex145 := position, tokenIndex
						if !_rules[ruleObjectProperty]() {
							goto l145
						}
						goto l146
					l145:
						position, tokenIndex = position145, tokenIndex145
					}
				l146:
					if buffer[position] != rune('\n') {
						goto l144
					}
					position++
					if buffer[position] != rune('@') {
						goto l144
					}
					position++
					if buffer[position] != rune('}') {
						goto l144
					}
					position++
					if buffer[position] != rune('@') {
						goto l144
					}
					position++
					goto l143
				l144:
					position, tokenIndex = position143, tokenIndex143
					if buffer[position] != rune('{') {
						goto l141
					}
					position++
					if buffer[position] != rune('@') {
						goto l141
					}
					position++
					if buffer[position] != rune('{') {
						goto l141
					}
					position++
					if buffer[position] != rune('@') {
						goto l141
					}
					position++
					if buffer[position] != rune('\n') {
						goto l141
					}
					position++
					{
						position147, tokenIndex147 := position, tokenIndex
						if !_rules[ruleObjectProperty]() {
							goto l147
						}
						goto l148
					l147:
						position, tokenIndex = position147, tokenIndex147
					}
				l148:
					if buffer[position] != rune('\n') {
						goto l141
					}
					position++
					if buffer[position] != rune('@') {
						goto l141
					}
					position++
					if buffer[position] != rune('}') {
						goto l141
					}
					position++
					if buffer[position] != rune('@') {
						goto l141
					}
					position++
					if buffer[position] != rune('\n') {
						goto l141
					}
					position++
					if buffer[position] != rune('}') {
						goto l141
					}
					position++
				}
			l143:
				add(ruleObjectBlock, position142)
			}
			return true
		l141:
			position, tokenIndex = position141, tokenIndex141
			return false
		},
		/* 21 ObjectProperty <- <(Identifier ':' Assignable (','? '\n'? ObjectProperty)?)> */
		func() bool {
			position149, tokenIndex149 := position, tokenIndex
			{
				position150 := position
				if !_rules[ruleIdentifier]() {
					goto l149
				}
				if buffer[position] != rune(':') {
					goto l149
				}
				position++
				if !_rules[ruleAssignable]() {
					goto l149
				}
				{
					position151, tokenIndex151 := position, tokenIndex
					{
						position153, tokenIndex153 := position, tokenIndex
						if buffer[position] != rune(',') {
							goto l153
						}
						position++
						goto l154
					l153:
						position, tokenIndex = position153, tokenIndex153
					}
				l154:
					{
						position155, tokenIndex155 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l155
						}
						position++
						goto l156
					l155:
						position, tokenIndex = position155, tokenIndex155
					}
				l156:
					if !_rules[ruleObjectProperty]() {
						goto l151
					}
					goto l152
				l151:
					position, tokenIndex = position151, tokenIndex151
				}
			l152:
				add(ruleObjectProperty, position150)
			}
			return true
		l149:
			position, tokenIndex = position149, tokenIndex149
			return false
		},
		/* 22 If <- <(('i' / 'I') ('f' / 'F') BoolTest (Block / ('=' '>' Expression)) ElseIf? Else?)> */
		func() bool {
			position157, tokenIndex157 := position, tokenIndex
			{
				position158 := position
				{
					position159, tokenIndex159 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l160
					}
					position++
					goto l159
				l160:
					position, tokenIndex = position159, tokenIndex159
					if buffer[position] != rune('I') {
						goto l157
					}
					position++
				}
			l159:
				{
					position161, tokenIndex161 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l162
					}
					position++
					goto l161
				l162:
					position, tokenIndex = position161, tokenIndex161
					if buffer[position] != rune('F') {
						goto l157
					}
					position++
				}
			l161:
				if !_rules[ruleBoolTest]() {
					goto l157
				}
				{
					position163, tokenIndex163 := position, tokenIndex
					if !_rules[ruleBlock]() {
						goto l164
					}
					goto l163
				l164:
					position, tokenIndex = position163, tokenIndex163
					if buffer[position] != rune('=') {
						goto l157
					}
					position++
					if buffer[position] != rune('>') {
						goto l157
					}
					position++
					if !_rules[ruleExpression]() {
						goto l157
					}
				}
			l163:
				{
					position165, tokenIndex165 := position, tokenIndex
					if !_rules[ruleElseIf]() {
						goto l165
					}
					goto l166
				l165:
					position, tokenIndex = position165, tokenIndex165
				}
			l166:
				{
					position167, tokenIndex167 := position, tokenIndex
					if !_rules[ruleElse]() {
						goto l167
					}
					goto l168
				l167:
					position, tokenIndex = position167, tokenIndex167
				}
			l168:
				add(ruleIf, position158)
			}
			return true
		l157:
			position, tokenIndex = position157, tokenIndex157
			return false
		},
		/* 23 ElseIf <- <(('e' / 'E') ('l' / 'L') ('s' / 'S') ('e' / 'E') ('i' / 'I') ('f' / 'F') BoolTest (Block / ('=' '>' Expression)) ElseIf? Else?)> */
		func() bool {
			position169, tokenIndex169 := position, tokenIndex
			{
				position170 := position
				{
					position171, tokenIndex171 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l172
					}
					position++
					goto l171
				l172:
					position, tokenIndex = position171, tokenIndex171
					if buffer[position] != rune('E') {
						goto l169
					}
					position++
				}
			l171:
				{
					position173, tokenIndex173 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l174
					}
					position++
					goto l173
				l174:
					position, tokenIndex = position173, tokenIndex173
					if buffer[position] != rune('L') {
						goto l169
					}
					position++
				}
			l173:
				{
					position175, tokenIndex175 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l176
					}
					position++
					goto l175
				l176:
					position, tokenIndex = position175, tokenIndex175
					if buffer[position] != rune('S') {
						goto l169
					}
					position++
				}
			l175:
				{
					position177, tokenIndex177 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l178
					}
					position++
					goto l177
				l178:
					position, tokenIndex = position177, tokenIndex177
					if buffer[position] != rune('E') {
						goto l169
					}
					position++
				}
			l177:
				{
					position179, tokenIndex179 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l180
					}
					position++
					goto l179
				l180:
					position, tokenIndex = position179, tokenIndex179
					if buffer[position] != rune('I') {
						goto l169
					}
					position++
				}
			l179:
				{
					position181, tokenIndex181 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l182
					}
					position++
					goto l181
				l182:
					position, tokenIndex = position181, tokenIndex181
					if buffer[position] != rune('F') {
						goto l169
					}
					position++
				}
			l181:
				if !_rules[ruleBoolTest]() {
					goto l169
				}
				{
					position183, tokenIndex183 := position, tokenIndex
					if !_rules[ruleBlock]() {
						goto l184
					}
					goto l183
				l184:
					position, tokenIndex = position183, tokenIndex183
					if buffer[position] != rune('=') {
						goto l169
					}
					position++
					if buffer[position] != rune('>') {
						goto l169
					}
					position++
					if !_rules[ruleExpression]() {
						goto l169
					}
				}
			l183:
				{
					position185, tokenIndex185 := position, tokenIndex
					if !_rules[ruleElseIf]() {
						goto l185
					}
					goto l186
				l185:
					position, tokenIndex = position185, tokenIndex185
				}
			l186:
				{
					position187, tokenIndex187 := position, tokenIndex
					if !_rules[ruleElse]() {
						goto l187
					}
					goto l188
				l187:
					position, tokenIndex = position187, tokenIndex187
				}
			l188:
				add(ruleElseIf, position170)
			}
			return true
		l169:
			position, tokenIndex = position169, tokenIndex169
			return false
		},
		/* 24 Else <- <(('e' / 'E') ('l' / 'L') ('s' / 'S') ('e' / 'E') (Block / ('=' '>' Expression)))> */
		func() bool {
			position189, tokenIndex189 := position, tokenIndex
			{
				position190 := position
				{
					position191, tokenIndex191 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l192
					}
					position++
					goto l191
				l192:
					position, tokenIndex = position191, tokenIndex191
					if buffer[position] != rune('E') {
						goto l189
					}
					position++
				}
			l191:
				{
					position193, tokenIndex193 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l194
					}
					position++
					goto l193
				l194:
					position, tokenIndex = position193, tokenIndex193
					if buffer[position] != rune('L') {
						goto l189
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
						goto l189
					}
					position++
				}
			l195:
				{
					position197, tokenIndex197 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l198
					}
					position++
					goto l197
				l198:
					position, tokenIndex = position197, tokenIndex197
					if buffer[position] != rune('E') {
						goto l189
					}
					position++
				}
			l197:
				{
					position199, tokenIndex199 := position, tokenIndex
					if !_rules[ruleBlock]() {
						goto l200
					}
					goto l199
				l200:
					position, tokenIndex = position199, tokenIndex199
					if buffer[position] != rune('=') {
						goto l189
					}
					position++
					if buffer[position] != rune('>') {
						goto l189
					}
					position++
					if !_rules[ruleExpression]() {
						goto l189
					}
				}
			l199:
				add(ruleElse, position190)
			}
			return true
		l189:
			position, tokenIndex = position189, tokenIndex189
			return false
		},
		/* 25 BoolTest <- <(Assignable '<' (BoolTest / Assignable))> */
		func() bool {
			position201, tokenIndex201 := position, tokenIndex
			{
				position202 := position
				if !_rules[ruleAssignable]() {
					goto l201
				}
				if buffer[position] != rune('<') {
					goto l201
				}
				position++
				{
					position203, tokenIndex203 := position, tokenIndex
					if !_rules[ruleBoolTest]() {
						goto l204
					}
					goto l203
				l204:
					position, tokenIndex = position203, tokenIndex203
					if !_rules[ruleAssignable]() {
						goto l201
					}
				}
			l203:
				add(ruleBoolTest, position202)
			}
			return true
		l201:
			position, tokenIndex = position201, tokenIndex201
			return false
		},
		/* 26 Class <- <(('c' / 'C') ('l' / 'L') ('a' / 'A') ('s' / 'S') ('s' / 'S') Identifier ClassBlock)> */
		func() bool {
			position205, tokenIndex205 := position, tokenIndex
			{
				position206 := position
				{
					position207, tokenIndex207 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l208
					}
					position++
					goto l207
				l208:
					position, tokenIndex = position207, tokenIndex207
					if buffer[position] != rune('C') {
						goto l205
					}
					position++
				}
			l207:
				{
					position209, tokenIndex209 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l210
					}
					position++
					goto l209
				l210:
					position, tokenIndex = position209, tokenIndex209
					if buffer[position] != rune('L') {
						goto l205
					}
					position++
				}
			l209:
				{
					position211, tokenIndex211 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l212
					}
					position++
					goto l211
				l212:
					position, tokenIndex = position211, tokenIndex211
					if buffer[position] != rune('A') {
						goto l205
					}
					position++
				}
			l211:
				{
					position213, tokenIndex213 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l214
					}
					position++
					goto l213
				l214:
					position, tokenIndex = position213, tokenIndex213
					if buffer[position] != rune('S') {
						goto l205
					}
					position++
				}
			l213:
				{
					position215, tokenIndex215 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l216
					}
					position++
					goto l215
				l216:
					position, tokenIndex = position215, tokenIndex215
					if buffer[position] != rune('S') {
						goto l205
					}
					position++
				}
			l215:
				if !_rules[ruleIdentifier]() {
					goto l205
				}
				if !_rules[ruleClassBlock]() {
					goto l205
				}
				add(ruleClass, position206)
			}
			return true
		l205:
			position, tokenIndex = position205, tokenIndex205
			return false
		},
		/* 27 ClassBlock <- <('@' '{' '@' '\n' ClassEntry+ ('@' '}' '@'))> */
		func() bool {
			position217, tokenIndex217 := position, tokenIndex
			{
				position218 := position
				if buffer[position] != rune('@') {
					goto l217
				}
				position++
				if buffer[position] != rune('{') {
					goto l217
				}
				position++
				if buffer[position] != rune('@') {
					goto l217
				}
				position++
				if buffer[position] != rune('\n') {
					goto l217
				}
				position++
				if !_rules[ruleClassEntry]() {
					goto l217
				}
			l219:
				{
					position220, tokenIndex220 := position, tokenIndex
					if !_rules[ruleClassEntry]() {
						goto l220
					}
					goto l219
				l220:
					position, tokenIndex = position220, tokenIndex220
				}
				if buffer[position] != rune('@') {
					goto l217
				}
				position++
				if buffer[position] != rune('}') {
					goto l217
				}
				position++
				if buffer[position] != rune('@') {
					goto l217
				}
				position++
				add(ruleClassBlock, position218)
			}
			return true
		l217:
			position, tokenIndex = position217, tokenIndex217
			return false
		},
		/* 28 ClassEntry <- <(Identifier (':' Type)? ('=' Assignable)? '\n')> */
		func() bool {
			position221, tokenIndex221 := position, tokenIndex
			{
				position222 := position
				if !_rules[ruleIdentifier]() {
					goto l221
				}
				{
					position223, tokenIndex223 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l223
					}
					position++
					if !_rules[ruleType]() {
						goto l223
					}
					goto l224
				l223:
					position, tokenIndex = position223, tokenIndex223
				}
			l224:
				{
					position225, tokenIndex225 := position, tokenIndex
					if buffer[position] != rune('=') {
						goto l225
					}
					position++
					if !_rules[ruleAssignable]() {
						goto l225
					}
					goto l226
				l225:
					position, tokenIndex = position225, tokenIndex225
				}
			l226:
				if buffer[position] != rune('\n') {
					goto l221
				}
				position++
				add(ruleClassEntry, position222)
			}
			return true
		l221:
			position, tokenIndex = position221, tokenIndex221
			return false
		},
		/* 29 Array <- <('[' ArrayElem ']')> */
		func() bool {
			position227, tokenIndex227 := position, tokenIndex
			{
				position228 := position
				if buffer[position] != rune('[') {
					goto l227
				}
				position++
				if !_rules[ruleArrayElem]() {
					goto l227
				}
				if buffer[position] != rune(']') {
					goto l227
				}
				position++
				add(ruleArray, position228)
			}
			return true
		l227:
			position, tokenIndex = position227, tokenIndex227
			return false
		},
		/* 30 ArrayElem <- <(Assignable (',' ArrayElem)?)> */
		func() bool {
			position229, tokenIndex229 := position, tokenIndex
			{
				position230 := position
				if !_rules[ruleAssignable]() {
					goto l229
				}
				{
					position231, tokenIndex231 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l231
					}
					position++
					if !_rules[ruleArrayElem]() {
						goto l231
					}
					goto l232
				l231:
					position, tokenIndex = position231, tokenIndex231
				}
			l232:
				add(ruleArrayElem, position230)
			}
			return true
		l229:
			position, tokenIndex = position229, tokenIndex229
			return false
		},
		/* 31 Operation <- <(Operand Operator (Operation / Operand))> */
		func() bool {
			position233, tokenIndex233 := position, tokenIndex
			{
				position234 := position
				if !_rules[ruleOperand]() {
					goto l233
				}
				if !_rules[ruleOperator]() {
					goto l233
				}
				{
					position235, tokenIndex235 := position, tokenIndex
					if !_rules[ruleOperation]() {
						goto l236
					}
					goto l235
				l236:
					position, tokenIndex = position235, tokenIndex235
					if !_rules[ruleOperand]() {
						goto l233
					}
				}
			l235:
				add(ruleOperation, position234)
			}
			return true
		l233:
			position, tokenIndex = position233, tokenIndex233
			return false
		},
		/* 32 Operand <- <(FunctionCall / ComputedProperty / Decimal)> */
		func() bool {
			position237, tokenIndex237 := position, tokenIndex
			{
				position238 := position
				{
					position239, tokenIndex239 := position, tokenIndex
					if !_rules[ruleFunctionCall]() {
						goto l240
					}
					goto l239
				l240:
					position, tokenIndex = position239, tokenIndex239
					if !_rules[ruleComputedProperty]() {
						goto l241
					}
					goto l239
				l241:
					position, tokenIndex = position239, tokenIndex239
					if !_rules[ruleDecimal]() {
						goto l237
					}
				}
			l239:
				add(ruleOperand, position238)
			}
			return true
		l237:
			position, tokenIndex = position237, tokenIndex237
			return false
		},
		/* 33 Operator <- <('+' / '-' / '/' / '*')> */
		func() bool {
			position242, tokenIndex242 := position, tokenIndex
			{
				position243 := position
				{
					position244, tokenIndex244 := position, tokenIndex
					if buffer[position] != rune('+') {
						goto l245
					}
					position++
					goto l244
				l245:
					position, tokenIndex = position244, tokenIndex244
					if buffer[position] != rune('-') {
						goto l246
					}
					position++
					goto l244
				l246:
					position, tokenIndex = position244, tokenIndex244
					if buffer[position] != rune('/') {
						goto l247
					}
					position++
					goto l244
				l247:
					position, tokenIndex = position244, tokenIndex244
					if buffer[position] != rune('*') {
						goto l242
					}
					position++
				}
			l244:
				add(ruleOperator, position243)
			}
			return true
		l242:
			position, tokenIndex = position242, tokenIndex242
			return false
		},
		/* 34 FunctionDeclaration <- <(Arguments? (':' Type)? NoReturn? ('-' '>') (Block / Expression))> */
		func() bool {
			position248, tokenIndex248 := position, tokenIndex
			{
				position249 := position
				{
					position250, tokenIndex250 := position, tokenIndex
					if !_rules[ruleArguments]() {
						goto l250
					}
					goto l251
				l250:
					position, tokenIndex = position250, tokenIndex250
				}
			l251:
				{
					position252, tokenIndex252 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l252
					}
					position++
					if !_rules[ruleType]() {
						goto l252
					}
					goto l253
				l252:
					position, tokenIndex = position252, tokenIndex252
				}
			l253:
				{
					position254, tokenIndex254 := position, tokenIndex
					if !_rules[ruleNoReturn]() {
						goto l254
					}
					goto l255
				l254:
					position, tokenIndex = position254, tokenIndex254
				}
			l255:
				if buffer[position] != rune('-') {
					goto l248
				}
				position++
				if buffer[position] != rune('>') {
					goto l248
				}
				position++
				{
					position256, tokenIndex256 := position, tokenIndex
					if !_rules[ruleBlock]() {
						goto l257
					}
					goto l256
				l257:
					position, tokenIndex = position256, tokenIndex256
					if !_rules[ruleExpression]() {
						goto l248
					}
				}
			l256:
				add(ruleFunctionDeclaration, position249)
			}
			return true
		l248:
			position, tokenIndex = position248, tokenIndex248
			return false
		},
		/* 35 Arguments <- <('(' ArgumentDecl* ')')> */
		func() bool {
			position258, tokenIndex258 := position, tokenIndex
			{
				position259 := position
				if buffer[position] != rune('(') {
					goto l258
				}
				position++
			l260:
				{
					position261, tokenIndex261 := position, tokenIndex
					if !_rules[ruleArgumentDecl]() {
						goto l261
					}
					goto l260
				l261:
					position, tokenIndex = position261, tokenIndex261
				}
				if buffer[position] != rune(')') {
					goto l258
				}
				position++
				add(ruleArguments, position259)
			}
			return true
		l258:
			position, tokenIndex = position258, tokenIndex258
			return false
		},
		/* 36 ArgumentDecl <- <(Identifier (':' Type)? ','?)> */
		func() bool {
			position262, tokenIndex262 := position, tokenIndex
			{
				position263 := position
				if !_rules[ruleIdentifier]() {
					goto l262
				}
				{
					position264, tokenIndex264 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l264
					}
					position++
					if !_rules[ruleType]() {
						goto l264
					}
					goto l265
				l264:
					position, tokenIndex = position264, tokenIndex264
				}
			l265:
				{
					position266, tokenIndex266 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l266
					}
					position++
					goto l267
				l266:
					position, tokenIndex = position266, tokenIndex266
				}
			l267:
				add(ruleArgumentDecl, position263)
			}
			return true
		l262:
			position, tokenIndex = position262, tokenIndex262
			return false
		},
		/* 37 NoReturn <- <'!'> */
		func() bool {
			position268, tokenIndex268 := position, tokenIndex
			{
				position269 := position
				if buffer[position] != rune('!') {
					goto l268
				}
				position++
				add(ruleNoReturn, position269)
			}
			return true
		l268:
			position, tokenIndex = position268, tokenIndex268
			return false
		},
		/* 38 Block <- <('@' '{' '@' '\n' Statement+ ('@' '}' '@'))> */
		func() bool {
			position270, tokenIndex270 := position, tokenIndex
			{
				position271 := position
				if buffer[position] != rune('@') {
					goto l270
				}
				position++
				if buffer[position] != rune('{') {
					goto l270
				}
				position++
				if buffer[position] != rune('@') {
					goto l270
				}
				position++
				if buffer[position] != rune('\n') {
					goto l270
				}
				position++
				if !_rules[ruleStatement]() {
					goto l270
				}
			l272:
				{
					position273, tokenIndex273 := position, tokenIndex
					if !_rules[ruleStatement]() {
						goto l273
					}
					goto l272
				l273:
					position, tokenIndex = position273, tokenIndex273
				}
				if buffer[position] != rune('@') {
					goto l270
				}
				position++
				if buffer[position] != rune('}') {
					goto l270
				}
				position++
				if buffer[position] != rune('@') {
					goto l270
				}
				position++
				add(ruleBlock, position271)
			}
			return true
		l270:
			position, tokenIndex = position270, tokenIndex270
			return false
		},
		/* 39 FunctionCall <- <(VarUse '(' Argument? ')')> */
		func() bool {
			position274, tokenIndex274 := position, tokenIndex
			{
				position275 := position
				if !_rules[ruleVarUse]() {
					goto l274
				}
				if buffer[position] != rune('(') {
					goto l274
				}
				position++
				{
					position276, tokenIndex276 := position, tokenIndex
					if !_rules[ruleArgument]() {
						goto l276
					}
					goto l277
				l276:
					position, tokenIndex = position276, tokenIndex276
				}
			l277:
				if buffer[position] != rune(')') {
					goto l274
				}
				position++
				add(ruleFunctionCall, position275)
			}
			return true
		l274:
			position, tokenIndex = position274, tokenIndex274
			return false
		},
		/* 40 Argument <- <(Assignable (',' Argument)?)> */
		func() bool {
			position278, tokenIndex278 := position, tokenIndex
			{
				position279 := position
				if !_rules[ruleAssignable]() {
					goto l278
				}
				{
					position280, tokenIndex280 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l280
					}
					position++
					if !_rules[ruleArgument]() {
						goto l280
					}
					goto l281
				l280:
					position, tokenIndex = position280, tokenIndex280
				}
			l281:
				add(ruleArgument, position279)
			}
			return true
		l278:
			position, tokenIndex = position278, tokenIndex278
			return false
		},
		/* 41 Identifier <- <(('_' / [a-z] / [A-Z]) ([a-z] / [A-Z] / [0-9] / '_')*)> */
		func() bool {
			position282, tokenIndex282 := position, tokenIndex
			{
				position283 := position
				{
					position284, tokenIndex284 := position, tokenIndex
					if buffer[position] != rune('_') {
						goto l285
					}
					position++
					goto l284
				l285:
					position, tokenIndex = position284, tokenIndex284
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l286
					}
					position++
					goto l284
				l286:
					position, tokenIndex = position284, tokenIndex284
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l282
					}
					position++
				}
			l284:
			l287:
				{
					position288, tokenIndex288 := position, tokenIndex
					{
						position289, tokenIndex289 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l290
						}
						position++
						goto l289
					l290:
						position, tokenIndex = position289, tokenIndex289
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l291
						}
						position++
						goto l289
					l291:
						position, tokenIndex = position289, tokenIndex289
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l292
						}
						position++
						goto l289
					l292:
						position, tokenIndex = position289, tokenIndex289
						if buffer[position] != rune('_') {
							goto l288
						}
						position++
					}
				l289:
					goto l287
				l288:
					position, tokenIndex = position288, tokenIndex288
				}
				add(ruleIdentifier, position283)
			}
			return true
		l282:
			position, tokenIndex = position282, tokenIndex282
			return false
		},
		/* 42 VarUse <- <(('_' / [a-z] / [A-Z]) ([a-z] / [A-Z] / [0-9] / '_')*)> */
		func() bool {
			position293, tokenIndex293 := position, tokenIndex
			{
				position294 := position
				{
					position295, tokenIndex295 := position, tokenIndex
					if buffer[position] != rune('_') {
						goto l296
					}
					position++
					goto l295
				l296:
					position, tokenIndex = position295, tokenIndex295
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l297
					}
					position++
					goto l295
				l297:
					position, tokenIndex = position295, tokenIndex295
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l293
					}
					position++
				}
			l295:
			l298:
				{
					position299, tokenIndex299 := position, tokenIndex
					{
						position300, tokenIndex300 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l301
						}
						position++
						goto l300
					l301:
						position, tokenIndex = position300, tokenIndex300
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l302
						}
						position++
						goto l300
					l302:
						position, tokenIndex = position300, tokenIndex300
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l303
						}
						position++
						goto l300
					l303:
						position, tokenIndex = position300, tokenIndex300
						if buffer[position] != rune('_') {
							goto l299
						}
						position++
					}
				l300:
					goto l298
				l299:
					position, tokenIndex = position299, tokenIndex299
				}
				add(ruleVarUse, position294)
			}
			return true
		l293:
			position, tokenIndex = position293, tokenIndex293
			return false
		},
		/* 43 Type <- <(FuncType / ArrayType / Identifier)> */
		func() bool {
			position304, tokenIndex304 := position, tokenIndex
			{
				position305 := position
				{
					position306, tokenIndex306 := position, tokenIndex
					if !_rules[ruleFuncType]() {
						goto l307
					}
					goto l306
				l307:
					position, tokenIndex = position306, tokenIndex306
					if !_rules[ruleArrayType]() {
						goto l308
					}
					goto l306
				l308:
					position, tokenIndex = position306, tokenIndex306
					if !_rules[ruleIdentifier]() {
						goto l304
					}
				}
			l306:
				add(ruleType, position305)
			}
			return true
		l304:
			position, tokenIndex = position304, tokenIndex304
			return false
		},
		/* 44 ArrayType <- <('[' Type ']')> */
		func() bool {
			position309, tokenIndex309 := position, tokenIndex
			{
				position310 := position
				if buffer[position] != rune('[') {
					goto l309
				}
				position++
				if !_rules[ruleType]() {
					goto l309
				}
				if buffer[position] != rune(']') {
					goto l309
				}
				position++
				add(ruleArrayType, position310)
			}
			return true
		l309:
			position, tokenIndex = position309, tokenIndex309
			return false
		},
		/* 45 FuncType <- <('(' FuncTypeArg ')' ':' Type)> */
		func() bool {
			position311, tokenIndex311 := position, tokenIndex
			{
				position312 := position
				if buffer[position] != rune('(') {
					goto l311
				}
				position++
				if !_rules[ruleFuncTypeArg]() {
					goto l311
				}
				if buffer[position] != rune(')') {
					goto l311
				}
				position++
				if buffer[position] != rune(':') {
					goto l311
				}
				position++
				if !_rules[ruleType]() {
					goto l311
				}
				add(ruleFuncType, position312)
			}
			return true
		l311:
			position, tokenIndex = position311, tokenIndex311
			return false
		},
		/* 46 FuncTypeArg <- <(Type (',' FuncTypeArg)?)> */
		func() bool {
			position313, tokenIndex313 := position, tokenIndex
			{
				position314 := position
				if !_rules[ruleType]() {
					goto l313
				}
				{
					position315, tokenIndex315 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l315
					}
					position++
					if !_rules[ruleFuncTypeArg]() {
						goto l315
					}
					goto l316
				l315:
					position, tokenIndex = position315, tokenIndex315
				}
			l316:
				add(ruleFuncTypeArg, position314)
			}
			return true
		l313:
			position, tokenIndex = position313, tokenIndex313
			return false
		},
		/* 47 Literal <- <(String / Decimal)> */
		func() bool {
			position317, tokenIndex317 := position, tokenIndex
			{
				position318 := position
				{
					position319, tokenIndex319 := position, tokenIndex
					if !_rules[ruleString]() {
						goto l320
					}
					goto l319
				l320:
					position, tokenIndex = position319, tokenIndex319
					if !_rules[ruleDecimal]() {
						goto l317
					}
				}
			l319:
				add(ruleLiteral, position318)
			}
			return true
		l317:
			position, tokenIndex = position317, tokenIndex317
			return false
		},
		/* 48 Decimal <- <('0' / ([1-9] [0-9]*))> */
		func() bool {
			position321, tokenIndex321 := position, tokenIndex
			{
				position322 := position
				{
					position323, tokenIndex323 := position, tokenIndex
					if buffer[position] != rune('0') {
						goto l324
					}
					position++
					goto l323
				l324:
					position, tokenIndex = position323, tokenIndex323
					if c := buffer[position]; c < rune('1') || c > rune('9') {
						goto l321
					}
					position++
				l325:
					{
						position326, tokenIndex326 := position, tokenIndex
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l326
						}
						position++
						goto l325
					l326:
						position, tokenIndex = position326, tokenIndex326
					}
				}
			l323:
				add(ruleDecimal, position322)
			}
			return true
		l321:
			position, tokenIndex = position321, tokenIndex321
			return false
		},
		/* 49 String <- <('"' (!'"' .)* '"')> */
		func() bool {
			position327, tokenIndex327 := position, tokenIndex
			{
				position328 := position
				if buffer[position] != rune('"') {
					goto l327
				}
				position++
			l329:
				{
					position330, tokenIndex330 := position, tokenIndex
					{
						position331, tokenIndex331 := position, tokenIndex
						if buffer[position] != rune('"') {
							goto l331
						}
						position++
						goto l330
					l331:
						position, tokenIndex = position331, tokenIndex331
					}
					if !matchDot() {
						goto l330
					}
					goto l329
				l330:
					position, tokenIndex = position330, tokenIndex330
				}
				if buffer[position] != rune('"') {
					goto l327
				}
				position++
				add(ruleString, position328)
			}
			return true
		l327:
			position, tokenIndex = position327, tokenIndex327
			return false
		},
	}
	p.rules = _rules
}
