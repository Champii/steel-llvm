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
	ruleMultiplicativeExpression
	ruleAdditiveExpression
	ruleRelationalExpression
	ruleEqualityExpression
	ruleAndExpression
	ruleOrExpression
	ruleConditionalExpression
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
	"MultiplicativeExpression",
	"AdditiveExpression",
	"RelationalExpression",
	"EqualityExpression",
	"AndExpression",
	"OrExpression",
	"ConditionalExpression",
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
	rules  [57]func() bool
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
		/* 7 Expression <- <(Class / Return / If / Assignation / ConditionalExpression)> */
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
					if !_rules[ruleConditionalExpression]() {
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
		/* 8 Return <- <(('r' / 'R') ('e' / 'E') ('t' / 'T') ('u' / 'U') ('r' / 'R') ('n' / 'N') ConditionalExpression)> */
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
				if !_rules[ruleConditionalExpression]() {
					goto l57
				}
				add(ruleReturn, position58)
			}
			return true
		l57:
			position, tokenIndex = position57, tokenIndex57
			return false
		},
		/* 9 Assignation <- <(ComputedPropertyUnderef (':' Type)? '=' (FunctionDeclaration / ConditionalExpression))> */
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
				{
					position75, tokenIndex75 := position, tokenIndex
					if !_rules[ruleFunctionDeclaration]() {
						goto l76
					}
					goto l75
				l76:
					position, tokenIndex = position75, tokenIndex75
					if !_rules[ruleConditionalExpression]() {
						goto l71
					}
				}
			l75:
				add(ruleAssignation, position72)
			}
			return true
		l71:
			position, tokenIndex = position71, tokenIndex71
			return false
		},
		/* 10 Assignable <- <(New / Object / FunctionDeclaration / Array / FunctionCall / ComputedProperty / Literal)> */
		func() bool {
			position77, tokenIndex77 := position, tokenIndex
			{
				position78 := position
				{
					position79, tokenIndex79 := position, tokenIndex
					if !_rules[ruleNew]() {
						goto l80
					}
					goto l79
				l80:
					position, tokenIndex = position79, tokenIndex79
					if !_rules[ruleObject]() {
						goto l81
					}
					goto l79
				l81:
					position, tokenIndex = position79, tokenIndex79
					if !_rules[ruleFunctionDeclaration]() {
						goto l82
					}
					goto l79
				l82:
					position, tokenIndex = position79, tokenIndex79
					if !_rules[ruleArray]() {
						goto l83
					}
					goto l79
				l83:
					position, tokenIndex = position79, tokenIndex79
					if !_rules[ruleFunctionCall]() {
						goto l84
					}
					goto l79
				l84:
					position, tokenIndex = position79, tokenIndex79
					if !_rules[ruleComputedProperty]() {
						goto l85
					}
					goto l79
				l85:
					position, tokenIndex = position79, tokenIndex79
					if !_rules[ruleLiteral]() {
						goto l77
					}
				}
			l79:
				add(ruleAssignable, position78)
			}
			return true
		l77:
			position, tokenIndex = position77, tokenIndex77
			return false
		},
		/* 11 New <- <(('n' / 'N') ('e' / 'E') ('w' / 'W') Identifier '(' Argument* ')')> */
		func() bool {
			position86, tokenIndex86 := position, tokenIndex
			{
				position87 := position
				{
					position88, tokenIndex88 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l89
					}
					position++
					goto l88
				l89:
					position, tokenIndex = position88, tokenIndex88
					if buffer[position] != rune('N') {
						goto l86
					}
					position++
				}
			l88:
				{
					position90, tokenIndex90 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l91
					}
					position++
					goto l90
				l91:
					position, tokenIndex = position90, tokenIndex90
					if buffer[position] != rune('E') {
						goto l86
					}
					position++
				}
			l90:
				{
					position92, tokenIndex92 := position, tokenIndex
					if buffer[position] != rune('w') {
						goto l93
					}
					position++
					goto l92
				l93:
					position, tokenIndex = position92, tokenIndex92
					if buffer[position] != rune('W') {
						goto l86
					}
					position++
				}
			l92:
				if !_rules[ruleIdentifier]() {
					goto l86
				}
				if buffer[position] != rune('(') {
					goto l86
				}
				position++
			l94:
				{
					position95, tokenIndex95 := position, tokenIndex
					if !_rules[ruleArgument]() {
						goto l95
					}
					goto l94
				l95:
					position, tokenIndex = position95, tokenIndex95
				}
				if buffer[position] != rune(')') {
					goto l86
				}
				position++
				add(ruleNew, position87)
			}
			return true
		l86:
			position, tokenIndex = position86, tokenIndex86
			return false
		},
		/* 12 ComputedProperty <- <(Identifier (BraceComputedProperty / DotComputedProperty)?)> */
		func() bool {
			position96, tokenIndex96 := position, tokenIndex
			{
				position97 := position
				if !_rules[ruleIdentifier]() {
					goto l96
				}
				{
					position98, tokenIndex98 := position, tokenIndex
					{
						position100, tokenIndex100 := position, tokenIndex
						if !_rules[ruleBraceComputedProperty]() {
							goto l101
						}
						goto l100
					l101:
						position, tokenIndex = position100, tokenIndex100
						if !_rules[ruleDotComputedProperty]() {
							goto l98
						}
					}
				l100:
					goto l99
				l98:
					position, tokenIndex = position98, tokenIndex98
				}
			l99:
				add(ruleComputedProperty, position97)
			}
			return true
		l96:
			position, tokenIndex = position96, tokenIndex96
			return false
		},
		/* 13 BraceComputedProperty <- <('[' (Operation / FunctionCall / ComputedProperty / Literal) ']' (BraceComputedProperty / DotComputedProperty)?)> */
		func() bool {
			position102, tokenIndex102 := position, tokenIndex
			{
				position103 := position
				if buffer[position] != rune('[') {
					goto l102
				}
				position++
				{
					position104, tokenIndex104 := position, tokenIndex
					if !_rules[ruleOperation]() {
						goto l105
					}
					goto l104
				l105:
					position, tokenIndex = position104, tokenIndex104
					if !_rules[ruleFunctionCall]() {
						goto l106
					}
					goto l104
				l106:
					position, tokenIndex = position104, tokenIndex104
					if !_rules[ruleComputedProperty]() {
						goto l107
					}
					goto l104
				l107:
					position, tokenIndex = position104, tokenIndex104
					if !_rules[ruleLiteral]() {
						goto l102
					}
				}
			l104:
				if buffer[position] != rune(']') {
					goto l102
				}
				position++
				{
					position108, tokenIndex108 := position, tokenIndex
					{
						position110, tokenIndex110 := position, tokenIndex
						if !_rules[ruleBraceComputedProperty]() {
							goto l111
						}
						goto l110
					l111:
						position, tokenIndex = position110, tokenIndex110
						if !_rules[ruleDotComputedProperty]() {
							goto l108
						}
					}
				l110:
					goto l109
				l108:
					position, tokenIndex = position108, tokenIndex108
				}
			l109:
				add(ruleBraceComputedProperty, position103)
			}
			return true
		l102:
			position, tokenIndex = position102, tokenIndex102
			return false
		},
		/* 14 DotComputedProperty <- <('.' ComputedProperty)> */
		func() bool {
			position112, tokenIndex112 := position, tokenIndex
			{
				position113 := position
				if buffer[position] != rune('.') {
					goto l112
				}
				position++
				if !_rules[ruleComputedProperty]() {
					goto l112
				}
				add(ruleDotComputedProperty, position113)
			}
			return true
		l112:
			position, tokenIndex = position112, tokenIndex112
			return false
		},
		/* 15 ComputedPropertyUnderef <- <(Identifier (BraceComputedPropertyUnderef / DotComputedPropertyUnderef)?)> */
		func() bool {
			position114, tokenIndex114 := position, tokenIndex
			{
				position115 := position
				if !_rules[ruleIdentifier]() {
					goto l114
				}
				{
					position116, tokenIndex116 := position, tokenIndex
					{
						position118, tokenIndex118 := position, tokenIndex
						if !_rules[ruleBraceComputedPropertyUnderef]() {
							goto l119
						}
						goto l118
					l119:
						position, tokenIndex = position118, tokenIndex118
						if !_rules[ruleDotComputedPropertyUnderef]() {
							goto l116
						}
					}
				l118:
					goto l117
				l116:
					position, tokenIndex = position116, tokenIndex116
				}
			l117:
				add(ruleComputedPropertyUnderef, position115)
			}
			return true
		l114:
			position, tokenIndex = position114, tokenIndex114
			return false
		},
		/* 16 BraceComputedPropertyUnderef <- <('[' (Operation / FunctionCall / ComputedPropertyUnderef / Literal) ']' (BraceComputedPropertyUnderef / DotComputedPropertyUnderef)?)> */
		func() bool {
			position120, tokenIndex120 := position, tokenIndex
			{
				position121 := position
				if buffer[position] != rune('[') {
					goto l120
				}
				position++
				{
					position122, tokenIndex122 := position, tokenIndex
					if !_rules[ruleOperation]() {
						goto l123
					}
					goto l122
				l123:
					position, tokenIndex = position122, tokenIndex122
					if !_rules[ruleFunctionCall]() {
						goto l124
					}
					goto l122
				l124:
					position, tokenIndex = position122, tokenIndex122
					if !_rules[ruleComputedPropertyUnderef]() {
						goto l125
					}
					goto l122
				l125:
					position, tokenIndex = position122, tokenIndex122
					if !_rules[ruleLiteral]() {
						goto l120
					}
				}
			l122:
				if buffer[position] != rune(']') {
					goto l120
				}
				position++
				{
					position126, tokenIndex126 := position, tokenIndex
					{
						position128, tokenIndex128 := position, tokenIndex
						if !_rules[ruleBraceComputedPropertyUnderef]() {
							goto l129
						}
						goto l128
					l129:
						position, tokenIndex = position128, tokenIndex128
						if !_rules[ruleDotComputedPropertyUnderef]() {
							goto l126
						}
					}
				l128:
					goto l127
				l126:
					position, tokenIndex = position126, tokenIndex126
				}
			l127:
				add(ruleBraceComputedPropertyUnderef, position121)
			}
			return true
		l120:
			position, tokenIndex = position120, tokenIndex120
			return false
		},
		/* 17 DotComputedPropertyUnderef <- <('.' ComputedPropertyUnderef)> */
		func() bool {
			position130, tokenIndex130 := position, tokenIndex
			{
				position131 := position
				if buffer[position] != rune('.') {
					goto l130
				}
				position++
				if !_rules[ruleComputedPropertyUnderef]() {
					goto l130
				}
				add(ruleDotComputedPropertyUnderef, position131)
			}
			return true
		l130:
			position, tokenIndex = position130, tokenIndex130
			return false
		},
		/* 18 Object <- <(ObjectInline / ObjectBlock)> */
		func() bool {
			position132, tokenIndex132 := position, tokenIndex
			{
				position133 := position
				{
					position134, tokenIndex134 := position, tokenIndex
					if !_rules[ruleObjectInline]() {
						goto l135
					}
					goto l134
				l135:
					position, tokenIndex = position134, tokenIndex134
					if !_rules[ruleObjectBlock]() {
						goto l132
					}
				}
			l134:
				add(ruleObject, position133)
			}
			return true
		l132:
			position, tokenIndex = position132, tokenIndex132
			return false
		},
		/* 19 ObjectInline <- <(('{' ObjectProperty? '}') / ObjectProperty)> */
		func() bool {
			position136, tokenIndex136 := position, tokenIndex
			{
				position137 := position
				{
					position138, tokenIndex138 := position, tokenIndex
					if buffer[position] != rune('{') {
						goto l139
					}
					position++
					{
						position140, tokenIndex140 := position, tokenIndex
						if !_rules[ruleObjectProperty]() {
							goto l140
						}
						goto l141
					l140:
						position, tokenIndex = position140, tokenIndex140
					}
				l141:
					if buffer[position] != rune('}') {
						goto l139
					}
					position++
					goto l138
				l139:
					position, tokenIndex = position138, tokenIndex138
					if !_rules[ruleObjectProperty]() {
						goto l136
					}
				}
			l138:
				add(ruleObjectInline, position137)
			}
			return true
		l136:
			position, tokenIndex = position136, tokenIndex136
			return false
		},
		/* 20 ObjectBlock <- <(('@' '{' '@' '\n' ObjectProperty? ('\n' '@' '}' '@')) / ('{' '@' '{' '@' '\n' ObjectProperty? ('\n' '@' '}' '@' '\n' '}')))> */
		func() bool {
			position142, tokenIndex142 := position, tokenIndex
			{
				position143 := position
				{
					position144, tokenIndex144 := position, tokenIndex
					if buffer[position] != rune('@') {
						goto l145
					}
					position++
					if buffer[position] != rune('{') {
						goto l145
					}
					position++
					if buffer[position] != rune('@') {
						goto l145
					}
					position++
					if buffer[position] != rune('\n') {
						goto l145
					}
					position++
					{
						position146, tokenIndex146 := position, tokenIndex
						if !_rules[ruleObjectProperty]() {
							goto l146
						}
						goto l147
					l146:
						position, tokenIndex = position146, tokenIndex146
					}
				l147:
					if buffer[position] != rune('\n') {
						goto l145
					}
					position++
					if buffer[position] != rune('@') {
						goto l145
					}
					position++
					if buffer[position] != rune('}') {
						goto l145
					}
					position++
					if buffer[position] != rune('@') {
						goto l145
					}
					position++
					goto l144
				l145:
					position, tokenIndex = position144, tokenIndex144
					if buffer[position] != rune('{') {
						goto l142
					}
					position++
					if buffer[position] != rune('@') {
						goto l142
					}
					position++
					if buffer[position] != rune('{') {
						goto l142
					}
					position++
					if buffer[position] != rune('@') {
						goto l142
					}
					position++
					if buffer[position] != rune('\n') {
						goto l142
					}
					position++
					{
						position148, tokenIndex148 := position, tokenIndex
						if !_rules[ruleObjectProperty]() {
							goto l148
						}
						goto l149
					l148:
						position, tokenIndex = position148, tokenIndex148
					}
				l149:
					if buffer[position] != rune('\n') {
						goto l142
					}
					position++
					if buffer[position] != rune('@') {
						goto l142
					}
					position++
					if buffer[position] != rune('}') {
						goto l142
					}
					position++
					if buffer[position] != rune('@') {
						goto l142
					}
					position++
					if buffer[position] != rune('\n') {
						goto l142
					}
					position++
					if buffer[position] != rune('}') {
						goto l142
					}
					position++
				}
			l144:
				add(ruleObjectBlock, position143)
			}
			return true
		l142:
			position, tokenIndex = position142, tokenIndex142
			return false
		},
		/* 21 ObjectProperty <- <(Identifier ':' Assignable (','? '\n'? ObjectProperty)?)> */
		func() bool {
			position150, tokenIndex150 := position, tokenIndex
			{
				position151 := position
				if !_rules[ruleIdentifier]() {
					goto l150
				}
				if buffer[position] != rune(':') {
					goto l150
				}
				position++
				if !_rules[ruleAssignable]() {
					goto l150
				}
				{
					position152, tokenIndex152 := position, tokenIndex
					{
						position154, tokenIndex154 := position, tokenIndex
						if buffer[position] != rune(',') {
							goto l154
						}
						position++
						goto l155
					l154:
						position, tokenIndex = position154, tokenIndex154
					}
				l155:
					{
						position156, tokenIndex156 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l156
						}
						position++
						goto l157
					l156:
						position, tokenIndex = position156, tokenIndex156
					}
				l157:
					if !_rules[ruleObjectProperty]() {
						goto l152
					}
					goto l153
				l152:
					position, tokenIndex = position152, tokenIndex152
				}
			l153:
				add(ruleObjectProperty, position151)
			}
			return true
		l150:
			position, tokenIndex = position150, tokenIndex150
			return false
		},
		/* 22 If <- <(('i' / 'I') ('f' / 'F') ConditionalExpression (Block / ('=' '>' Expression)) (ElseIf / Else)?)> */
		func() bool {
			position158, tokenIndex158 := position, tokenIndex
			{
				position159 := position
				{
					position160, tokenIndex160 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l161
					}
					position++
					goto l160
				l161:
					position, tokenIndex = position160, tokenIndex160
					if buffer[position] != rune('I') {
						goto l158
					}
					position++
				}
			l160:
				{
					position162, tokenIndex162 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l163
					}
					position++
					goto l162
				l163:
					position, tokenIndex = position162, tokenIndex162
					if buffer[position] != rune('F') {
						goto l158
					}
					position++
				}
			l162:
				if !_rules[ruleConditionalExpression]() {
					goto l158
				}
				{
					position164, tokenIndex164 := position, tokenIndex
					if !_rules[ruleBlock]() {
						goto l165
					}
					goto l164
				l165:
					position, tokenIndex = position164, tokenIndex164
					if buffer[position] != rune('=') {
						goto l158
					}
					position++
					if buffer[position] != rune('>') {
						goto l158
					}
					position++
					if !_rules[ruleExpression]() {
						goto l158
					}
				}
			l164:
				{
					position166, tokenIndex166 := position, tokenIndex
					{
						position168, tokenIndex168 := position, tokenIndex
						if !_rules[ruleElseIf]() {
							goto l169
						}
						goto l168
					l169:
						position, tokenIndex = position168, tokenIndex168
						if !_rules[ruleElse]() {
							goto l166
						}
					}
				l168:
					goto l167
				l166:
					position, tokenIndex = position166, tokenIndex166
				}
			l167:
				add(ruleIf, position159)
			}
			return true
		l158:
			position, tokenIndex = position158, tokenIndex158
			return false
		},
		/* 23 ElseIf <- <('\n' ('e' / 'E') ('l' / 'L') ('s' / 'S') ('e' / 'E') ('i' / 'I') ('f' / 'F') ConditionalExpression (Block / ('=' '>' Expression)) (ElseIf / Else)?)> */
		func() bool {
			position170, tokenIndex170 := position, tokenIndex
			{
				position171 := position
				if buffer[position] != rune('\n') {
					goto l170
				}
				position++
				{
					position172, tokenIndex172 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l173
					}
					position++
					goto l172
				l173:
					position, tokenIndex = position172, tokenIndex172
					if buffer[position] != rune('E') {
						goto l170
					}
					position++
				}
			l172:
				{
					position174, tokenIndex174 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l175
					}
					position++
					goto l174
				l175:
					position, tokenIndex = position174, tokenIndex174
					if buffer[position] != rune('L') {
						goto l170
					}
					position++
				}
			l174:
				{
					position176, tokenIndex176 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l177
					}
					position++
					goto l176
				l177:
					position, tokenIndex = position176, tokenIndex176
					if buffer[position] != rune('S') {
						goto l170
					}
					position++
				}
			l176:
				{
					position178, tokenIndex178 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l179
					}
					position++
					goto l178
				l179:
					position, tokenIndex = position178, tokenIndex178
					if buffer[position] != rune('E') {
						goto l170
					}
					position++
				}
			l178:
				{
					position180, tokenIndex180 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l181
					}
					position++
					goto l180
				l181:
					position, tokenIndex = position180, tokenIndex180
					if buffer[position] != rune('I') {
						goto l170
					}
					position++
				}
			l180:
				{
					position182, tokenIndex182 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l183
					}
					position++
					goto l182
				l183:
					position, tokenIndex = position182, tokenIndex182
					if buffer[position] != rune('F') {
						goto l170
					}
					position++
				}
			l182:
				if !_rules[ruleConditionalExpression]() {
					goto l170
				}
				{
					position184, tokenIndex184 := position, tokenIndex
					if !_rules[ruleBlock]() {
						goto l185
					}
					goto l184
				l185:
					position, tokenIndex = position184, tokenIndex184
					if buffer[position] != rune('=') {
						goto l170
					}
					position++
					if buffer[position] != rune('>') {
						goto l170
					}
					position++
					if !_rules[ruleExpression]() {
						goto l170
					}
				}
			l184:
				{
					position186, tokenIndex186 := position, tokenIndex
					{
						position188, tokenIndex188 := position, tokenIndex
						if !_rules[ruleElseIf]() {
							goto l189
						}
						goto l188
					l189:
						position, tokenIndex = position188, tokenIndex188
						if !_rules[ruleElse]() {
							goto l186
						}
					}
				l188:
					goto l187
				l186:
					position, tokenIndex = position186, tokenIndex186
				}
			l187:
				add(ruleElseIf, position171)
			}
			return true
		l170:
			position, tokenIndex = position170, tokenIndex170
			return false
		},
		/* 24 Else <- <('\n' ('e' / 'E') ('l' / 'L') ('s' / 'S') ('e' / 'E') (Block / ('=' '>' Expression)))> */
		func() bool {
			position190, tokenIndex190 := position, tokenIndex
			{
				position191 := position
				if buffer[position] != rune('\n') {
					goto l190
				}
				position++
				{
					position192, tokenIndex192 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l193
					}
					position++
					goto l192
				l193:
					position, tokenIndex = position192, tokenIndex192
					if buffer[position] != rune('E') {
						goto l190
					}
					position++
				}
			l192:
				{
					position194, tokenIndex194 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l195
					}
					position++
					goto l194
				l195:
					position, tokenIndex = position194, tokenIndex194
					if buffer[position] != rune('L') {
						goto l190
					}
					position++
				}
			l194:
				{
					position196, tokenIndex196 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l197
					}
					position++
					goto l196
				l197:
					position, tokenIndex = position196, tokenIndex196
					if buffer[position] != rune('S') {
						goto l190
					}
					position++
				}
			l196:
				{
					position198, tokenIndex198 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l199
					}
					position++
					goto l198
				l199:
					position, tokenIndex = position198, tokenIndex198
					if buffer[position] != rune('E') {
						goto l190
					}
					position++
				}
			l198:
				{
					position200, tokenIndex200 := position, tokenIndex
					if !_rules[ruleBlock]() {
						goto l201
					}
					goto l200
				l201:
					position, tokenIndex = position200, tokenIndex200
					if buffer[position] != rune('=') {
						goto l190
					}
					position++
					if buffer[position] != rune('>') {
						goto l190
					}
					position++
					if !_rules[ruleExpression]() {
						goto l190
					}
				}
			l200:
				add(ruleElse, position191)
			}
			return true
		l190:
			position, tokenIndex = position190, tokenIndex190
			return false
		},
		/* 25 MultiplicativeExpression <- <(Assignable (('*' / '/' / '%') Assignable)*)> */
		func() bool {
			position202, tokenIndex202 := position, tokenIndex
			{
				position203 := position
				if !_rules[ruleAssignable]() {
					goto l202
				}
			l204:
				{
					position205, tokenIndex205 := position, tokenIndex
					{
						position206, tokenIndex206 := position, tokenIndex
						if buffer[position] != rune('*') {
							goto l207
						}
						position++
						goto l206
					l207:
						position, tokenIndex = position206, tokenIndex206
						if buffer[position] != rune('/') {
							goto l208
						}
						position++
						goto l206
					l208:
						position, tokenIndex = position206, tokenIndex206
						if buffer[position] != rune('%') {
							goto l205
						}
						position++
					}
				l206:
					if !_rules[ruleAssignable]() {
						goto l205
					}
					goto l204
				l205:
					position, tokenIndex = position205, tokenIndex205
				}
				add(ruleMultiplicativeExpression, position203)
			}
			return true
		l202:
			position, tokenIndex = position202, tokenIndex202
			return false
		},
		/* 26 AdditiveExpression <- <(MultiplicativeExpression (('+' / '-') MultiplicativeExpression)*)> */
		func() bool {
			position209, tokenIndex209 := position, tokenIndex
			{
				position210 := position
				if !_rules[ruleMultiplicativeExpression]() {
					goto l209
				}
			l211:
				{
					position212, tokenIndex212 := position, tokenIndex
					{
						position213, tokenIndex213 := position, tokenIndex
						if buffer[position] != rune('+') {
							goto l214
						}
						position++
						goto l213
					l214:
						position, tokenIndex = position213, tokenIndex213
						if buffer[position] != rune('-') {
							goto l212
						}
						position++
					}
				l213:
					if !_rules[ruleMultiplicativeExpression]() {
						goto l212
					}
					goto l211
				l212:
					position, tokenIndex = position212, tokenIndex212
				}
				add(ruleAdditiveExpression, position210)
			}
			return true
		l209:
			position, tokenIndex = position209, tokenIndex209
			return false
		},
		/* 27 RelationalExpression <- <(AdditiveExpression (('<' / ('<' '=') / '>' / ('>' '=')) AdditiveExpression)*)> */
		func() bool {
			position215, tokenIndex215 := position, tokenIndex
			{
				position216 := position
				if !_rules[ruleAdditiveExpression]() {
					goto l215
				}
			l217:
				{
					position218, tokenIndex218 := position, tokenIndex
					{
						position219, tokenIndex219 := position, tokenIndex
						if buffer[position] != rune('<') {
							goto l220
						}
						position++
						goto l219
					l220:
						position, tokenIndex = position219, tokenIndex219
						if buffer[position] != rune('<') {
							goto l221
						}
						position++
						if buffer[position] != rune('=') {
							goto l221
						}
						position++
						goto l219
					l221:
						position, tokenIndex = position219, tokenIndex219
						if buffer[position] != rune('>') {
							goto l222
						}
						position++
						goto l219
					l222:
						position, tokenIndex = position219, tokenIndex219
						if buffer[position] != rune('>') {
							goto l218
						}
						position++
						if buffer[position] != rune('=') {
							goto l218
						}
						position++
					}
				l219:
					if !_rules[ruleAdditiveExpression]() {
						goto l218
					}
					goto l217
				l218:
					position, tokenIndex = position218, tokenIndex218
				}
				add(ruleRelationalExpression, position216)
			}
			return true
		l215:
			position, tokenIndex = position215, tokenIndex215
			return false
		},
		/* 28 EqualityExpression <- <(RelationalExpression ('=' '=' RelationalExpression)*)> */
		func() bool {
			position223, tokenIndex223 := position, tokenIndex
			{
				position224 := position
				if !_rules[ruleRelationalExpression]() {
					goto l223
				}
			l225:
				{
					position226, tokenIndex226 := position, tokenIndex
					if buffer[position] != rune('=') {
						goto l226
					}
					position++
					if buffer[position] != rune('=') {
						goto l226
					}
					position++
					if !_rules[ruleRelationalExpression]() {
						goto l226
					}
					goto l225
				l226:
					position, tokenIndex = position226, tokenIndex226
				}
				add(ruleEqualityExpression, position224)
			}
			return true
		l223:
			position, tokenIndex = position223, tokenIndex223
			return false
		},
		/* 29 AndExpression <- <(EqualityExpression ('&' '&' EqualityExpression)*)> */
		func() bool {
			position227, tokenIndex227 := position, tokenIndex
			{
				position228 := position
				if !_rules[ruleEqualityExpression]() {
					goto l227
				}
			l229:
				{
					position230, tokenIndex230 := position, tokenIndex
					if buffer[position] != rune('&') {
						goto l230
					}
					position++
					if buffer[position] != rune('&') {
						goto l230
					}
					position++
					if !_rules[ruleEqualityExpression]() {
						goto l230
					}
					goto l229
				l230:
					position, tokenIndex = position230, tokenIndex230
				}
				add(ruleAndExpression, position228)
			}
			return true
		l227:
			position, tokenIndex = position227, tokenIndex227
			return false
		},
		/* 30 OrExpression <- <(AndExpression ('|' '|' AndExpression)*)> */
		func() bool {
			position231, tokenIndex231 := position, tokenIndex
			{
				position232 := position
				if !_rules[ruleAndExpression]() {
					goto l231
				}
			l233:
				{
					position234, tokenIndex234 := position, tokenIndex
					if buffer[position] != rune('|') {
						goto l234
					}
					position++
					if buffer[position] != rune('|') {
						goto l234
					}
					position++
					if !_rules[ruleAndExpression]() {
						goto l234
					}
					goto l233
				l234:
					position, tokenIndex = position234, tokenIndex234
				}
				add(ruleOrExpression, position232)
			}
			return true
		l231:
			position, tokenIndex = position231, tokenIndex231
			return false
		},
		/* 31 ConditionalExpression <- <(OrExpression ('?' ConditionalExpression ':' ConditionalExpression)*)> */
		func() bool {
			position235, tokenIndex235 := position, tokenIndex
			{
				position236 := position
				if !_rules[ruleOrExpression]() {
					goto l235
				}
			l237:
				{
					position238, tokenIndex238 := position, tokenIndex
					if buffer[position] != rune('?') {
						goto l238
					}
					position++
					if !_rules[ruleConditionalExpression]() {
						goto l238
					}
					if buffer[position] != rune(':') {
						goto l238
					}
					position++
					if !_rules[ruleConditionalExpression]() {
						goto l238
					}
					goto l237
				l238:
					position, tokenIndex = position238, tokenIndex238
				}
				add(ruleConditionalExpression, position236)
			}
			return true
		l235:
			position, tokenIndex = position235, tokenIndex235
			return false
		},
		/* 32 Class <- <(('c' / 'C') ('l' / 'L') ('a' / 'A') ('s' / 'S') ('s' / 'S') Identifier ClassBlock)> */
		func() bool {
			position239, tokenIndex239 := position, tokenIndex
			{
				position240 := position
				{
					position241, tokenIndex241 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l242
					}
					position++
					goto l241
				l242:
					position, tokenIndex = position241, tokenIndex241
					if buffer[position] != rune('C') {
						goto l239
					}
					position++
				}
			l241:
				{
					position243, tokenIndex243 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l244
					}
					position++
					goto l243
				l244:
					position, tokenIndex = position243, tokenIndex243
					if buffer[position] != rune('L') {
						goto l239
					}
					position++
				}
			l243:
				{
					position245, tokenIndex245 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l246
					}
					position++
					goto l245
				l246:
					position, tokenIndex = position245, tokenIndex245
					if buffer[position] != rune('A') {
						goto l239
					}
					position++
				}
			l245:
				{
					position247, tokenIndex247 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l248
					}
					position++
					goto l247
				l248:
					position, tokenIndex = position247, tokenIndex247
					if buffer[position] != rune('S') {
						goto l239
					}
					position++
				}
			l247:
				{
					position249, tokenIndex249 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l250
					}
					position++
					goto l249
				l250:
					position, tokenIndex = position249, tokenIndex249
					if buffer[position] != rune('S') {
						goto l239
					}
					position++
				}
			l249:
				if !_rules[ruleIdentifier]() {
					goto l239
				}
				if !_rules[ruleClassBlock]() {
					goto l239
				}
				add(ruleClass, position240)
			}
			return true
		l239:
			position, tokenIndex = position239, tokenIndex239
			return false
		},
		/* 33 ClassBlock <- <('@' '{' '@' '\n' ClassEntry+ ('@' '}' '@'))> */
		func() bool {
			position251, tokenIndex251 := position, tokenIndex
			{
				position252 := position
				if buffer[position] != rune('@') {
					goto l251
				}
				position++
				if buffer[position] != rune('{') {
					goto l251
				}
				position++
				if buffer[position] != rune('@') {
					goto l251
				}
				position++
				if buffer[position] != rune('\n') {
					goto l251
				}
				position++
				if !_rules[ruleClassEntry]() {
					goto l251
				}
			l253:
				{
					position254, tokenIndex254 := position, tokenIndex
					if !_rules[ruleClassEntry]() {
						goto l254
					}
					goto l253
				l254:
					position, tokenIndex = position254, tokenIndex254
				}
				if buffer[position] != rune('@') {
					goto l251
				}
				position++
				if buffer[position] != rune('}') {
					goto l251
				}
				position++
				if buffer[position] != rune('@') {
					goto l251
				}
				position++
				add(ruleClassBlock, position252)
			}
			return true
		l251:
			position, tokenIndex = position251, tokenIndex251
			return false
		},
		/* 34 ClassEntry <- <(Identifier (':' Type)? ('=' Assignable)? '\n')> */
		func() bool {
			position255, tokenIndex255 := position, tokenIndex
			{
				position256 := position
				if !_rules[ruleIdentifier]() {
					goto l255
				}
				{
					position257, tokenIndex257 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l257
					}
					position++
					if !_rules[ruleType]() {
						goto l257
					}
					goto l258
				l257:
					position, tokenIndex = position257, tokenIndex257
				}
			l258:
				{
					position259, tokenIndex259 := position, tokenIndex
					if buffer[position] != rune('=') {
						goto l259
					}
					position++
					if !_rules[ruleAssignable]() {
						goto l259
					}
					goto l260
				l259:
					position, tokenIndex = position259, tokenIndex259
				}
			l260:
				if buffer[position] != rune('\n') {
					goto l255
				}
				position++
				add(ruleClassEntry, position256)
			}
			return true
		l255:
			position, tokenIndex = position255, tokenIndex255
			return false
		},
		/* 35 Array <- <('[' ArrayElem ']')> */
		func() bool {
			position261, tokenIndex261 := position, tokenIndex
			{
				position262 := position
				if buffer[position] != rune('[') {
					goto l261
				}
				position++
				if !_rules[ruleArrayElem]() {
					goto l261
				}
				if buffer[position] != rune(']') {
					goto l261
				}
				position++
				add(ruleArray, position262)
			}
			return true
		l261:
			position, tokenIndex = position261, tokenIndex261
			return false
		},
		/* 36 ArrayElem <- <(Assignable (',' ArrayElem)?)> */
		func() bool {
			position263, tokenIndex263 := position, tokenIndex
			{
				position264 := position
				if !_rules[ruleAssignable]() {
					goto l263
				}
				{
					position265, tokenIndex265 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l265
					}
					position++
					if !_rules[ruleArrayElem]() {
						goto l265
					}
					goto l266
				l265:
					position, tokenIndex = position265, tokenIndex265
				}
			l266:
				add(ruleArrayElem, position264)
			}
			return true
		l263:
			position, tokenIndex = position263, tokenIndex263
			return false
		},
		/* 37 Operation <- <(Operand Operator (Operation / Operand))> */
		func() bool {
			position267, tokenIndex267 := position, tokenIndex
			{
				position268 := position
				if !_rules[ruleOperand]() {
					goto l267
				}
				if !_rules[ruleOperator]() {
					goto l267
				}
				{
					position269, tokenIndex269 := position, tokenIndex
					if !_rules[ruleOperation]() {
						goto l270
					}
					goto l269
				l270:
					position, tokenIndex = position269, tokenIndex269
					if !_rules[ruleOperand]() {
						goto l267
					}
				}
			l269:
				add(ruleOperation, position268)
			}
			return true
		l267:
			position, tokenIndex = position267, tokenIndex267
			return false
		},
		/* 38 Operand <- <(FunctionCall / ComputedProperty / Decimal)> */
		func() bool {
			position271, tokenIndex271 := position, tokenIndex
			{
				position272 := position
				{
					position273, tokenIndex273 := position, tokenIndex
					if !_rules[ruleFunctionCall]() {
						goto l274
					}
					goto l273
				l274:
					position, tokenIndex = position273, tokenIndex273
					if !_rules[ruleComputedProperty]() {
						goto l275
					}
					goto l273
				l275:
					position, tokenIndex = position273, tokenIndex273
					if !_rules[ruleDecimal]() {
						goto l271
					}
				}
			l273:
				add(ruleOperand, position272)
			}
			return true
		l271:
			position, tokenIndex = position271, tokenIndex271
			return false
		},
		/* 39 Operator <- <('+' / '-' / '/' / '*')> */
		func() bool {
			position276, tokenIndex276 := position, tokenIndex
			{
				position277 := position
				{
					position278, tokenIndex278 := position, tokenIndex
					if buffer[position] != rune('+') {
						goto l279
					}
					position++
					goto l278
				l279:
					position, tokenIndex = position278, tokenIndex278
					if buffer[position] != rune('-') {
						goto l280
					}
					position++
					goto l278
				l280:
					position, tokenIndex = position278, tokenIndex278
					if buffer[position] != rune('/') {
						goto l281
					}
					position++
					goto l278
				l281:
					position, tokenIndex = position278, tokenIndex278
					if buffer[position] != rune('*') {
						goto l276
					}
					position++
				}
			l278:
				add(ruleOperator, position277)
			}
			return true
		l276:
			position, tokenIndex = position276, tokenIndex276
			return false
		},
		/* 40 FunctionDeclaration <- <(Arguments? (':' Type)? NoReturn? ('-' '>') (Block / Expression))> */
		func() bool {
			position282, tokenIndex282 := position, tokenIndex
			{
				position283 := position
				{
					position284, tokenIndex284 := position, tokenIndex
					if !_rules[ruleArguments]() {
						goto l284
					}
					goto l285
				l284:
					position, tokenIndex = position284, tokenIndex284
				}
			l285:
				{
					position286, tokenIndex286 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l286
					}
					position++
					if !_rules[ruleType]() {
						goto l286
					}
					goto l287
				l286:
					position, tokenIndex = position286, tokenIndex286
				}
			l287:
				{
					position288, tokenIndex288 := position, tokenIndex
					if !_rules[ruleNoReturn]() {
						goto l288
					}
					goto l289
				l288:
					position, tokenIndex = position288, tokenIndex288
				}
			l289:
				if buffer[position] != rune('-') {
					goto l282
				}
				position++
				if buffer[position] != rune('>') {
					goto l282
				}
				position++
				{
					position290, tokenIndex290 := position, tokenIndex
					if !_rules[ruleBlock]() {
						goto l291
					}
					goto l290
				l291:
					position, tokenIndex = position290, tokenIndex290
					if !_rules[ruleExpression]() {
						goto l282
					}
				}
			l290:
				add(ruleFunctionDeclaration, position283)
			}
			return true
		l282:
			position, tokenIndex = position282, tokenIndex282
			return false
		},
		/* 41 Arguments <- <('(' ArgumentDecl* ')')> */
		func() bool {
			position292, tokenIndex292 := position, tokenIndex
			{
				position293 := position
				if buffer[position] != rune('(') {
					goto l292
				}
				position++
			l294:
				{
					position295, tokenIndex295 := position, tokenIndex
					if !_rules[ruleArgumentDecl]() {
						goto l295
					}
					goto l294
				l295:
					position, tokenIndex = position295, tokenIndex295
				}
				if buffer[position] != rune(')') {
					goto l292
				}
				position++
				add(ruleArguments, position293)
			}
			return true
		l292:
			position, tokenIndex = position292, tokenIndex292
			return false
		},
		/* 42 ArgumentDecl <- <(Identifier (':' Type)? ','?)> */
		func() bool {
			position296, tokenIndex296 := position, tokenIndex
			{
				position297 := position
				if !_rules[ruleIdentifier]() {
					goto l296
				}
				{
					position298, tokenIndex298 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l298
					}
					position++
					if !_rules[ruleType]() {
						goto l298
					}
					goto l299
				l298:
					position, tokenIndex = position298, tokenIndex298
				}
			l299:
				{
					position300, tokenIndex300 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l300
					}
					position++
					goto l301
				l300:
					position, tokenIndex = position300, tokenIndex300
				}
			l301:
				add(ruleArgumentDecl, position297)
			}
			return true
		l296:
			position, tokenIndex = position296, tokenIndex296
			return false
		},
		/* 43 NoReturn <- <'!'> */
		func() bool {
			position302, tokenIndex302 := position, tokenIndex
			{
				position303 := position
				if buffer[position] != rune('!') {
					goto l302
				}
				position++
				add(ruleNoReturn, position303)
			}
			return true
		l302:
			position, tokenIndex = position302, tokenIndex302
			return false
		},
		/* 44 Block <- <('@' '{' '@' '\n' Statement+ ('@' '}' '@'))> */
		func() bool {
			position304, tokenIndex304 := position, tokenIndex
			{
				position305 := position
				if buffer[position] != rune('@') {
					goto l304
				}
				position++
				if buffer[position] != rune('{') {
					goto l304
				}
				position++
				if buffer[position] != rune('@') {
					goto l304
				}
				position++
				if buffer[position] != rune('\n') {
					goto l304
				}
				position++
				if !_rules[ruleStatement]() {
					goto l304
				}
			l306:
				{
					position307, tokenIndex307 := position, tokenIndex
					if !_rules[ruleStatement]() {
						goto l307
					}
					goto l306
				l307:
					position, tokenIndex = position307, tokenIndex307
				}
				if buffer[position] != rune('@') {
					goto l304
				}
				position++
				if buffer[position] != rune('}') {
					goto l304
				}
				position++
				if buffer[position] != rune('@') {
					goto l304
				}
				position++
				add(ruleBlock, position305)
			}
			return true
		l304:
			position, tokenIndex = position304, tokenIndex304
			return false
		},
		/* 45 FunctionCall <- <(VarUse '(' Argument? ')')> */
		func() bool {
			position308, tokenIndex308 := position, tokenIndex
			{
				position309 := position
				if !_rules[ruleVarUse]() {
					goto l308
				}
				if buffer[position] != rune('(') {
					goto l308
				}
				position++
				{
					position310, tokenIndex310 := position, tokenIndex
					if !_rules[ruleArgument]() {
						goto l310
					}
					goto l311
				l310:
					position, tokenIndex = position310, tokenIndex310
				}
			l311:
				if buffer[position] != rune(')') {
					goto l308
				}
				position++
				add(ruleFunctionCall, position309)
			}
			return true
		l308:
			position, tokenIndex = position308, tokenIndex308
			return false
		},
		/* 46 Argument <- <(Assignable (',' Argument)?)> */
		func() bool {
			position312, tokenIndex312 := position, tokenIndex
			{
				position313 := position
				if !_rules[ruleAssignable]() {
					goto l312
				}
				{
					position314, tokenIndex314 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l314
					}
					position++
					if !_rules[ruleArgument]() {
						goto l314
					}
					goto l315
				l314:
					position, tokenIndex = position314, tokenIndex314
				}
			l315:
				add(ruleArgument, position313)
			}
			return true
		l312:
			position, tokenIndex = position312, tokenIndex312
			return false
		},
		/* 47 Identifier <- <(('_' / [a-z] / [A-Z]) ([a-z] / [A-Z] / [0-9] / '_')*)> */
		func() bool {
			position316, tokenIndex316 := position, tokenIndex
			{
				position317 := position
				{
					position318, tokenIndex318 := position, tokenIndex
					if buffer[position] != rune('_') {
						goto l319
					}
					position++
					goto l318
				l319:
					position, tokenIndex = position318, tokenIndex318
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l320
					}
					position++
					goto l318
				l320:
					position, tokenIndex = position318, tokenIndex318
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l316
					}
					position++
				}
			l318:
			l321:
				{
					position322, tokenIndex322 := position, tokenIndex
					{
						position323, tokenIndex323 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l324
						}
						position++
						goto l323
					l324:
						position, tokenIndex = position323, tokenIndex323
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l325
						}
						position++
						goto l323
					l325:
						position, tokenIndex = position323, tokenIndex323
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l326
						}
						position++
						goto l323
					l326:
						position, tokenIndex = position323, tokenIndex323
						if buffer[position] != rune('_') {
							goto l322
						}
						position++
					}
				l323:
					goto l321
				l322:
					position, tokenIndex = position322, tokenIndex322
				}
				add(ruleIdentifier, position317)
			}
			return true
		l316:
			position, tokenIndex = position316, tokenIndex316
			return false
		},
		/* 48 VarUse <- <(('_' / [a-z] / [A-Z]) ([a-z] / [A-Z] / [0-9] / '_')*)> */
		func() bool {
			position327, tokenIndex327 := position, tokenIndex
			{
				position328 := position
				{
					position329, tokenIndex329 := position, tokenIndex
					if buffer[position] != rune('_') {
						goto l330
					}
					position++
					goto l329
				l330:
					position, tokenIndex = position329, tokenIndex329
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l331
					}
					position++
					goto l329
				l331:
					position, tokenIndex = position329, tokenIndex329
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l327
					}
					position++
				}
			l329:
			l332:
				{
					position333, tokenIndex333 := position, tokenIndex
					{
						position334, tokenIndex334 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l335
						}
						position++
						goto l334
					l335:
						position, tokenIndex = position334, tokenIndex334
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l336
						}
						position++
						goto l334
					l336:
						position, tokenIndex = position334, tokenIndex334
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l337
						}
						position++
						goto l334
					l337:
						position, tokenIndex = position334, tokenIndex334
						if buffer[position] != rune('_') {
							goto l333
						}
						position++
					}
				l334:
					goto l332
				l333:
					position, tokenIndex = position333, tokenIndex333
				}
				add(ruleVarUse, position328)
			}
			return true
		l327:
			position, tokenIndex = position327, tokenIndex327
			return false
		},
		/* 49 Type <- <(FuncType / ArrayType / Identifier)> */
		func() bool {
			position338, tokenIndex338 := position, tokenIndex
			{
				position339 := position
				{
					position340, tokenIndex340 := position, tokenIndex
					if !_rules[ruleFuncType]() {
						goto l341
					}
					goto l340
				l341:
					position, tokenIndex = position340, tokenIndex340
					if !_rules[ruleArrayType]() {
						goto l342
					}
					goto l340
				l342:
					position, tokenIndex = position340, tokenIndex340
					if !_rules[ruleIdentifier]() {
						goto l338
					}
				}
			l340:
				add(ruleType, position339)
			}
			return true
		l338:
			position, tokenIndex = position338, tokenIndex338
			return false
		},
		/* 50 ArrayType <- <('[' Type ']')> */
		func() bool {
			position343, tokenIndex343 := position, tokenIndex
			{
				position344 := position
				if buffer[position] != rune('[') {
					goto l343
				}
				position++
				if !_rules[ruleType]() {
					goto l343
				}
				if buffer[position] != rune(']') {
					goto l343
				}
				position++
				add(ruleArrayType, position344)
			}
			return true
		l343:
			position, tokenIndex = position343, tokenIndex343
			return false
		},
		/* 51 FuncType <- <('(' FuncTypeArg ')' ':' Type)> */
		func() bool {
			position345, tokenIndex345 := position, tokenIndex
			{
				position346 := position
				if buffer[position] != rune('(') {
					goto l345
				}
				position++
				if !_rules[ruleFuncTypeArg]() {
					goto l345
				}
				if buffer[position] != rune(')') {
					goto l345
				}
				position++
				if buffer[position] != rune(':') {
					goto l345
				}
				position++
				if !_rules[ruleType]() {
					goto l345
				}
				add(ruleFuncType, position346)
			}
			return true
		l345:
			position, tokenIndex = position345, tokenIndex345
			return false
		},
		/* 52 FuncTypeArg <- <(Type (',' FuncTypeArg)?)> */
		func() bool {
			position347, tokenIndex347 := position, tokenIndex
			{
				position348 := position
				if !_rules[ruleType]() {
					goto l347
				}
				{
					position349, tokenIndex349 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l349
					}
					position++
					if !_rules[ruleFuncTypeArg]() {
						goto l349
					}
					goto l350
				l349:
					position, tokenIndex = position349, tokenIndex349
				}
			l350:
				add(ruleFuncTypeArg, position348)
			}
			return true
		l347:
			position, tokenIndex = position347, tokenIndex347
			return false
		},
		/* 53 Literal <- <(String / Decimal)> */
		func() bool {
			position351, tokenIndex351 := position, tokenIndex
			{
				position352 := position
				{
					position353, tokenIndex353 := position, tokenIndex
					if !_rules[ruleString]() {
						goto l354
					}
					goto l353
				l354:
					position, tokenIndex = position353, tokenIndex353
					if !_rules[ruleDecimal]() {
						goto l351
					}
				}
			l353:
				add(ruleLiteral, position352)
			}
			return true
		l351:
			position, tokenIndex = position351, tokenIndex351
			return false
		},
		/* 54 Decimal <- <('0' / ([1-9] [0-9]*))> */
		func() bool {
			position355, tokenIndex355 := position, tokenIndex
			{
				position356 := position
				{
					position357, tokenIndex357 := position, tokenIndex
					if buffer[position] != rune('0') {
						goto l358
					}
					position++
					goto l357
				l358:
					position, tokenIndex = position357, tokenIndex357
					if c := buffer[position]; c < rune('1') || c > rune('9') {
						goto l355
					}
					position++
				l359:
					{
						position360, tokenIndex360 := position, tokenIndex
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l360
						}
						position++
						goto l359
					l360:
						position, tokenIndex = position360, tokenIndex360
					}
				}
			l357:
				add(ruleDecimal, position356)
			}
			return true
		l355:
			position, tokenIndex = position355, tokenIndex355
			return false
		},
		/* 55 String <- <('"' (!'"' .)* '"')> */
		func() bool {
			position361, tokenIndex361 := position, tokenIndex
			{
				position362 := position
				if buffer[position] != rune('"') {
					goto l361
				}
				position++
			l363:
				{
					position364, tokenIndex364 := position, tokenIndex
					{
						position365, tokenIndex365 := position, tokenIndex
						if buffer[position] != rune('"') {
							goto l365
						}
						position++
						goto l364
					l365:
						position, tokenIndex = position365, tokenIndex365
					}
					if !matchDot() {
						goto l364
					}
					goto l363
				l364:
					position, tokenIndex = position364, tokenIndex364
				}
				if buffer[position] != rune('"') {
					goto l361
				}
				position++
				add(ruleString, position362)
			}
			return true
		l361:
			position, tokenIndex = position361, tokenIndex361
			return false
		},
	}
	p.rules = _rules
}
