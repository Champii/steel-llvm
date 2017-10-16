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
	rules  [30]func() bool
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
		/* 1 Statement <- <((External / Expression)? '\n')> */
		func() bool {
			position4, tokenIndex4 := position, tokenIndex
			{
				position5 := position
				{
					position6, tokenIndex6 := position, tokenIndex
					{
						position8, tokenIndex8 := position, tokenIndex
						if !_rules[ruleExternal]() {
							goto l9
						}
						goto l8
					l9:
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
		/* 2 External <- <(('e' / 'E') ('x' / 'X') ('t' / 'T') ('e' / 'E') ('r' / 'R') ('n' / 'N') ('a' / 'A') ('l' / 'L') ExternalBlock)> */
		func() bool {
			position10, tokenIndex10 := position, tokenIndex
			{
				position11 := position
				{
					position12, tokenIndex12 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l13
					}
					position++
					goto l12
				l13:
					position, tokenIndex = position12, tokenIndex12
					if buffer[position] != rune('E') {
						goto l10
					}
					position++
				}
			l12:
				{
					position14, tokenIndex14 := position, tokenIndex
					if buffer[position] != rune('x') {
						goto l15
					}
					position++
					goto l14
				l15:
					position, tokenIndex = position14, tokenIndex14
					if buffer[position] != rune('X') {
						goto l10
					}
					position++
				}
			l14:
				{
					position16, tokenIndex16 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l17
					}
					position++
					goto l16
				l17:
					position, tokenIndex = position16, tokenIndex16
					if buffer[position] != rune('T') {
						goto l10
					}
					position++
				}
			l16:
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
						goto l10
					}
					position++
				}
			l18:
				{
					position20, tokenIndex20 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l21
					}
					position++
					goto l20
				l21:
					position, tokenIndex = position20, tokenIndex20
					if buffer[position] != rune('R') {
						goto l10
					}
					position++
				}
			l20:
				{
					position22, tokenIndex22 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l23
					}
					position++
					goto l22
				l23:
					position, tokenIndex = position22, tokenIndex22
					if buffer[position] != rune('N') {
						goto l10
					}
					position++
				}
			l22:
				{
					position24, tokenIndex24 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l25
					}
					position++
					goto l24
				l25:
					position, tokenIndex = position24, tokenIndex24
					if buffer[position] != rune('A') {
						goto l10
					}
					position++
				}
			l24:
				{
					position26, tokenIndex26 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l27
					}
					position++
					goto l26
				l27:
					position, tokenIndex = position26, tokenIndex26
					if buffer[position] != rune('L') {
						goto l10
					}
					position++
				}
			l26:
				if !_rules[ruleExternalBlock]() {
					goto l10
				}
				add(ruleExternal, position11)
			}
			return true
		l10:
			position, tokenIndex = position10, tokenIndex10
			return false
		},
		/* 3 ExternalBlock <- <('@' '{' '@' '\n' ExternalDef+ ('@' '}' '@'))> */
		func() bool {
			position28, tokenIndex28 := position, tokenIndex
			{
				position29 := position
				if buffer[position] != rune('@') {
					goto l28
				}
				position++
				if buffer[position] != rune('{') {
					goto l28
				}
				position++
				if buffer[position] != rune('@') {
					goto l28
				}
				position++
				if buffer[position] != rune('\n') {
					goto l28
				}
				position++
				if !_rules[ruleExternalDef]() {
					goto l28
				}
			l30:
				{
					position31, tokenIndex31 := position, tokenIndex
					if !_rules[ruleExternalDef]() {
						goto l31
					}
					goto l30
				l31:
					position, tokenIndex = position31, tokenIndex31
				}
				if buffer[position] != rune('@') {
					goto l28
				}
				position++
				if buffer[position] != rune('}') {
					goto l28
				}
				position++
				if buffer[position] != rune('@') {
					goto l28
				}
				position++
				add(ruleExternalBlock, position29)
			}
			return true
		l28:
			position, tokenIndex = position28, tokenIndex28
			return false
		},
		/* 4 ExternalDef <- <(Identifier '(' ExternalArgument? ')' ':' Type '\n')> */
		func() bool {
			position32, tokenIndex32 := position, tokenIndex
			{
				position33 := position
				if !_rules[ruleIdentifier]() {
					goto l32
				}
				if buffer[position] != rune('(') {
					goto l32
				}
				position++
				{
					position34, tokenIndex34 := position, tokenIndex
					if !_rules[ruleExternalArgument]() {
						goto l34
					}
					goto l35
				l34:
					position, tokenIndex = position34, tokenIndex34
				}
			l35:
				if buffer[position] != rune(')') {
					goto l32
				}
				position++
				if buffer[position] != rune(':') {
					goto l32
				}
				position++
				if !_rules[ruleType]() {
					goto l32
				}
				if buffer[position] != rune('\n') {
					goto l32
				}
				position++
				add(ruleExternalDef, position33)
			}
			return true
		l32:
			position, tokenIndex = position32, tokenIndex32
			return false
		},
		/* 5 ExternalArgument <- <(Type (',' ExternalArgument)?)> */
		func() bool {
			position36, tokenIndex36 := position, tokenIndex
			{
				position37 := position
				if !_rules[ruleType]() {
					goto l36
				}
				{
					position38, tokenIndex38 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l38
					}
					position++
					if !_rules[ruleExternalArgument]() {
						goto l38
					}
					goto l39
				l38:
					position, tokenIndex = position38, tokenIndex38
				}
			l39:
				add(ruleExternalArgument, position37)
			}
			return true
		l36:
			position, tokenIndex = position36, tokenIndex36
			return false
		},
		/* 6 Expression <- <(Return / Assignation / Assignable)> */
		func() bool {
			position40, tokenIndex40 := position, tokenIndex
			{
				position41 := position
				{
					position42, tokenIndex42 := position, tokenIndex
					if !_rules[ruleReturn]() {
						goto l43
					}
					goto l42
				l43:
					position, tokenIndex = position42, tokenIndex42
					if !_rules[ruleAssignation]() {
						goto l44
					}
					goto l42
				l44:
					position, tokenIndex = position42, tokenIndex42
					if !_rules[ruleAssignable]() {
						goto l40
					}
				}
			l42:
				add(ruleExpression, position41)
			}
			return true
		l40:
			position, tokenIndex = position40, tokenIndex40
			return false
		},
		/* 7 Return <- <(('r' / 'R') ('e' / 'E') ('t' / 'T') ('u' / 'U') ('r' / 'R') ('n' / 'N') Assignable)> */
		func() bool {
			position45, tokenIndex45 := position, tokenIndex
			{
				position46 := position
				{
					position47, tokenIndex47 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l48
					}
					position++
					goto l47
				l48:
					position, tokenIndex = position47, tokenIndex47
					if buffer[position] != rune('R') {
						goto l45
					}
					position++
				}
			l47:
				{
					position49, tokenIndex49 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l50
					}
					position++
					goto l49
				l50:
					position, tokenIndex = position49, tokenIndex49
					if buffer[position] != rune('E') {
						goto l45
					}
					position++
				}
			l49:
				{
					position51, tokenIndex51 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l52
					}
					position++
					goto l51
				l52:
					position, tokenIndex = position51, tokenIndex51
					if buffer[position] != rune('T') {
						goto l45
					}
					position++
				}
			l51:
				{
					position53, tokenIndex53 := position, tokenIndex
					if buffer[position] != rune('u') {
						goto l54
					}
					position++
					goto l53
				l54:
					position, tokenIndex = position53, tokenIndex53
					if buffer[position] != rune('U') {
						goto l45
					}
					position++
				}
			l53:
				{
					position55, tokenIndex55 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l56
					}
					position++
					goto l55
				l56:
					position, tokenIndex = position55, tokenIndex55
					if buffer[position] != rune('R') {
						goto l45
					}
					position++
				}
			l55:
				{
					position57, tokenIndex57 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l58
					}
					position++
					goto l57
				l58:
					position, tokenIndex = position57, tokenIndex57
					if buffer[position] != rune('N') {
						goto l45
					}
					position++
				}
			l57:
				if !_rules[ruleAssignable]() {
					goto l45
				}
				add(ruleReturn, position46)
			}
			return true
		l45:
			position, tokenIndex = position45, tokenIndex45
			return false
		},
		/* 8 Assignation <- <(Identifier (':' Type)? '=' Assignable)> */
		func() bool {
			position59, tokenIndex59 := position, tokenIndex
			{
				position60 := position
				if !_rules[ruleIdentifier]() {
					goto l59
				}
				{
					position61, tokenIndex61 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l61
					}
					position++
					if !_rules[ruleType]() {
						goto l61
					}
					goto l62
				l61:
					position, tokenIndex = position61, tokenIndex61
				}
			l62:
				if buffer[position] != rune('=') {
					goto l59
				}
				position++
				if !_rules[ruleAssignable]() {
					goto l59
				}
				add(ruleAssignation, position60)
			}
			return true
		l59:
			position, tokenIndex = position59, tokenIndex59
			return false
		},
		/* 9 Assignable <- <(Operation / FunctionDeclaration / Array / FunctionCall / VarUse / Literal)> */
		func() bool {
			position63, tokenIndex63 := position, tokenIndex
			{
				position64 := position
				{
					position65, tokenIndex65 := position, tokenIndex
					if !_rules[ruleOperation]() {
						goto l66
					}
					goto l65
				l66:
					position, tokenIndex = position65, tokenIndex65
					if !_rules[ruleFunctionDeclaration]() {
						goto l67
					}
					goto l65
				l67:
					position, tokenIndex = position65, tokenIndex65
					if !_rules[ruleArray]() {
						goto l68
					}
					goto l65
				l68:
					position, tokenIndex = position65, tokenIndex65
					if !_rules[ruleFunctionCall]() {
						goto l69
					}
					goto l65
				l69:
					position, tokenIndex = position65, tokenIndex65
					if !_rules[ruleVarUse]() {
						goto l70
					}
					goto l65
				l70:
					position, tokenIndex = position65, tokenIndex65
					if !_rules[ruleLiteral]() {
						goto l63
					}
				}
			l65:
				add(ruleAssignable, position64)
			}
			return true
		l63:
			position, tokenIndex = position63, tokenIndex63
			return false
		},
		/* 10 Array <- <('[' ArrayElem ']')> */
		func() bool {
			position71, tokenIndex71 := position, tokenIndex
			{
				position72 := position
				if buffer[position] != rune('[') {
					goto l71
				}
				position++
				if !_rules[ruleArrayElem]() {
					goto l71
				}
				if buffer[position] != rune(']') {
					goto l71
				}
				position++
				add(ruleArray, position72)
			}
			return true
		l71:
			position, tokenIndex = position71, tokenIndex71
			return false
		},
		/* 11 ArrayElem <- <(Assignable (',' ArrayElem)?)> */
		func() bool {
			position73, tokenIndex73 := position, tokenIndex
			{
				position74 := position
				if !_rules[ruleAssignable]() {
					goto l73
				}
				{
					position75, tokenIndex75 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l75
					}
					position++
					if !_rules[ruleArrayElem]() {
						goto l75
					}
					goto l76
				l75:
					position, tokenIndex = position75, tokenIndex75
				}
			l76:
				add(ruleArrayElem, position74)
			}
			return true
		l73:
			position, tokenIndex = position73, tokenIndex73
			return false
		},
		/* 12 Operation <- <(Operand Operator (Operation / Operand))> */
		func() bool {
			position77, tokenIndex77 := position, tokenIndex
			{
				position78 := position
				if !_rules[ruleOperand]() {
					goto l77
				}
				if !_rules[ruleOperator]() {
					goto l77
				}
				{
					position79, tokenIndex79 := position, tokenIndex
					if !_rules[ruleOperation]() {
						goto l80
					}
					goto l79
				l80:
					position, tokenIndex = position79, tokenIndex79
					if !_rules[ruleOperand]() {
						goto l77
					}
				}
			l79:
				add(ruleOperation, position78)
			}
			return true
		l77:
			position, tokenIndex = position77, tokenIndex77
			return false
		},
		/* 13 Operand <- <(FunctionCall / VarUse / Decimal)> */
		func() bool {
			position81, tokenIndex81 := position, tokenIndex
			{
				position82 := position
				{
					position83, tokenIndex83 := position, tokenIndex
					if !_rules[ruleFunctionCall]() {
						goto l84
					}
					goto l83
				l84:
					position, tokenIndex = position83, tokenIndex83
					if !_rules[ruleVarUse]() {
						goto l85
					}
					goto l83
				l85:
					position, tokenIndex = position83, tokenIndex83
					if !_rules[ruleDecimal]() {
						goto l81
					}
				}
			l83:
				add(ruleOperand, position82)
			}
			return true
		l81:
			position, tokenIndex = position81, tokenIndex81
			return false
		},
		/* 14 Operator <- <('+' / '-' / '/' / '*')> */
		func() bool {
			position86, tokenIndex86 := position, tokenIndex
			{
				position87 := position
				{
					position88, tokenIndex88 := position, tokenIndex
					if buffer[position] != rune('+') {
						goto l89
					}
					position++
					goto l88
				l89:
					position, tokenIndex = position88, tokenIndex88
					if buffer[position] != rune('-') {
						goto l90
					}
					position++
					goto l88
				l90:
					position, tokenIndex = position88, tokenIndex88
					if buffer[position] != rune('/') {
						goto l91
					}
					position++
					goto l88
				l91:
					position, tokenIndex = position88, tokenIndex88
					if buffer[position] != rune('*') {
						goto l86
					}
					position++
				}
			l88:
				add(ruleOperator, position87)
			}
			return true
		l86:
			position, tokenIndex = position86, tokenIndex86
			return false
		},
		/* 15 FunctionDeclaration <- <(Arguments? (':' Type)? NoReturn? ('-' '>') (Expression / Block))> */
		func() bool {
			position92, tokenIndex92 := position, tokenIndex
			{
				position93 := position
				{
					position94, tokenIndex94 := position, tokenIndex
					if !_rules[ruleArguments]() {
						goto l94
					}
					goto l95
				l94:
					position, tokenIndex = position94, tokenIndex94
				}
			l95:
				{
					position96, tokenIndex96 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l96
					}
					position++
					if !_rules[ruleType]() {
						goto l96
					}
					goto l97
				l96:
					position, tokenIndex = position96, tokenIndex96
				}
			l97:
				{
					position98, tokenIndex98 := position, tokenIndex
					if !_rules[ruleNoReturn]() {
						goto l98
					}
					goto l99
				l98:
					position, tokenIndex = position98, tokenIndex98
				}
			l99:
				if buffer[position] != rune('-') {
					goto l92
				}
				position++
				if buffer[position] != rune('>') {
					goto l92
				}
				position++
				{
					position100, tokenIndex100 := position, tokenIndex
					if !_rules[ruleExpression]() {
						goto l101
					}
					goto l100
				l101:
					position, tokenIndex = position100, tokenIndex100
					if !_rules[ruleBlock]() {
						goto l92
					}
				}
			l100:
				add(ruleFunctionDeclaration, position93)
			}
			return true
		l92:
			position, tokenIndex = position92, tokenIndex92
			return false
		},
		/* 16 Arguments <- <('(' ArgumentDecl* ')')> */
		func() bool {
			position102, tokenIndex102 := position, tokenIndex
			{
				position103 := position
				if buffer[position] != rune('(') {
					goto l102
				}
				position++
			l104:
				{
					position105, tokenIndex105 := position, tokenIndex
					if !_rules[ruleArgumentDecl]() {
						goto l105
					}
					goto l104
				l105:
					position, tokenIndex = position105, tokenIndex105
				}
				if buffer[position] != rune(')') {
					goto l102
				}
				position++
				add(ruleArguments, position103)
			}
			return true
		l102:
			position, tokenIndex = position102, tokenIndex102
			return false
		},
		/* 17 ArgumentDecl <- <(Identifier (':' Type)? ','?)> */
		func() bool {
			position106, tokenIndex106 := position, tokenIndex
			{
				position107 := position
				if !_rules[ruleIdentifier]() {
					goto l106
				}
				{
					position108, tokenIndex108 := position, tokenIndex
					if buffer[position] != rune(':') {
						goto l108
					}
					position++
					if !_rules[ruleType]() {
						goto l108
					}
					goto l109
				l108:
					position, tokenIndex = position108, tokenIndex108
				}
			l109:
				{
					position110, tokenIndex110 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l110
					}
					position++
					goto l111
				l110:
					position, tokenIndex = position110, tokenIndex110
				}
			l111:
				add(ruleArgumentDecl, position107)
			}
			return true
		l106:
			position, tokenIndex = position106, tokenIndex106
			return false
		},
		/* 18 NoReturn <- <'!'> */
		func() bool {
			position112, tokenIndex112 := position, tokenIndex
			{
				position113 := position
				if buffer[position] != rune('!') {
					goto l112
				}
				position++
				add(ruleNoReturn, position113)
			}
			return true
		l112:
			position, tokenIndex = position112, tokenIndex112
			return false
		},
		/* 19 Block <- <('@' '{' '@' '\n' Statement+ ('@' '}' '@'))> */
		func() bool {
			position114, tokenIndex114 := position, tokenIndex
			{
				position115 := position
				if buffer[position] != rune('@') {
					goto l114
				}
				position++
				if buffer[position] != rune('{') {
					goto l114
				}
				position++
				if buffer[position] != rune('@') {
					goto l114
				}
				position++
				if buffer[position] != rune('\n') {
					goto l114
				}
				position++
				if !_rules[ruleStatement]() {
					goto l114
				}
			l116:
				{
					position117, tokenIndex117 := position, tokenIndex
					if !_rules[ruleStatement]() {
						goto l117
					}
					goto l116
				l117:
					position, tokenIndex = position117, tokenIndex117
				}
				if buffer[position] != rune('@') {
					goto l114
				}
				position++
				if buffer[position] != rune('}') {
					goto l114
				}
				position++
				if buffer[position] != rune('@') {
					goto l114
				}
				position++
				add(ruleBlock, position115)
			}
			return true
		l114:
			position, tokenIndex = position114, tokenIndex114
			return false
		},
		/* 20 FunctionCall <- <(VarUse '(' Argument* ')')> */
		func() bool {
			position118, tokenIndex118 := position, tokenIndex
			{
				position119 := position
				if !_rules[ruleVarUse]() {
					goto l118
				}
				if buffer[position] != rune('(') {
					goto l118
				}
				position++
			l120:
				{
					position121, tokenIndex121 := position, tokenIndex
					if !_rules[ruleArgument]() {
						goto l121
					}
					goto l120
				l121:
					position, tokenIndex = position121, tokenIndex121
				}
				if buffer[position] != rune(')') {
					goto l118
				}
				position++
				add(ruleFunctionCall, position119)
			}
			return true
		l118:
			position, tokenIndex = position118, tokenIndex118
			return false
		},
		/* 21 Argument <- <(Assignable ','?)> */
		func() bool {
			position122, tokenIndex122 := position, tokenIndex
			{
				position123 := position
				if !_rules[ruleAssignable]() {
					goto l122
				}
				{
					position124, tokenIndex124 := position, tokenIndex
					if buffer[position] != rune(',') {
						goto l124
					}
					position++
					goto l125
				l124:
					position, tokenIndex = position124, tokenIndex124
				}
			l125:
				add(ruleArgument, position123)
			}
			return true
		l122:
			position, tokenIndex = position122, tokenIndex122
			return false
		},
		/* 22 Identifier <- <(('_' / [a-z] / [A-Z]) ([a-z] / [A-Z] / [0-9] / '_')*)> */
		func() bool {
			position126, tokenIndex126 := position, tokenIndex
			{
				position127 := position
				{
					position128, tokenIndex128 := position, tokenIndex
					if buffer[position] != rune('_') {
						goto l129
					}
					position++
					goto l128
				l129:
					position, tokenIndex = position128, tokenIndex128
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l130
					}
					position++
					goto l128
				l130:
					position, tokenIndex = position128, tokenIndex128
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l126
					}
					position++
				}
			l128:
			l131:
				{
					position132, tokenIndex132 := position, tokenIndex
					{
						position133, tokenIndex133 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l134
						}
						position++
						goto l133
					l134:
						position, tokenIndex = position133, tokenIndex133
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l135
						}
						position++
						goto l133
					l135:
						position, tokenIndex = position133, tokenIndex133
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l136
						}
						position++
						goto l133
					l136:
						position, tokenIndex = position133, tokenIndex133
						if buffer[position] != rune('_') {
							goto l132
						}
						position++
					}
				l133:
					goto l131
				l132:
					position, tokenIndex = position132, tokenIndex132
				}
				add(ruleIdentifier, position127)
			}
			return true
		l126:
			position, tokenIndex = position126, tokenIndex126
			return false
		},
		/* 23 VarUse <- <(('_' / [a-z] / [A-Z]) ([a-z] / [A-Z] / [0-9] / '_')*)> */
		func() bool {
			position137, tokenIndex137 := position, tokenIndex
			{
				position138 := position
				{
					position139, tokenIndex139 := position, tokenIndex
					if buffer[position] != rune('_') {
						goto l140
					}
					position++
					goto l139
				l140:
					position, tokenIndex = position139, tokenIndex139
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l141
					}
					position++
					goto l139
				l141:
					position, tokenIndex = position139, tokenIndex139
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l137
					}
					position++
				}
			l139:
			l142:
				{
					position143, tokenIndex143 := position, tokenIndex
					{
						position144, tokenIndex144 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l145
						}
						position++
						goto l144
					l145:
						position, tokenIndex = position144, tokenIndex144
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l146
						}
						position++
						goto l144
					l146:
						position, tokenIndex = position144, tokenIndex144
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l147
						}
						position++
						goto l144
					l147:
						position, tokenIndex = position144, tokenIndex144
						if buffer[position] != rune('_') {
							goto l143
						}
						position++
					}
				l144:
					goto l142
				l143:
					position, tokenIndex = position143, tokenIndex143
				}
				add(ruleVarUse, position138)
			}
			return true
		l137:
			position, tokenIndex = position137, tokenIndex137
			return false
		},
		/* 24 Type <- <(ArrayType / Identifier)> */
		func() bool {
			position148, tokenIndex148 := position, tokenIndex
			{
				position149 := position
				{
					position150, tokenIndex150 := position, tokenIndex
					if !_rules[ruleArrayType]() {
						goto l151
					}
					goto l150
				l151:
					position, tokenIndex = position150, tokenIndex150
					if !_rules[ruleIdentifier]() {
						goto l148
					}
				}
			l150:
				add(ruleType, position149)
			}
			return true
		l148:
			position, tokenIndex = position148, tokenIndex148
			return false
		},
		/* 25 ArrayType <- <('[' Type ']')> */
		func() bool {
			position152, tokenIndex152 := position, tokenIndex
			{
				position153 := position
				if buffer[position] != rune('[') {
					goto l152
				}
				position++
				if !_rules[ruleType]() {
					goto l152
				}
				if buffer[position] != rune(']') {
					goto l152
				}
				position++
				add(ruleArrayType, position153)
			}
			return true
		l152:
			position, tokenIndex = position152, tokenIndex152
			return false
		},
		/* 26 Literal <- <(String / Decimal)> */
		func() bool {
			position154, tokenIndex154 := position, tokenIndex
			{
				position155 := position
				{
					position156, tokenIndex156 := position, tokenIndex
					if !_rules[ruleString]() {
						goto l157
					}
					goto l156
				l157:
					position, tokenIndex = position156, tokenIndex156
					if !_rules[ruleDecimal]() {
						goto l154
					}
				}
			l156:
				add(ruleLiteral, position155)
			}
			return true
		l154:
			position, tokenIndex = position154, tokenIndex154
			return false
		},
		/* 27 Decimal <- <('0' / ([1-9] [0-9]*))> */
		func() bool {
			position158, tokenIndex158 := position, tokenIndex
			{
				position159 := position
				{
					position160, tokenIndex160 := position, tokenIndex
					if buffer[position] != rune('0') {
						goto l161
					}
					position++
					goto l160
				l161:
					position, tokenIndex = position160, tokenIndex160
					if c := buffer[position]; c < rune('1') || c > rune('9') {
						goto l158
					}
					position++
				l162:
					{
						position163, tokenIndex163 := position, tokenIndex
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l163
						}
						position++
						goto l162
					l163:
						position, tokenIndex = position163, tokenIndex163
					}
				}
			l160:
				add(ruleDecimal, position159)
			}
			return true
		l158:
			position, tokenIndex = position158, tokenIndex158
			return false
		},
		/* 28 String <- <('"' (!'"' .)* '"')> */
		func() bool {
			position164, tokenIndex164 := position, tokenIndex
			{
				position165 := position
				if buffer[position] != rune('"') {
					goto l164
				}
				position++
			l166:
				{
					position167, tokenIndex167 := position, tokenIndex
					{
						position168, tokenIndex168 := position, tokenIndex
						if buffer[position] != rune('"') {
							goto l168
						}
						position++
						goto l167
					l168:
						position, tokenIndex = position168, tokenIndex168
					}
					if !matchDot() {
						goto l167
					}
					goto l166
				l167:
					position, tokenIndex = position167, tokenIndex167
				}
				if buffer[position] != rune('"') {
					goto l164
				}
				position++
				add(ruleString, position165)
			}
			return true
		l164:
			position, tokenIndex = position164, tokenIndex164
			return false
		},
	}
	p.rules = _rules
}
