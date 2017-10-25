package steel

type Preparser struct {
	// variables map[string]llvm.Value
}

func (p *Preparser) Parse(node *Node) []interface{} {
	var res []interface{}

	for _, child := range node.children {
		var toAdd interface{}

		switch child.token {
		case ruleFunctionDeclaration:
			toAdd = p.FunctionDeclaration(child)
		default:
			p.Parse(child)

		}
		res = append(res, toAdd)
	}

	return res
}

func (p *Preparser) FunctionDeclaration(node *Node) interface{} {
	noRet := getChild(node, ruleNoReturn)
	if noRet != nil {
		return nil
	}

	block := node.children[len(node.children)-1]

	if block.token == ruleBlock {
		last := block.children[len(block.children)-1]

		if last.children[0].children[0].token != ruleReturn {
			oldV := last.children[0].children[0]
			last.children[0].children[0] = &Node{token: ruleReturn, parent: block, children: []*Node{oldV}}
			oldV.parent = last.children[0].children[0]
		}
	} else {
		last := block.children[0]
		if last.token != ruleReturn {
			oldV := last
			block.children[0] = &Node{token: ruleReturn, parent: block, children: []*Node{oldV}}
			oldV.parent = block.children[0]
		}

	}

	return nil
}
