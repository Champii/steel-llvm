package steel

type Node struct {
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
