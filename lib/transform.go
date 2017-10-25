package steel

import (
	"log"
	"strings"
)

func parseNode(steel *SteelParser, node *node32) *Node {
	newNode := new(Node)

	newNode.token = node.pegRule
	newNode.value = string(([]rune(steel.buffer)[node.begin:node.end]))

	return newNode
}

func recurParse(line int, steel *SteelParser, parent *Node, node *node32) {
	for node != nil {
		newNode := parseNode(steel, node)
		newNode.line = line
		parent.AddChildren(newNode)
		if node.up != nil {
			recurParse(strings.Count(steel.Buffer[:node.end], "\n")+1, steel, newNode, node.up)
		}
		node = node.next
	}
}

func getNewAst(steel *SteelParser) *Node {
	oldRoot := steel.AST()
	newRoot := new(Node)
	line := 1

	recurParse(line, steel, newRoot, oldRoot)

	// fmt.Println(newRoot.children[0].token)
	return newRoot
}

func getAst(steel *SteelParser) *Node {
	steel.Init()
	steel.Pretty = true

	if err := steel.Parse(); err != nil {
		log.Panic(err)
	}

	return getNewAst(steel)
}
