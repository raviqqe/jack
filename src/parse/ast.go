package parse

import (
  "../dfg"
)



// classes

type AstNode interface {
  GenerateDfg() dfg.DfgNode
}


//// null node

type nullNode struct {}

func (_ nullNode) GenerateDfg() dfg.DfgNode {
  return dfg.NewNullNode()
}
