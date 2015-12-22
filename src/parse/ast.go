package parse

import (
  "../dfg"
)



type AstNode interface {
  GenerateDfg() dfg.DfgNode
}


type nullNode struct {}

func (_ nullNode) GenerateDfg() dfg.DfgNode {
  return dfg.NewNullNode()
}


type DefinitionNode struct {
  functionName string
  argumentNames []string
  statements []StatementNode
  expression ExpressionNode
}

func (node DefinitionNode) GenerateDfg() dfg.DfgNode {
  // FIXME
  return dfg.NewNullNode()
}


type TypeDeclarationNode struct {
  functionName string
  typeExpression TypeNode
}

func (node TypeDeclarationNode) GenerateDfg() dfg.DfgNode {
  // FIXME
  return dfg.NewNullNode()
}


type Expression struct {
}
