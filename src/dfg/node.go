package dfg



type DfgNode interface {
}


type nodeKind int

const (
  nodeAdd : nodeKind = iota
  nodeNumber
)


type NullNode struct {}

func NewNullNode() NullNode {
  return NullNode{}
}
