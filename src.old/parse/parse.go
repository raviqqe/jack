package parse



func Parse(filename string,
           nodes chan<- AstNode,
           errorMessages chan<- string) {
  p := newParser(node, errorMessages)
  p.parse(newSourceCode(filename))
}


type sourceCode string

func newSourceCode(filename) sourceCode {
  return ioutil.ReadFile(filename)
}

type parser struct {
  context Context
}


func newParser {
  return parser{
    context : newContext()
  }
}


func (p parser) parse(sc sourceCode) {
  p.Module(
}
