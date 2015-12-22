package parse



func Parse(filename string,
           nodes chan<- AstNode,
           errorMessages chan<- string) {
  newParserState(filename, nodes, errorMessages).Module()
}
