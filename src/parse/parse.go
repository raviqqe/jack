package parse



func Parse(file os.File, nodes chan<- AstNode, errorMessages chan<- string) {
  newParserState(file, nodes, errorMessages).Module()
}
