package parse

import (
  "bufio"
  "io"
  "ioutil"
  "os"
)



type Context struct {
  indentLevel int
  indentLiteral string

  // debug information
  filename string
  lineNumber int
  cachedErrorMessage string
}

func newParserState(file os.File,
                    nodes chan<- AstNode,
                    errorMessages chan<- string) parserState {
  return parserState{
    scanner : ioutil.ReadFile(filename),
    nodes : nodes,
    errorMessages : errorMessages,

    indentLevel : 0,
    indentLiteral : dummyString,

    filename : file.Name(),
    lineNumber : 0,
    cachedErrorMessage : "",
  }
}

func (s *parserState) Module() {
  position := 0

  for {
    if ok, result, newPosition := s.statement(position)(); ok {
      position = newPosition

      switch value := result.(type) {
      case AstNode:
        nodes <- value
      case nil: // End of file
        break
      default:
        panic("Unknown type of parsed results is detected.")
      }
    } else {
      errorMessages <- s.debugInfo.Message
    }
  }
}

func (s *parserState) cacheErrorMessage(message string) {
  s.debugInfo.Message += fmt.Sprintf(
      "parse:%s:%d: %s\n",
      s.debugInfo.Filename,
      s.debugInfo.LineNumber,
      message)
}
