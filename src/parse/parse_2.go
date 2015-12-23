package parse

import (
  "bufio"
  "fmt"
  "io"
  "os"
)



func Parse(file *os.File,
           nodes chan<- AstNode,
           errorMessages chan<- string) {
  p := newParser(astNodes, errorMessages)
  p.parse(newSourceCode(file))
}


type parser struct {
  astNodes chan<- AstNode,
  errorMessages chan<- string,

  context parserContext,
  debugInfo parserDebugInfo,
}

func newParser(astNodes chan<- AstNode, errorMessages chan<- string) parser {
  return parser{
    astNodes : astNodes,
    errorMessages : errorMessages,
    cachedErrorMessage : "",

    context : newParserContext(),
    debugInfo : newParserDebugInfo(),
  }
}

func (p *parser) parse(sc sourceCode) {
  position := 0

  for {
    if result, newPosition, ok := s.statement(sc, position)(); ok {
      position = newPosition

      switch value := result.(type) {
      case AstNode:
        p.astNodes <- value
      case nil: // End of file
        break
      default:
        panic("Unknown type of parsed results is detected.")
      }
    } else {
      p.errorMessages <- p.cachedErrorMessage
    }
  }
}

func (p *parser) cacheErrorMessage(message string) {
  p.chachedErrorMessage += fmt.Sprintf("parse:%s:%d: %s\n",
                                       s.filename,
                                       s.lineNumber,
                                       message)
}


type sourceCode struct {
  scanner bufio.Scanner,
  cachedText []rune,
}

func newSourceCode(filename) sourceCode {
  return ioutil.ReadFile(filename)
}

func (sc *sourceCode) Rune(position int) (rune, error) {
  if position < len(sc.cachedText) {
    return sc.cachedText[position], nil
  } else {
    err := sc.Scan()
    if err != nil {
      return '\0', err
    }

    return sc.Rune(position)
  }
}

func (sc *sourceCode) Substring(startPosition int, endPosition int)
                              (string, error) {
  if startPosition < 0 {
    return "", newParserError("Start positions must be natural numbers.")
  } else if endPosition < len(sc.cachedText) {
    return string(sc.cachedText[startPosition:endPosition]), nil
  } else {
    err := sc.Scan()
    if err != nil {
      return "", newParserError("")
    }

    return sc.Substring(startPosition, endPosition)
  }
}

func (sc *sourceCode) Scan() error {
  err := sc.scanner.Scan()
  if err != nil {
    return err
  }

  sc.cachedText += sc.scanner.Text() + "\n"
  return nil
}


type parserContext struct {
  indentLevel int
  indentLiteral string
}

func newParserContext() parserContext {
  return parserContext{
    indentLevel : 0,
    indentLiteral : dummyString,
  }
}


type parserDebugInfo struct {
  filename string,
  lineNumber int,
}

func newParserDebugInfo() parserDebugInfo {
  return parserDebugInfo{
    filename : dummyString,
    lineNumber : 0,
  }
}


type parserError {
  message string
}

func newParserError(message string) parserError {
  return parserError{
    message : message,
  }
}

func (e parserError) Error() string {
  return e.message
}
