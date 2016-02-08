package parse

import (
  "container/list"
  "unicode"
)



// types

type parserResult interface {}

type position int

type parser func() (bool, parserResult, position)



// parsers

//// statements

func (s *parserState) statement(position) parser {
  return func() {
    result, ok, position := or(s.typeDeclaration(position),
                               s.definition(position))()
    if !ok {
      s.cacheErrorMessage(
          "Failed to parse a statement. A import, type declaration, "
          "or definition statement is expected.")
      return nil, false
    }

    return result, true
  }
}


func (p *parserState) definition() parser {
  return func() {
    result, ok := and(s.identifier()(), many(s.identifier()), )()

    s.indentLevel += 1
    or(and(s.equalSign()))
    s.indentLevel -= 1

    option(and(s.newLine(), many(s.indent(), s.statement()))))()
  }
}


func (s *parserState) leftSideOfDefinition() parser {
  return func() {
    result, ok := and(s.equalSign(), s.expression)
    if !ok {
      s.cacheErrorMessage("Failed to parse left side of definition statement.")
      return nil, false
    }

    return result, true
  }
}


func (s *parserState) increaseIndentLevel() parser {
  return func() {
    s.indentLevel += 1
    return nil, true
  }
}


func (s *parserState) decreaseIndentLevel() parser {
  return func() {
    s.indentLevel -= 1
    return nil, true
  }
}


func (s *parserState) indent() parser {
  return func() {
    return nil, true
  }
}


func (s *parserState) blanks() parser {
  return func() {
    _, ok := many(s.blank())
    if !ok {
      panic("Failed to parse blanks.")
    }

    return nil, true
  }
}


func (s *parserState) blank() parser {


}



// parser combinators

func or(parsers ...parser) parser {
  return func() {
    for p := range parsers {
      result, ok := p()
      if ok {
        return result, true
      }
    }

    return nil, false
  }
}


func and(parsers ...parser) parser {
  return func() {
    for p := range parsers {
      result, ok := p()
      if !ok {
        return nil, false
      }
    }

    return result, true
  }
}


func many(p parser) parser {
  return func() {
    results := list.New()

    for {
      result, ok := parser()
      if !ok {
        break
      }

      results.PushBack(result)
    }

    return results.ToSlice(), true
  }
}



//// primitives

func (s *parserState) char() parser {

}


func (s *parserState) char() parser {
  return func() {
    if s.position >= len(s.line) {
      if s.scanner.Scan() {
        s.line = s.scanner.Text() + "\n"
        s.position = 0
        s.widthOfLastRune = 0
      } else {
        s.widthOfLastRune = 0
        return nil, true
      }
    }

    r, width := utf8.DecodeRuneInString(s.line[l.position:])
    s.widthOfLastRune = width
    s.position += width

    return r, true
  }
}


func (s *parserState) letter() parser {
  return func() {
    r, ok := s.char()()
    if ok && unicode.isLetter(r) {
      return r, true
    } else {
      s.debugInfo.message += s.debugMessage("Failed to parse a letter.")
      return nil, false
    }
  }
}
