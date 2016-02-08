package "lex"



type lexer struct {
}


func newLexer() lexer {
  return lexer{}
}


func (l *lexer) lex(runes <-chan rune, tokens chan<- Token) {
  for r := range runes {

  }
}
