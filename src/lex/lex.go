package "lex"



func Lex(runes <-chan rune, tokens chan<- Token) {
  newLexer().lex(runes, tokens)
}
