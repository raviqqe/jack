package "lex"



type Token interface {
  Kind tokenKind
  Value
}


type tokenKind int


const (
  tokenEOF tokenKind = iota

  tokenComma

  tokenIf
  tokenElse

  tokenEqualSign
  tokenPlusSign
  tokenMinusSign

  tokenLeftParen
  tokenRightParen
  tokenLeftCurlyBracket
  tokenRightCurlyBracket
  tokenLeftSquareBracket
  tokenRightSquareBracket

  tokenStringLiteral
  tokenIntLiteral
  tokenRealLiteral

  tokenBlockBegin
  tokenBlockEnd
)
