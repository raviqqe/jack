package parse



// constants

type tokenKind int

const (
  tokenEOF : tokenKind = iota

  tokenIdentifier

  // keywords
  tokenIf
  tokenThen
  tokenElse
  tokenMod
  tokenAnd
  tokenOr

  // literals
  tokenInteger
  tokenReal
  tokenString

  // punctuation
  tokenLeftParen
  tokenRightParen
  tokenLeftSquareBracket
  tokenRightSquareBracket
  tokenLeftCurlyBracket
  tokenRightCurlyBracket
  tokenIndent
  tokenComment
  tokenBlank

  // operators
  tokenEqualSign

  tokenPlus
  tokenMinus
  tokenStar
  tokenSlash
  tokenCaret

  tokenDoubleEqualSign
  tokenLessThan
  tokenGreaterThan
  tokenLessThanOrEqualTo
  tokenGreaterThanOrEqualTo
)


var key = map[string]tokenKind{
  "if" : tokenIf,
  "then" : tokenThen,
  "else" : tokenElse,
  "mod" : tokenMod,
  "and" : tokenAnd,
  "or" : tokenOr,
}



// classes

type token struct {
  kind tokenKind
  value string
}

func NewToken(kind tokenKind, value string) {
  return token{
    kind : kind,
    value : value,
  }
}
