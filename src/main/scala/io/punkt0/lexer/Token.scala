package io.punkt0.lexer

import io.punkt0.Position

sealed trait TokenKind

case object STRLITKIND extends TokenKind
case object INTLITKIND extends TokenKind
case object IDKIND     extends TokenKind

case object BAD       extends TokenKind // invalid token
case object EOF       extends TokenKind
case object COLON     extends TokenKind // :
case object SEMICOLON extends TokenKind // ;
case object DOT       extends TokenKind // .
case object COMMA     extends TokenKind // ,
case object EQSIGN    extends TokenKind // =
case object EQUALS    extends TokenKind // ==
case object BANG      extends TokenKind // !
case object LPAREN    extends TokenKind // (
case object RPAREN    extends TokenKind // )
case object LBRACE    extends TokenKind // {
case object RBRACE    extends TokenKind // }
case object AND       extends TokenKind // &&
case object OR        extends TokenKind // ||
case object LESSTHAN  extends TokenKind // <
case object PLUS      extends TokenKind // +
case object MINUS     extends TokenKind // -
case object TIMES     extends TokenKind // *
case object DIV       extends TokenKind // /
case object OBJECT    extends TokenKind // object
case object CLASS     extends TokenKind // class
case object DEF       extends TokenKind // def
case object OVERRIDE  extends TokenKind // override
case object VAR       extends TokenKind // var
case object UNIT      extends TokenKind // Unit
case object STRING    extends TokenKind // String
case object EXTENDS   extends TokenKind // extends
case object INT       extends TokenKind // Int
case object BOOLEAN   extends TokenKind // Boolean
case object WHILE     extends TokenKind // while
case object IF        extends TokenKind // if
case object ELSE      extends TokenKind // else
case object TRUE      extends TokenKind // true
case object FALSE     extends TokenKind // false
case object THIS      extends TokenKind // this
case object NULL      extends TokenKind // null
case object NEW       extends TokenKind // new
case object PRINTLN   extends TokenKind // println

sealed trait BaseToken:
    def kind: TokenKind
    def position: Position

// identifiers
case class ID(value: String, position: Position) extends BaseToken:
    def kind: TokenKind           = IDKIND
    override def toString: String = s"ID($value)${position.location}"

// integer literals
case class INTLIT(value: Int, position: Position) extends BaseToken:
    def kind: TokenKind           = INTLITKIND
    override def toString: String = s"INT($value)${position.location}"

// string literals
case class STRLIT(
    value: String,
    position: Position,
) extends BaseToken:
    def kind: TokenKind           = STRLITKIND
    override def toString: String = s"STR($value)${position.location}"

// Others
case class Token(kind: TokenKind, position: Position) extends BaseToken:
    override def toString: String = s"$kind${position.location}"
