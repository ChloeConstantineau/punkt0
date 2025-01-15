package io.punkt0.lexer

import io.punkt0.Position

enum TokenKind:
    case STRLITKIND, INTLITKIND, IDKIND, BAD, EOF, COLON, SEMICOLON, DOT, COMMA, EQSIGN, EQUALS, BANG, LPAREN,
      RPAREN, LBRACE, RBRACE, AND, OR, LESSTHAN, PLUS, MINUS, TIMES, DIV, OBJECT, CLASS, DEF, OVERRIDE, VAR,
      UNIT, STRING, EXTENDS, INT, BOOLEAN, WHILE, IF, ELSE, TRUE, FALSE, THIS, NULL, NEW, PRINTLN

sealed trait BaseToken:
    def kind: TokenKind

// identifiers
case class ID(value: String, position: Position) extends BaseToken:
    def kind: TokenKind           = TokenKind.IDKIND
    override def toString: String = s"ID($value)${position.location}"

// integer literals
case class INTLIT(value: Int, position: Position) extends BaseToken:
    def kind: TokenKind           = TokenKind.INTLITKIND
    override def toString: String = s"INT($value)${position.location}"

// string literals
case class STRLIT(
    value: String,
    position: Position,
) extends BaseToken:
    def kind: TokenKind           = TokenKind.STRLITKIND
    override def toString: String = s"STR($value)${position.location}"

// Others
case class Token(kind: TokenKind, position: Position) extends BaseToken:
    override def toString: String = s"$kind${position.location}"
