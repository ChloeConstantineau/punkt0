package io.punkt0.lexer

import io.punkt0.{Coordinates, Positioned}

enum TokenKind:
    case STRLITKIND, INTLITKIND, IDKIND, BAD, EOF, COLON, SEMICOLON, DOT, COMMA, EQSIGN, EQUALS, BANG, LPAREN,
      RPAREN, LBRACE, RBRACE, AND, OR, LESSTHAN, PLUS, MINUS, TIMES, DIV, OBJECT, CLASS, DEF, OVERRIDE, VAR,
      UNIT, STRING, EXTENDS, INT, BOOLEAN, WHILE, IF, ELSE, TRUE, FALSE, THIS, NULL, NEW, PRINTLN

sealed trait TokenT extends Positioned:
    def kind: TokenKind

case class ID(value: String, coordinates: Coordinates) extends TokenT:
    override def kind: TokenKind  = TokenKind.IDKIND
    override def toString: String = s"ID($value)${this.location}"

case class INTLIT(value: Int, coordinates: Coordinates) extends TokenT:
    override def kind: TokenKind  = TokenKind.INTLITKIND
    override def toString: String = s"INT($value)${this.location}"

case class STRLIT(
    value: String,
    coordinates: Coordinates,
) extends TokenT:
    override def kind: TokenKind  = TokenKind.STRLITKIND
    override def toString: String = s"STR($value)${this.location}"

case class Token(kind: TokenKind, coordinates: Coordinates) extends TokenT:
    override def toString: String = s"$kind${this.location}"
