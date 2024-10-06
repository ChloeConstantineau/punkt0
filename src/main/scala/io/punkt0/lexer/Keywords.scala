package io.punkt0.lexer

object Keywords {

  private val keywords = Map[String, TokenKind](
    "class" -> CLASS,
    "object" -> OBJECT,
    "def" -> DEF,
    "override" -> OVERRIDE,
    "var" -> VAR,
    "Boolean" -> BOOLEAN,
    "Int" -> INT,
    "String" -> STRING,
    "Unit" -> UNIT,
    "extends" -> EXTENDS,
    "while" -> WHILE,
    "if" -> IF,
    "else" -> ELSE,
    "true" -> TRUE,
    "false" -> FALSE,
    "this" -> THIS,
    "null" -> NULL,
    "new" -> NEW,
    "println" -> PRINTLN
  )

  def lookup(value: String): Option[TokenKind] = keywords.get(value)
}
