package io.punkt0.lexer

import io.punkt0.{Context, Phase, Position}

import java.io.File
import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Lexer extends Phase[File, Iterator[Token]] {

  private val alphaR: Regex = "[a-zA-Z]".r
  private val numericalR: Regex = "[0-9]".r
  private val identifierR: Regex = "[a-zA-Z0-9_]".r

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = Source.fromFile(f)
    val lines = source.getLines()

    @tailrec
    def parseLines(
        lines: Iterator[List[Char]],
        accTokens: List[Token],
        lineIndex: Int
    ): List[Token] = {

      @tailrec
      def parseLine(
          line: List[Char],
          tokens: List[Token],
          columnIndex: Int
      ): List[Token] = {
        val cursor = new Position(line = lineIndex, column = columnIndex)
        line match {
          case ::(head, tail) =>
            head match {
              case ' ' =>
                parseLine(
                  tail,
                  tokens,
                  columnIndex + 1
                )
              case '"' =>
                if (tail.contains('"')) {
                  val stringLit = tail.takeWhile(_ != '"')
                  parseLine(
                    tail.drop(stringLit.length + 1),
                    tokens ++ Iterator(new STRLIT(stringLit.mkString, cursor)),
                    columnIndex + stringLit.length + 2
                  )
                } else
                  parseLine(
                    tail,
                    tokens ++ Iterator(Token(BAD, cursor)),
                    columnIndex + 1
                  )
              case '&' | '|' | '=' =>
                val tokenKind = tail.headOption match {
                  case Some('&')        => AND
                  case Some('|')        => OR
                  case Some('=')        => EQUALS
                  case _ if head == '=' => EQSIGN
                  case _                => BAD
                }
                if (tokenKind == EQUALS || tokenKind == BAD)
                  parseLine(
                    tail,
                    tokens ++ Iterator(Token(tokenKind, cursor)),
                    columnIndex + 1
                  )
                else
                  parseLine(
                    tail.drop(1),
                    tokens ++ Iterator(Token(tokenKind, cursor)),
                    columnIndex + 2
                  )
              case alphaR(_*) =>
                val matched =
                  tail.takeWhile(_.toString.matches(identifierR.regex)).mkString
                val id = s"$head${matched}"
                val token = Keywords.lookup(id) match {
                  case Some(keyword) => Token(keyword, cursor)
                  case None =>
                    new ID(
                      id,
                      cursor
                    )
                }
                parseLine(
                  tail.drop(matched.length),
                  tokens ++ Iterator(token),
                  columnIndex + id.length
                )
              case numericalR(_*) =>
                val matched =
                  tail.takeWhile(_.toString.matches(numericalR.regex)).mkString
                val number = s"$head$matched"
                parseLine(
                  tail.drop(matched.length),
                  tokens ++ Iterator(new INTLIT(number.toInt, cursor)),
                  columnIndex + number.length
                )
              case '/' =>
                tail.headOption match {
                  case Some('/') =>
                    parseLine(Nil, tokens, columnIndex + tail.length)
                  case Some('*') => ???
                  case _ =>
                    parseLine(
                      tail,
                      tokens ++ Iterator(
                        Token(
                          DIV,
                          cursor
                        )
                      ),
                      columnIndex + 1
                    )
                }
              case _ =>
                val tokenKind = head match {
                  case ':' => COLON
                  case ';' => SEMICOLON
                  case '.' => DOT
                  case ',' => COMMA
                  case '!' => BANG
                  case '(' => LPAREN
                  case ')' => RPAREN
                  case '{' => LBRACE
                  case '}' => RBRACE
                  case '<' => LESSTHAN
                  case '+' => PLUS
                  case '-' => MINUS
                  case '*' => TIMES
                  case '/' => DIV
                  case _   => BAD
                }
                parseLine(
                  tail,
                  tokens ++ Iterator(
                    Token(
                      tokenKind,
                      cursor
                    )
                  ),
                  columnIndex + 1
                )
            }
          case Nil =>
            if (lines.isEmpty) tokens ++ Iterator(Token(EOF, cursor))
            else tokens
        }
      }

      if (lines.isEmpty) {
        accTokens
      } else {
        parseLines(lines, parseLine(lines.next(), accTokens, 1), lineIndex + 1)
      }
    }

    parseLines(lines.map(_.toList), List.empty, 1).iterator
  }
}
