package io.punkt0.lexer

import io.punkt0.{Context, Phase, Position}

import java.io.File
import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Lexer extends Phase[File, Iterator[BaseToken]] {

  private val alphaR: Regex = "[a-zA-Z]".r
  private val numericalR: Regex = "[0-9]".r
  private val identifierR: Regex = "[a-zA-Z0-9_]".r

  @tailrec
  def parseLines(
      lines: Iterator[List[Char]],
      accTokens: List[BaseToken],
      y: Int
  ): List[BaseToken] = {

    @tailrec
    def parseLine(line: List[Char], tokens: List[BaseToken], x: Int): List[BaseToken] = {
      val cursor = new Position(line = y, column = x)
      line match {
        case ::(head, tail) =>
          head match {
            case ' ' => parseLine(tail, tokens, x + 1)
            case '"' =>
              if (tail.contains('"')) {
                val stringLit = tail.takeWhile(_ != '"')
                parseLine(
                  tail.drop(stringLit.length + 1),
                  tokens :+ STRLIT(stringLit.mkString, cursor),
                  x + stringLit.length + 2
                )
              } else
                parseLine(tail, tokens :+ Token(BAD, cursor), x + 1)
            case '&' | '|' | '=' =>
              val tokenKind = tail.headOption match {
                case Some('&')        => AND
                case Some('|')        => OR
                case Some('=')        => EQUALS
                case _ if head == '=' => EQSIGN
                case _                => BAD
              }
              val token = Token(tokenKind, cursor)
              if (tokenKind == EQUALS || tokenKind == BAD)
                parseLine(tail, tokens :+ token, x + 1)
              else
                parseLine(tail.drop(1), tokens :+ token, x + 2)
            case alphaR(_*) =>
              val matched =
                tail.takeWhile(_.toString.matches(identifierR.regex)).mkString
              val id = s"$head$matched"
              val token = Keywords.lookup(id) match {
                case Some(keyword) => Token(keyword, cursor)
                case None          => new ID(id, cursor)
              }
              parseLine(
                tail.drop(matched.length),
                tokens :+ token,
                x + id.length
              )
            case numericalR(_*) =>
              val matched =
                tail.takeWhile(_.toString.matches(numericalR.regex)).mkString
              val number = s"$head$matched"
              val token =
                if (number.startsWith("0") && number.length > 1) Token(BAD, cursor)
                else INTLIT(number.toInt, cursor)
              parseLine(
                tail.drop(matched.length),
                tokens :+ token,
                x + number.length
              )
            case '/' =>
              tail.headOption match {
                case Some('/') => parseLine(Nil, tokens, x + tail.length + 1)
                case _         => parseLine(tail, tokens :+ Token(DIV, cursor), x + 1)
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
              parseLine(tail, tokens :+ Token(tokenKind, cursor), x + 1)
          }
        case Nil =>
          if (lines.isEmpty) tokens :+ Token(EOF, cursor)
          else tokens
      }
    }

    if (lines.isEmpty) {
      accTokens
    } else {
      parseLines(lines, parseLine(lines.next(), accTokens, 1), y + 1)
    }
  }

  def run(f: File)(ctx: Context): Iterator[BaseToken] = {
    val source = Source.fromFile(f)
    val lines = source.getLines()

    val result = parseLines(lines.map(_.toList), List.empty, 1).iterator
    source.close()
    result
  }
}
