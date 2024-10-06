//package io.punkt0.lexer
//
//import io.punkt0.{Context, Phase, Position}
//
//import java.io.File
//import scala.annotation.tailrec
//import scala.io.Source
//import scala.util.matching.Regex
//
//object Lexer extends Phase[File, Iterator[Token]] {
//
//  private val alphaR: Regex = "[a-zA-Z]".r
//  private val numericalR: Regex = "[0-9]".r
//  private val identifierR: Regex = "[a-zA-Z0-9_]".r
//
//  def run(f: File)(ctx: Context): Iterator[Token] = {
//    val source = Source.fromFile(f)
//    val lines = source.getLines()
//
//    def matchChar(char: Char): TokenKind =
//      char match {
//        case ':' => COLON
//        case ';' => SEMICOLON
//        case '.' => DOT
//        case ',' => COMMA
//        case '!' => BANG
//        case '(' => LPAREN
//        case ')' => RPAREN
//        case '{' => LBRACE
//        case '}' => RBRACE
//        case '<' => LESSTHAN
//        case '+' => PLUS
//        case '-' => MINUS
//        case '*' => TIMES
//        case '/' => DIV
//        case _   => BAD
//      }
//
//    def parseLines(
//        lines: Iterator[Iterator[Char]],
//        tokenIt: Iterator[Token],
//        topCursor: Position
//    ): Iterator[Token] = {
//
//      @tailrec
//      def parseLine(
//          line: Iterator[Char],
//          cursor: Position,
//          tokens: Iterator[Token]
//      ): Iterator[Token] = {
//        line match {
//          case ::((char, column), tail) =>
//            val nextChar = tail.headOption.map(_._1)
//            char match {
//              case alphaR(_*) =>
//                val matched = tail.takeWhile { case (ch, _) =>
//                  ch.toString.matches(identifierR.regex)
//                }
//                val length = matched.length
//                val id = s"$char${matched.map(_._1).mkString}"
//                val tailNext = tail.drop(length)
//                val nextCursor =
//                  if (tailNext.isEmpty) new Position(cursor.line + 1, 0)
//                  else cursor.copy(column = cursor.column + column)
//                val token = Keywords.lookup(id) match {
//                  case Some(keyword) => Token(keyword, cursor)
//                  case None =>
//                    new ID(
//                      id,
//                      cursor
//                    )
//                }
//                parseLine(
//                  tailNext,
//                  nextCursor,
//                  tokens ++ Iterator(token)
//                )
//              case numericalR(_*) =>
//                val matched = tail
//                  .takeWhile { case (ch, _) =>
//                    ch.toString.matches(numericalR.regex)
//                  }
//                val number = s"$char${matched.map(_._1).mkString}".toInt
//                val tailNext = tail.drop(matched.length)
//                val nextCursor =
//                  if (tailNext.isEmpty) new Position(cursor.line + 1, 0)
//                  else cursor.copy(column = cursor.column + column)
//                parseLine(
//                  tailNext,
//                  nextCursor,
//                  tokens ++ Iterator(
//                    new INTLIT(number, cursor)
//                  )
//                )
//              case '"' =>
//                if (!tail.map(_._1).contains('"')) {
//                  parseLine(
//                    tail,
//                    cursor.copy(column = cursor.column + 1),
//                    tokens ++ Iterator(
//                      Token(BAD, cursor)
//                    )
//                  )
//                } else {
//                  val matched = tail
//                    .takeWhile { case (ch, _) =>
//                      ch != '"'
//                    }
//                  val stringLiteral = s""""${matched.map(_._1).mkString}""""
//                  val tailNext = tail.drop(matched.length)
//                  val nextCursor =
//                    if (tailNext.isEmpty) new Position(cursor.line + 1, 0)
//                    else cursor.copy(column = cursor.column + column + 1)
//
//                  parseLine(
//                    tailNext,
//                    nextCursor,
//                    tokens ++ Iterator(
//                      new STRLIT(stringLiteral, cursor)
//                    )
//                  )
//                }
////              case '/'                => ???
//              case ' ' =>
//                val nextCursor =
//                  if (tail.isEmpty) new Position(cursor.line + 1, 0)
//                  else cursor.copy(column = column + 1)
//                parseLine(
//                  tail,
//                  nextCursor,
//                  tokens
//                )
//              case '&' =>
//                if (nextChar.contains('&'))
//                  parseLine(
//                    tail,
//                    cursor.copy(column = column + 2),
//                    tokens ++ Iterator(
//                      Token(
//                        AND,
//                        cursor
//                      )
//                    )
//                  )
//                else
//                  parseLine(
//                    tail,
//                    cursor.copy(column = column + 1),
//                    tokens ++ Iterator(
//                      Token(
//                        BAD,
//                        cursor
//                      )
//                    )
//                  )
//              case '|' =>
//                if (nextChar.contains('|'))
//                  parseLine(
//                    tail,
//                    cursor.copy(column = column + 2),
//                    tokens ++ Iterator(
//                      Token(
//                        OR,
//                        cursor
//                      )
//                    )
//                  )
//                else
//                  parseLine(
//                    tail,
//                    cursor.copy(column = column + 1),
//                    tokens ++ Iterator(
//                      Token(
//                        BAD,
//                        cursor
//                      )
//                    )
//                  )
//              case '=' =>
//                if (nextChar.contains('='))
//                  parseLine(
//                    tail,
//                    cursor.copy(column = column + 2),
//                    tokens ++ Iterator(
//                      Token(
//                        EQUALS,
//                        cursor
//                      )
//                    )
//                  )
//                else
//                  parseLine(
//                    tail,
//                    cursor.copy(column = column + 1),
//                    tokens ++ Iterator(
//                      Token(
//                        EQSIGN,
//                        cursor
//                      )
//                    )
//                  )
//              case _ =>
//                parseLine(
//                  tail,
//                  cursor.copy(column = column + 1),
//                  tokens ++ Iterator(
//                    Token(
//                      matchChar(char),
//                      cursor
//                    )
//                  )
//                )
//            }
//          case Nil =>
//            if (lines.isEmpty) tokens ++ Iterator(Token(EOF, cursor))
//            else
//              parseLine(
//                lines.next()._1,
//                new Position(cursor.line + 1, 0),
//                tokens
//              )
//        }
//      }
//
//      if (lines.hasNext) {
//        parseLine(
//          lines.next()._1,
//          new Position(lines.next()._2, 0),
//          tokenIt
//        ) ++ parseLines(
//          lines,
//          tokenIt
//        )
//      } else tokenIt
//
//    }
//
//    parseLines(
//      lines.toList.zipWithIndex
//        .map(l => (l._1.toList.zipWithIndex, l._2))
//        .iterator,
//      Iterator.empty
//    )
//  }
//}
