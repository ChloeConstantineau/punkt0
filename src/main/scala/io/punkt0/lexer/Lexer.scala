package io.punkt0.lexer

import io.punkt0.{Context, Phase}
import io.punkt0.Reporter._
import io.punkt0.Positioned

import java.io.File

object Lexer extends Phase[File, Iterator[Token]] {

  val keywords = Map[String, TokenKind](
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

  // indicates end of input
  val EndOfFile: Char = java.lang.Character.MAX_VALUE

  def run(f: File)(ctx: Context): Iterator[Token] = {

    val source = scala.io.Source.fromFile(f)

    // the last char seen in the input stream
    var currentChar = '\u0000'
    // used to detect \r\n pairs and ouput only \n for them
    var previousChar = '\u0000'

    val buffer = new StringBuffer

    def readChar(): Char = {
      if (source.hasNext) source.next()
      else EndOfFile
    }

    def currentPos(): Positioned = {
      (new Positioned {}).setPos(f, source.pos)
    }

    /* puts the next character in the input stream in currentChar, or the
     * special character EndOfFile if the stream is exhausted */
    def nextChar(): Unit = {
      if (currentChar == EndOfFile) return

      currentChar = readChar()
      previousChar =
        if (previousChar == '\r' && currentChar == '\n') readChar()
        else currentChar
      currentChar = if (previousChar == '\r') '\n' else previousChar
    }

    /* removes comments and whitespace and reads next token */
    def nextToken(): Token = {
      while (currentChar == '/' || Character.isWhitespace(currentChar)) {
        if (currentChar == '/') {
          val tokenPos: Positioned = currentPos()
          nextChar()

          if (currentChar == '/') {
            // skip comment
            while (currentChar != '\n' && currentChar != EndOfFile) nextChar()
          } else if (currentChar == '*') {
            // skip block comment
            var atEnd = false
            while (!atEnd) {
              while (currentChar != '*') {
                if (currentChar == EndOfFile)
                  fatal("unclosed block comment", currentPos())
                nextChar()
              }
              nextChar()
              if (currentChar == '/') {
                atEnd = true
                nextChar()
              }
            }
          } else {
            return (new Token(DIV)).setPos(tokenPos)
          }
        } else {
          // skip whitespace
          nextChar()
        }
      }

      readToken()
    }

    /* read next token from stream */
    def readToken(): Token = {
      val tokenPos = currentPos()

      currentChar match {
        case EndOfFile => new Token(EOF).setPos(tokenPos)

        case _ if Character.isLetter(currentChar) =>
          buffer.setLength(0)
          do {
            buffer.append(currentChar)
            nextChar()
          } while (Character.isLetterOrDigit(currentChar) || currentChar == '_')
          val str = buffer.toString
          keywords.get(str) match {
            case Some(tokenInfo) =>
              new Token(tokenInfo).setPos(tokenPos)
            case None =>
              new ID(str).setPos(tokenPos)
          }

        case '0' =>
          nextChar()
          new INTLIT(0).setPos(tokenPos)

        case _ if Character.isDigit(currentChar) =>
          buffer.setLength(0)
          do {
            buffer.append(currentChar)
            nextChar()
          } while (Character.isDigit(currentChar))
          val num = scala.math.BigInt(buffer.toString)
          if (!num.isValidInt) {
            error("value out of integer range", tokenPos)
            new Token(BAD).setPos(tokenPos)
          } else {
            new INTLIT(num.intValue).setPos(tokenPos)
          }

        case '"' =>
          buffer.setLength(0)
          nextChar()
          while (currentChar != '"') {
            if (currentChar == '\n' || currentChar == EndOfFile)
              fatal("unclosed string literal", tokenPos)
            buffer.append(currentChar)
            nextChar()
          }
          nextChar()
          new STRLIT(buffer.toString).setPos(tokenPos)

        case ':' => nextChar(); new Token(COLON).setPos(tokenPos)
        case ';' => nextChar(); new Token(SEMICOLON).setPos(tokenPos)
        case '.' => nextChar(); new Token(DOT).setPos(tokenPos)
        case ',' => nextChar(); new Token(COMMA).setPos(tokenPos)
        case '!' => nextChar(); new Token(BANG).setPos(tokenPos)
        case '(' => nextChar(); new Token(LPAREN).setPos(tokenPos)
        case ')' => nextChar(); new Token(RPAREN).setPos(tokenPos)
        case '{' => nextChar(); new Token(LBRACE).setPos(tokenPos)
        case '}' => nextChar(); new Token(RBRACE).setPos(tokenPos)
        case '<' => nextChar(); new Token(LESSTHAN).setPos(tokenPos)
        case '+' => nextChar(); new Token(PLUS).setPos(tokenPos)
        case '-' => nextChar(); new Token(MINUS).setPos(tokenPos)
        case '*' => nextChar(); new Token(TIMES).setPos(tokenPos)
        case '=' =>
          nextChar()
          if (currentChar == '=') {
            nextChar()
            new Token(EQUALS).setPos(tokenPos)
          } else {
            new Token(EQSIGN).setPos(tokenPos)
          }
        case '&' =>
          nextChar()
          if (currentChar == '&') {
            nextChar()
            new Token(AND).setPos(tokenPos)
          } else {
            error("single '&'", tokenPos)
            new Token(BAD).setPos(tokenPos)
          }
        case '|' =>
          nextChar()
          if (currentChar == '|') {
            nextChar()
            new Token(OR).setPos(tokenPos)
          } else {
            error("single '|'", tokenPos)
            new Token(BAD).setPos(tokenPos)
          }
        case _ =>
          error("invalid character: " + currentChar, currentPos())
          nextChar()
          new Token(BAD).setPos(tokenPos)
      }
    }

    nextChar()

    new Iterator[Token] {
      var tokenCache = nextToken()
      var atEnd = false

      def hasNext = {
        tokenCache.kind != EOF || !atEnd
      }

      def next() = {
        val r = tokenCache
        tokenCache = nextToken()
        if (r.kind == EOF) {
          atEnd = true
        }
        r
      }
    }

  }
}
