package io.punkt0

import io.punkt0.lexer._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike

import java.io.File

class LexerSpec extends AnyWordSpecLike {
  import io.punkt0.lexer.BaseToken

  private def parse(value: String*): List[BaseToken] = {
    val lines = Iterator(value.map(_.toList).toList.flatten)
    Lexer.parseLines(lines, List.empty, 1)
  }

  "Lexer" should {
    "tokenize an ID" in {
      parse("my_long_id_987").head shouldBe ID("my_long_id_987", Position(1, 1))
    }

    "tokenize an INTLIT" in {
      parse("123456").head shouldBe INTLIT(123456, Position(1, 1))
    }

    "tokenize an STRINGLIT" in {
      parse("\"my_string\"").head shouldBe STRLIT("my_string", Position(1, 1))
      parse("\"\"").head shouldBe STRLIT("", Position(1, 1))
    }

    "tokenize keyword CLASS" in {
      parse("class").head shouldBe Token(CLASS, Position(1, 1))
    }

    "tokenize keyword OBJECT" in {
      parse("object").head shouldBe Token(OBJECT, Position(1, 1))
    }

    "tokenize keyword DEF" in {
      parse("def").head shouldBe Token(DEF, Position(1, 1))
    }

    "tokenize keyword OVERRIDE" in {
      parse("override").head shouldBe Token(OVERRIDE, Position(1, 1))
    }

    "tokenize keyword VAR" in {
      parse("var").head shouldBe Token(VAR, Position(1, 1))
    }

    "tokenize keyword BOOLEAN" in {
      parse("Boolean").head shouldBe Token(BOOLEAN, Position(1, 1))
    }

    "tokenize keyword INT" in {
      parse("Int").head shouldBe Token(INT, Position(1, 1))
    }

    "tokenize keyword STRING" in {
      parse("String").head shouldBe Token(STRING, Position(1, 1))
    }

    "tokenize keyword UNIT" in {
      parse("Unit").head shouldBe Token(UNIT, Position(1, 1))
    }

    "tokenize keyword EXTENDS" in {
      parse("extends").head shouldBe Token(EXTENDS, Position(1, 1))
    }

    "tokenize keyword WHILE" in {
      parse("while").head shouldBe Token(WHILE, Position(1, 1))
    }

    "tokenize keyword IF" in {
      parse("if").head shouldBe Token(IF, Position(1, 1))
    }

    "tokenize keyword ELSE" in {
      parse("else").head shouldBe Token(ELSE, Position(1, 1))
    }

    "tokenize keyword TRUE" in {
      parse("true").head shouldBe Token(TRUE, Position(1, 1))
    }

    "tokenize keyword FALSE" in {
      parse("false").head shouldBe Token(FALSE, Position(1, 1))
    }

    "tokenize keyword THIS" in {
      parse("this").head shouldBe Token(THIS, Position(1, 1))
    }

    "tokenize keyword NULL" in {
      parse("null").head shouldBe Token(NULL, Position(1, 1))
    }

    "tokenize keyword NEW" in {
      parse("new").head shouldBe Token(NEW, Position(1, 1))
    }

    "tokenize keyword PRINTLN" in {
      parse("println").head shouldBe Token(PRINTLN, Position(1, 1))
    }

    "tokenize COLON" in {
      parse(":").head shouldBe Token(COLON, Position(1, 1))
    }

    "tokenize SEMICOLON" in {
      parse(";").head shouldBe Token(SEMICOLON, Position(1, 1))
    }

    "tokenize DOT" in {
      parse(".").head shouldBe Token(DOT, Position(1, 1))
    }

    "tokenize COMMA" in {
      parse(",").head shouldBe Token(COMMA, Position(1, 1))
    }

    "tokenize EQSIGN" in {
      parse("=").head shouldBe Token(EQSIGN, Position(1, 1))
    }

    "tokenize EQUALS" in {
      parse("==").head shouldBe Token(EQUALS, Position(1, 1))
    }

    "tokenize BANG" in {
      parse("!").head shouldBe Token(BANG, Position(1, 1))
    }

    "tokenize LPAREN" in {
      parse("(").head shouldBe Token(LPAREN, Position(1, 1))
    }

    "tokenize RPAREN" in {
      parse(")").head shouldBe Token(RPAREN, Position(1, 1))
    }

    "tokenize LBRACE" in {
      parse("{").head shouldBe Token(LBRACE, Position(1, 1))
    }

    "tokenize RBRACE" in {
      parse("}").head shouldBe Token(RBRACE, Position(1, 1))
    }

    "tokenize AND" in {
      parse("&&").head shouldBe Token(AND, Position(1, 1))
    }

    "tokenize OR" in {
      parse("||").head shouldBe Token(OR, Position(1, 1))
    }

    "tokenize LESSTHAN" in {
      parse("<").head shouldBe Token(LESSTHAN, Position(1, 1))
    }

    "tokenize PLUS" in {
      parse("+").head shouldBe Token(PLUS, Position(1, 1))
    }

    "tokenize MINUS" in {
      parse("-").head shouldBe Token(MINUS, Position(1, 1))
    }

    "tokenize TIMES" in {
      parse("*").head shouldBe Token(TIMES, Position(1, 1))
    }

    "tokenize DIV" in {
      parse("/").head shouldBe Token(DIV, Position(1, 1))
    }

    "tokenize EOF" in {
      parse("").head shouldBe Token(EOF, Position(1, 1))
    }

    "be case sensitive in keywords" in {
      val res = parse("unit Unit")

      res.head shouldBe ID("unit", Position(1, 1))
      res(1) shouldBe Token(UNIT, Position(1, 6))
    }

    "support case sensitivity, numbers and underscores in identifiers" in {
      val res = parse("my_id my_ID myiD_9")

      res.head shouldBe ID("my_id", Position(1, 1))
      res(1) shouldBe ID("my_ID", Position(1, 7))
      res(2) shouldBe ID("myiD_9", Position(1, 13))
    }

    "ignore comments" in {
      val res = parse("my_id // my comment")

      res.head shouldBe ID("my_id", Position(1, 1))
      res(1) shouldBe Token(EOF, Position(1, 20))
    }

    "reject integers with trailing zeros" in {
      val res = parse("00 01 0")

      res.head shouldBe Token(BAD, Position(1, 1))
      res(1) shouldBe Token(BAD, Position(1, 4))
      res(2) shouldBe INTLIT(0, Position(1, 7))
    }

    "recognize BAD tokens" in {
      val res = parse("# $ % ~ ` @")

      res.head shouldBe Token(BAD, Position(1, 1))
      res(1) shouldBe Token(BAD, Position(1, 3))
      res(2) shouldBe Token(BAD, Position(1, 5))
      res(3) shouldBe Token(BAD, Position(1, 7))
      res(4) shouldBe Token(BAD, Position(1, 9))
      res(5) shouldBe Token(BAD, Position(1, 11))
    }

    "support many all token kinds on many lines" in {
      Lexer
        .run(new File("./testprograms/lab2/valid/all.p0"))(Context())
        .length shouldBe 50
    }
  }
}