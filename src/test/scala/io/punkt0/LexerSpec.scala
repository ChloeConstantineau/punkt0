package io.punkt0

import io.punkt0.lexer.*
import io.punkt0.lexer.TokenKind.*

class LexerSpec extends UnitTest:

    private def parse(value: String*): List[TokenT] =
      Lexer.parseLines(Iterator(value.map(_.toList).toList.flatten))

    "Lexer" should:
        "tokenize an ID" in:
            parse("my_long_id_987").head shouldBe ID("my_long_id_987", Coordinates(1, 1))

        "tokenize an INTLIT" in:
            parse("123456").head shouldBe INTLIT(123456, Coordinates(1, 1))

        "tokenize a STRINGLIT with content" in:
            parse("\"my_string\"").head shouldBe STRLIT("my_string", Coordinates(1, 1))

        "tokenize an empty STRINGLIT" in:
            parse("\"\"").head shouldBe STRLIT("", Coordinates(1, 1))

        "tokenize keyword CLASS" in:
            parse("class").head shouldBe Token(CLASS, Coordinates(1, 1))

        "tokenize keyword OBJECT" in:
            parse("object").head shouldBe Token(OBJECT, Coordinates(1, 1))

        "tokenize keyword DEF" in:
            parse("def").head shouldBe Token(DEF, Coordinates(1, 1))

        "tokenize keyword OVERRIDE" in:
            parse("override").head shouldBe Token(OVERRIDE, Coordinates(1, 1))

        "tokenize keyword VAR" in:
            parse("var").head shouldBe Token(VAR, Coordinates(1, 1))

        "tokenize keyword BOOLEAN" in:
            parse("Boolean").head shouldBe Token(BOOLEAN, Coordinates(1, 1))

        "tokenize keyword INT" in:
            parse("Int").head shouldBe Token(INT, Coordinates(1, 1))

        "tokenize keyword STRING" in:
            parse("String").head shouldBe Token(STRING, Coordinates(1, 1))

        "tokenize keyword UNIT" in:
            parse("Unit").head shouldBe Token(UNIT, Coordinates(1, 1))

        "tokenize keyword EXTENDS" in:
            parse("extends").head shouldBe Token(EXTENDS, Coordinates(1, 1))

        "tokenize keyword WHILE" in:
            parse("while").head shouldBe Token(WHILE, Coordinates(1, 1))

        "tokenize keyword IF" in:
            parse("if").head shouldBe Token(IF, Coordinates(1, 1))

        "tokenize keyword ELSE" in:
            parse("else").head shouldBe Token(ELSE, Coordinates(1, 1))

        "tokenize keyword TRUE" in:
            parse("true").head shouldBe Token(TRUE, Coordinates(1, 1))

        "tokenize keyword FALSE" in:
            parse("false").head shouldBe Token(FALSE, Coordinates(1, 1))

        "tokenize keyword THIS" in:
            parse("this").head shouldBe Token(THIS, Coordinates(1, 1))

        "tokenize keyword NULL" in:
            parse("null").head shouldBe Token(NULL, Coordinates(1, 1))

        "tokenize keyword NEW" in:
            parse("new").head shouldBe Token(NEW, Coordinates(1, 1))

        "tokenize keyword PRINTLN" in:
            parse("println").head shouldBe Token(PRINTLN, Coordinates(1, 1))

        "tokenize COLON" in:
            parse(":").head shouldBe Token(COLON, Coordinates(1, 1))

        "tokenize SEMICOLON" in:
            parse(";").head shouldBe Token(SEMICOLON, Coordinates(1, 1))

        "tokenize DOT" in:
            parse(".").head shouldBe Token(DOT, Coordinates(1, 1))

        "tokenize COMMA" in:
            parse(",").head shouldBe Token(COMMA, Coordinates(1, 1))

        "tokenize EQSIGN" in:
            parse("=").head shouldBe Token(EQSIGN, Coordinates(1, 1))

        "tokenize EQUALS" in:
            parse("==").head shouldBe Token(EQUALS, Coordinates(1, 1))

        "tokenize BANG" in:
            parse("!").head shouldBe Token(BANG, Coordinates(1, 1))

        "tokenize LPAREN" in:
            parse("(").head shouldBe Token(LPAREN, Coordinates(1, 1))

        "tokenize RPAREN" in:
            parse(")").head shouldBe Token(RPAREN, Coordinates(1, 1))

        "tokenize LBRACE" in:
            parse("{").head shouldBe Token(LBRACE, Coordinates(1, 1))

        "tokenize RBRACE" in:
            parse("}").head shouldBe Token(RBRACE, Coordinates(1, 1))

        "tokenize AND" in:
            parse("&&").head shouldBe Token(AND, Coordinates(1, 1))

        "tokenize OR" in:
            parse("||").head shouldBe Token(OR, Coordinates(1, 1))

        "tokenize LESSTHAN" in:
            parse("<").head shouldBe Token(LESSTHAN, Coordinates(1, 1))

        "tokenize PLUS" in:
            parse("+").head shouldBe Token(PLUS, Coordinates(1, 1))

        "tokenize MINUS" in:
            parse("-").head shouldBe Token(MINUS, Coordinates(1, 1))

        "tokenize TIMES" in:
            parse("*").head shouldBe Token(TIMES, Coordinates(1, 1))

        "tokenize DIV" in:
            parse("/").head shouldBe Token(DIV, Coordinates(1, 1))

        "tokenize EOF" in:
            parse("").head shouldBe Token(EOF, Coordinates(1, 1))

        "be case sensitive in keywords" in:
            val res = parse("unit Unit")

            assert(
              res.head == ID("unit", Coordinates(1, 1)) &&
                res(1) == Token(UNIT, Coordinates(1, 6)),
            )

        "support case sensitivity, numbers and underscores in identifiers" in:
            val res = parse("my_id my_ID myiD_9")

            assert(
              res.head == ID("my_id", Coordinates(1, 1)) &&
                res(1) == ID("my_ID", Coordinates(1, 7)) &&
                res(2) == ID("myiD_9", Coordinates(1, 13)),
            )

        "ignore comments" in:
            val res = parse("my_id // my comment")

            assert(
              res.head == ID("my_id", Coordinates(1, 1)) &&
                res.last == Token(EOF, Coordinates(1, 20)),
            )

        "reject integers with trailing zeros" in:
            val res = parse("00 01 0")

            assert(
              res.head == Token(BAD, Coordinates(1, 1)) &&
                res(1) == Token(BAD, Coordinates(1, 4)) &&
                res(2) == INTLIT(0, Coordinates(1, 7)),
            )

        "recognize BAD tokens" in:
            val res = parse("# $ % ~ ` @")

            assert(
              res.head == Token(BAD, Coordinates(1, 1)) &&
                res(1) == Token(BAD, Coordinates(1, 3)) &&
                res(2) == Token(BAD, Coordinates(1, 5)) &&
                res(3) == Token(BAD, Coordinates(1, 7)) &&
                res(4) == Token(BAD, Coordinates(1, 9)) &&
                res(5) == Token(BAD, Coordinates(1, 11)),
            )
