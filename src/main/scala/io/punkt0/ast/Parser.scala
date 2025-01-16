package io.punkt0.ast

import io.punkt0.{Context, Coordinates, Phase}
import io.punkt0.ast.Trees.*
import io.punkt0.lexer.*
import io.punkt0.lexer.TokenKind.*

import scala.annotation.{nowarn, tailrec}
import scala.collection.BufferedIterator

@nowarn
object Parser extends Phase[Iterator[TokenT], Program]:

    private def doParse(it: BufferedIterator[TokenT]): Program =

        val errorMessage = (badToken: Option[TokenT], expected: List[TokenKind]) => s"""
            |found: ${badToken.getOrElse("?")}
            |expected: ${expected.mkString(" or ")}
            |""".stripMargin

        @tailrec
        def getOrSkip(token: TokenT): TokenT =
          if token.kind == BAD && it.hasNext then { it.next(); getOrSkip(it.head) }
          else token

        def probe(kind: TokenKind*): Either[Error, TokenT] =
          getOrSkip(it.head) match
              case token if kind.toSet.contains(token.kind) => Right(it.next())
              case badToken                                 => Left(new Error(errorMessage(Some(badToken), kind.toList)))

        /** ''Eats'' the expected token based on kinds, or terminates with an error. */
        def eat(
            expectedKind: TokenKind*,
        ): Coordinates =
          probe(expectedKind*).fold(e => throw e, t => t.coordinates)

        /** ''Tastes'' the possibly expected token based on kinds, `eats` it if it was within the
          * possibilities, and returns it.
          */
        def taste(possibleKind: TokenKind*): Option[TokenT] =
          probe(possibleKind*).fold(_ => None, t => Some(t))

        /** Parse many of something, e.g. regex (VarDecl)* corresponds to many[VarDecl](Set(VAR),
          * varDeclaration) *
          */
        def many[T](
            next: () => T,
            kinds: TokenKind*,
        ): List[T] =
            @tailrec
            def run(acc: List[T]): List[T] =
              if kinds.toSet.contains(it.head.kind) then run(acc :+ next())
              else acc

            run(List.empty)

        /** ClassDeclaration ::= class Identifier ( extends Identifier )? { ( VarDeclaration )* (
          * MethodDeclaration )* }
          */
        def classDeclaration(): ClassDecl =
            val xy     = eat(CLASS)
            val id     = identifier()
            val parent = taste(EXTENDS, LBRACE) match
                case Some(Token(EXTENDS, _)) =>
                  val parentClass = identifier()
                  eat(LBRACE)
                  Some(parentClass)
                case Some(Token(LBRACE, _))  => None
                case e                       => throw new Error(errorMessage(e, List(EXTENDS, LBRACE)))

            val vars  = many(varDeclaration, VAR).flatten
            val meths = many(
              methodDeclaration,
              DEF,
              OVERRIDE,
            ).flatten
            eat(RBRACE)
            ClassDecl(id, parent, vars, meths, xy)

        /** object Identifier extends { ( VarDeclaration )* Expression (; Expression )* }
          */
        def mainDecl(): MainDecl =
            val xy     = eat(OBJECT)
            val objId  = identifier()
            eat(EXTENDS)
            val parent = identifier()
            eat(LBRACE)
            val vars   = many(varDeclaration, VAR).flatten
            val exprs  = exprList(SEMICOLON)
            eat(RBRACE)
            MainDecl(objId, parent, vars, exprs, xy)

        /** VarDeclaration ::= var Identifier : Type = Expression ;
          */
        def varDeclaration(): Option[VarDecl] =
          taste(VAR) match
              case Some(Token(VAR, xy)) =>
                val id   = identifier()
                eat(COLON)
                val tp   = parseType()
                eat(EQSIGN)
                val expr = expression()
                eat(SEMICOLON)
                Some(VarDecl(tp, id, expr, xy))
              case _                    => None

        /** (override) ? def Identifier ( ( Identifier : Type ( , Identifier : Type )* )? ) : Type = { (
          * VarDeclaration )* Expression ( ; Expression )* }
          */
        def methodDeclaration(): Option[MethodDecl] =
          taste(OVERRIDE, DEF) match
              case Some(Token(kind, xy)) if kind == OVERRIDE || kind == DEF =>
                // Header
                val overrides = kind == OVERRIDE
                if overrides then eat(DEF)

                val name    = identifier()
                eat(LPAREN)
                val args    = argList()
                eat(RPAREN)
                eat(COLON)
                val retType = parseType()
                eat(EQSIGN)
                eat(LBRACE)

                // Body
                val vars  = many(varDeclaration, VAR).flatten
                val exprs = exprList(SEMICOLON)
                eat(RBRACE)
                Some(
                  MethodDecl(
                    overrides,
                    retType,
                    name,
                    args,
                    vars,
                    exprs.dropRight(1),
                    exprs.last,
                    xy,
                  ),
                )
              case _                                                        => None

        def parseType(): TypeTree =
            val expected = List(BOOLEAN, INT, STRING, UNIT, IDKIND)
            taste(expected*) match
                case Some(Token(BOOLEAN, xy)) => BooleanType(xy)
                case Some(Token(INT, xy))     => IntType(xy)
                case Some(Token(STRING, xy))  => StringType(xy)
                case Some(Token(UNIT, xy))    => UnitType(xy)
                case Some(ID(value, xy))      => Identifier(value, xy)
                case badToken                 => throw new Error(errorMessage(badToken, expected))

        def expression(): ExprTree =
            @tailrec
            def runExpr(e: ExprTree): ExprTree =
              taste(OR) match
                  case Some(Token(OR, xy)) => runExpr(Or(e, orOperand(), xy))
                  case _                   => e

            runExpr(orOperand())

        def orOperand(): ExprTree =
            @tailrec
            def runOr(e: ExprTree): ExprTree =
              taste(AND) match
                  case Some(Token(AND, xy)) => runOr(And(e, andOperand(), xy))
                  case _                    => e

            runOr(andOperand())

        def andOperand(): ExprTree =
            @tailrec
            def runAnd(lhs: ExprTree): ExprTree =
              taste(LESSTHAN, EQUALS) match
                  case Some(Token(LESSTHAN, xy)) =>
                    runAnd(LessThan(lhs, comparatorOperand(), xy))
                  case Some(Token(EQUALS, xy))   =>
                    runAnd(Equals(lhs, comparatorOperand(), xy))
                  case _                         => lhs
            runAnd(comparatorOperand())

        def comparatorOperand(): ExprTree =
            @tailrec
            def runComparator(lhs: ExprTree): ExprTree =
              taste(PLUS, MINUS) match
                  case Some(Token(PLUS, xy))  => runComparator(Plus(lhs, term(), xy))
                  case Some(Token(MINUS, xy)) => runComparator(Minus(lhs, term(), xy))
                  case _                      => lhs
            runComparator(term())

        def term(): ExprTree =
            @tailrec
            def runTerm(lhs: ExprTree): ExprTree =
              taste(TIMES, DIV) match
                  case Some(Token(TIMES, xy)) => runTerm(Times(lhs, factor(), xy))
                  case Some(Token(DIV, xy))   => runTerm(Div(lhs, factor(), xy))
                  case _                      => lhs
            runTerm(factor())

        def factor(): ExprTree =
          expressionSuffix(innerFactor())

        def innerFactor(): ExprTree =
            val expected = List(
              IDKIND,
              INTLITKIND,
              STRLITKIND,
              TRUE,
              FALSE,
              THIS,
              NULL,
              NEW,
              BANG,
              LPAREN,
              LBRACE,
              IF,
              WHILE,
              PRINTLN,
            )
            taste(expected*) match
                case Some(ID(value, xy))     => assignment(Identifier(value, xy))
                case Some(INTLIT(value, xy)) => IntLit(value, xy)
                case Some(STRLIT(value, xy)) => StringLit(value, xy)
                case Some(token)             =>
                  val xy = token.coordinates
                  token.kind match
                      case TRUE    => True(xy)
                      case FALSE   => False(xy)
                      case THIS    => This(xy)
                      case NULL    => Null(xy)
                      case NEW     =>
                        val id = identifier()
                        eat(LPAREN)
                        eat(RPAREN)
                        New(id, xy)
                      case BANG    => Not(factor(), xy)
                      case LPAREN  =>
                        val expr = expression()
                        eat(RPAREN)
                        expr
                      case LBRACE  =>
                        val exprs =
                          taste(RBRACE) match
                              case Some(Token(RBRACE, _)) => List.empty
                              case _                      =>
                                val exprs = exprList(SEMICOLON)
                                eat(RBRACE)
                                exprs
                        Block(exprs, xy)
                      case IF      =>
                        eat(LPAREN)
                        val cond = expression()
                        eat(RPAREN)
                        val thn  = expression()
                        val els  = it.head.kind match
                            case ELSE =>
                              eat(ELSE)
                              Some(expression())
                            case _    => None
                        If(cond, thn, els, xy)
                      case PRINTLN =>
                        eat(LPAREN)
                        val expr = expression()
                        eat(RPAREN)
                        Println(expr, xy)
                      case WHILE   =>
                        eat(LPAREN)
                        val cond = expression()
                        eat(RPAREN)
                        While(cond, expression(), xy)
                      case _       => throw new Error(errorMessage(Some(token), expected))
                case e                       => throw new Error(errorMessage(e, expected))

        @tailrec
        def expressionSuffix(
            expr: ExprTree,
        ): ExprTree =
          taste(DOT) match
              case Some(Token(DOT, xy)) =>
                val id   = identifier()
                eat(LPAREN)
                val args = taste(RPAREN) match
                    case Some(Token(RPAREN, _)) => List.empty
                    case _                      =>
                      val exprs = exprList(COMMA)
                      eat(RPAREN)
                      exprs
                expressionSuffix(MethodCall(expr, id, args, xy))
              case _                    => expr

        def identifier(): Identifier =
          taste(IDKIND) match
              case Some(ID(value, xy)) => Identifier(value, xy)
              case e                   => throw new Error(errorMessage(e, List(IDKIND)))

        def assignment(
            id: Identifier,
        ): ExprTree =
          taste(EQSIGN) match
              case Some(Token(EQSIGN, xy)) => Assign(id, expression(), xy)
              case _                       => id

        def argList(): List[Formal] =
            @tailrec
            def runArgsList(list: List[Formal]): List[Formal] =
              taste(COMMA) match
                  case Some(Token(COMMA, _)) =>
                    val id = identifier()
                    eat(COLON)
                    val tp = parseType()
                    runArgsList(list :+ Formal(tp, id, id.coordinates))
                  case _                     => list

            taste(IDKIND) match
                case Some(ID(value, xy)) =>
                  val id = Identifier(value, xy)
                  eat(COLON)
                  val tp = parseType()
                  Formal(tp, id, xy) :: runArgsList(List.empty)
                case _                   => List.empty

        def exprList(
            separator: TokenKind,
        ): List[ExprTree] =
            @tailrec
            def runExprList(list: List[ExprTree]): List[ExprTree] =
              taste(separator) match
                  case Some(_) => runExprList(list :+ expression())
                  case None    => list

            expression() +: runExprList(List.empty)

        val classes: List[ClassDecl] =
          many(classDeclaration, CLASS)
        val main: MainDecl           = mainDecl()
        eat(EOF)
        Program(main, classes, Coordinates(1, 1))

    def run(tokens: Iterator[TokenT])(ctx: Context): Program =
      if tokens.isEmpty then throw new Error("Nothing to parse - tokens is empty!")
      else doParse(tokens.buffered)
