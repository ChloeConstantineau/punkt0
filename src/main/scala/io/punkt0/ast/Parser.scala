package io.punkt0.ast

import io.punkt0.ast.Trees._
import io.punkt0.lexer._
import io.punkt0.{Context, Phase}

import scala.annotation.tailrec
import scala.language.implicitConversions

object Parser extends Phase[Iterator[BaseToken], Program] {

  def run(tokens: Iterator[BaseToken])(ctx: Context): Program = {

    def readToken(tokens: Iterator[BaseToken]): BaseToken = {
      val getOrSkip = (token: BaseToken) =>
        if (tokens.hasNext && token.kind == BAD) tokens.next() else token

      getOrSkip(tokens.next())
    }

    /** ''Eats'' the throwExpected token, or terminates with an error. */
    def eat(
        tokens: Iterator[BaseToken],
        expectedKind: TokenKind*
    ): BaseToken = {
      readToken(tokens) match {
        case token if expectedKind.toSet.contains(token.kind) => token
        case badToken                                         => throwExpected(badToken, expectedKind: _*)
      }
    }

    /** Complains that what was found was not throwExpected. The method accepts arbitrarily many arguments of type TokenKind */
    def throwExpected(current: BaseToken, expectedKind: TokenKind*): Nothing = {
      val message =
        s"""
             |expected: ${expectedKind.mkString(" or ")}
             |found: ${current.kind} at ${current.position.location}
             |""".stripMargin

      throw new Error(message)
    }

    /** Parse many of something, e.g. regex (VarDecl)* corresponds to many[VarDecl](Set(VAR), varDeclaration) * */
    def many[T](
        next: () => T,
        tokens: Iterator[BaseToken],
        kinds: TokenKind*
    ): List[T] = {
      val cond = kinds.toSet.contains(readToken(tokens).kind)
      if (cond)
        Iterator
          .continually(
            next()
          )
          .takeWhile(_ => cond)
          .toList
      else List.empty
    }

    /** ClassDeclaration ::= class Identifier ( extends Identifier )? { ( VarDeclaration )* ( MethodDeclaration )* }
      */
    def classDeclaration(it: Iterator[BaseToken]): ClassDecl = {
      val id = identifier(it)
      val currentToken = readToken(it)
      val parent = currentToken.kind match {
        case EXTENDS => Some(identifier(it))
        case LBRACE  => None
        case _       => throwExpected(currentToken, EXTENDS, LBRACE)
      }

      val vars = many(() => varDeclaration(it), it, VAR)
      val meths = many(
        () => methodDeclaration(it: Iterator[BaseToken]),
        it,
        DEF,
        OVERRIDE
      )
      ClassDecl(id, parent, vars, meths)
    }

    /** object Identifier extends { ( VarDeclaration )* Expression (; Expression )* }
      */
    def mainDecl(it: Iterator[BaseToken]): MainDecl = {
      eat(it, OBJECT)
      val objId = identifier(it)
      eat(it, EXTENDS)
      val parent = identifier(it)
      eat(it, LBRACE)
      val vars = many(() => varDeclaration(it), it, VAR)
      val exprs = exprList(it, SEMICOLON)
      eat(it, RBRACE)
      MainDecl(objId, parent, vars, exprs)
    }

    /** VarDeclaration ::= var Identifier : Type = Expression ;
      */
    def varDeclaration(it: Iterator[BaseToken]): VarDecl = {
      eat(it, VAR)
      val id = identifier(it)
      eat(it, COLON)
      val tp = parseType(it)
      eat(it, EQSIGN)
      val expr = expression(it)
      eat(it, SEMICOLON)
      VarDecl(tp, id, expr)
    }

    /** (override) ? def Identifier
      * ( ( Identifier : Type ( , Identifier : Type )* )? ) : Type =
      * { ( VarDeclaration )* Expression ( ; Expression )* }
      */
    def methodDeclaration(it: Iterator[BaseToken]): MethodDecl = {
      // Header
      val currentToken = eat(it, OVERRIDE, DEF)
      val overrides = currentToken.kind match {
        case OVERRIDE => true
        case DEF      => false
        case _        => throwExpected(currentToken, OVERRIDE, DEF)
      }
      val name = identifier(it)
      eat(it, LPAREN)
      val args = argList(it)
      eat(it, RPAREN)
      eat(it, COLON)
      val retType = parseType(it)
      eat(it, EQSIGN)
      eat(it, LBRACE)

      // Body
      val vars = many(() => varDeclaration(it), it, VAR)
      val exprs = exprList(it, SEMICOLON)
      eat(it, RBRACE)
      MethodDecl(
        overrides,
        retType,
        name,
        args,
        vars,
        exprs.dropRight(1),
        exprs.last
      )
    }

    def parseType(it: Iterator[BaseToken]): TypeTree = {
      val currentToken = readToken(it)
      currentToken.kind match {
        case BOOLEAN => BooleanType()
        case INT     => IntType()
        case STRING  => StringType()
        case UNIT    => UnitType()
        case IDKIND  => identifier(it)
        case _ =>
          throwExpected(currentToken, BOOLEAN, INT, STRING, UNIT, IDKIND)
      }
    }

    def expression(it: Iterator[BaseToken]): ExprTree = {
      @tailrec
      def runExpr(e: ExprTree): ExprTree =
        if (readToken(it).kind == OR) {
          runExpr(And(e, orOperand(it)))
        } else e

      runExpr(orOperand(it))
    }

    def orOperand(it: Iterator[BaseToken]): ExprTree = {
      @tailrec
      def runOr(e: ExprTree): ExprTree =
        if (readToken(it).kind == AND) {
          runOr(And(e, andOperand(it)))
        } else e

      runOr(andOperand(it))
    }

    def andOperand(it: Iterator[BaseToken]): ExprTree = {
      @tailrec
      def runAnd(lhs: ExprTree): ExprTree =
        readToken(it).kind match {
          case LESSTHAN => runAnd(LessThan(lhs, comparatorOperand(it)))
          case EQUALS   => runAnd(Equals(lhs, comparatorOperand(it)))
          case _        => lhs
        }
      runAnd(comparatorOperand(it))
    }

    def comparatorOperand(it: Iterator[BaseToken]): ExprTree = {
      @tailrec
      def runComparator(lhs: ExprTree): ExprTree =
        readToken(it).kind match {
          case PLUS  => runComparator(Plus(lhs, term(it)))
          case MINUS => runComparator(Minus(lhs, term(it)))
          case _     => lhs
        }
      runComparator(term(it))
    }

    def term(it: Iterator[BaseToken]): ExprTree = {
      @tailrec
      def runTerm(lhs: ExprTree): ExprTree =
        readToken(it).kind match {
          case TIMES => runTerm(Times(lhs, factor(it)))
          case DIV   => runTerm(Div(lhs, factor(it)))
          case _     => lhs
        }
      runTerm(factor(it))
    }

    def factor(it: Iterator[BaseToken]): ExprTree =
      expressionSuffix(it, innerFactor(it))

    def innerFactor(it: Iterator[BaseToken]): ExprTree = {
      val currentToken = readToken(it)
      currentToken match {
        case ID(_, _)         => assignment(it, identifier(it))
        case INTLIT(value, _) => IntLit(value)
        case STRLIT(value, _) => StringLit(value)
        case Token(kind, _) =>
          kind match {
            case TRUE  => True()
            case FALSE => False()
            case THIS  => This()
            case NULL  => Null()
            case NEW =>
              val id = identifier(it)
              eat(it, LPAREN)
              eat(it, RPAREN)
              New(id)
            case BANG => Not(factor(it))
            case LPAREN =>
              val expr = expression(it)
              eat(it, RPAREN)
              expr
            case LBRACE => //TODO this clearly wont work
              val exprs =
                if (readToken(it).kind == RBRACE) List.empty
                else exprList(it, SEMICOLON)
              eat(it, RBRACE)
              Block(exprs)
            case IF =>
              eat(it, LPAREN)
              val expr = expression(it)
              eat(it, RPAREN)
              val thn = expression(it)
              val els = readToken(it).kind match {
                case ELSE =>
                  eat(it, ELSE); Some(expression(it))
                case _ => None
              }
              If(expr, thn, els)
            case PRINTLN =>
              eat(it, PRINTLN)
              eat(it, LPAREN)
              val expr = expression(it)
              eat(it, RPAREN)
              Println(expr)
            case _ =>
              throwExpected(
                currentToken,
                INTLITKIND,
                STRLITKIND,
                TRUE,
                FALSE,
                IDKIND,
                THIS,
                NULL,
                NEW,
                BANG,
                LPAREN,
                LBRACE,
                IF,
                WHILE,
                PRINTLN
              )
          }
      }
    }

    def expressionSuffix(
        it: Iterator[BaseToken],
        expr: ExprTree
    ): ExprTree = {
      eat(it, DOT)
      val id = identifier(it)
      eat(it, LPAREN)
      val args = readToken(it).kind match {
        case RPAREN => List.empty
        case _      => exprList(it, COMMA)
      }
      eat(it, RPAREN)
      MethodCall(expr, id, args)
    }

    def identifier(it: Iterator[BaseToken]): Identifier = {
      readToken(it) match {
        case ID(value, _) => Identifier(value)
        case badToken     => throwExpected(badToken, IDKIND)
      }
    }

    def assignment(it: Iterator[BaseToken], id: Identifier): ExprTree = {
      readToken(it) match {
        case Token(kind, _) if kind == EQSIGN => Assign(id, expression(it))
        case _                                => id
      }
    }

    def argList(it: Iterator[BaseToken]): List[Formal] = {
      @tailrec
      def runArgsList(list: List[Formal]): List[Formal] = {
        if (readToken(it).kind == COMMA) {
          val id = identifier(it)
          eat(it, COLON)
          val tp = parseType(it)
          runArgsList(list :+ Formal(tp, id))
        } else list
      }

      readToken(it).kind match {
        case IDKIND =>
          val id = identifier(it)
          eat(it, COLON)
          val tp = parseType(it)
          Formal(tp, id) :: runArgsList(List.empty)
        case _ => List.empty
      }
    }

    def exprList(
        it: Iterator[BaseToken],
        separator: TokenKind
    ): List[ExprTree] = {
      @tailrec
      def runExprList(list: List[ExprTree]): List[ExprTree] =
        if (readToken(it).kind == separator)
          runExprList(list :+ expression(it))
        else list

      expression(it) +: runExprList(List.empty)
    }

    if (tokens.isEmpty)
      throw new Error("Nothing to parse - tokens is empty!")
    else {
      val classes: List[ClassDecl] =
        many(() => classDeclaration(tokens), tokens, CLASS)
      val main: MainDecl = mainDecl(tokens)
      eat(tokens, EOF)
      Program(main, classes)
    }

  }
}
