package io.punkt0.ast

import io.punkt0.ast.Trees._
import io.punkt0.lexer._
import io.punkt0.{Context, Phase}

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

object Parser extends Phase[Iterator[Token], Program] {

  def run(tokens: Iterator[Token])(ctx: Context): Program = {
    import io.punkt0.Position

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = Token(BAD, Position(1, 1))

    def readToken(): Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next()

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next()
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken()
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      println(
        "expected: " + (kind :: more.toList)
          .mkString(" or ") + ", found: " + currentToken
      )
      sys.exit(1)
    }

    /** Parse many of something, e.g. regex (VarDecl)* corresponds to many[VarDecl](Set(VAR), varDeclaration) * */
    def many[T](kinds: Set[TokenKind], next: () => T): List[T] = Iterator
      .continually(
        if (kinds.contains(currentToken.kind)) Some(next()) else None
      )
      .takeWhile(_.isDefined)
      .map(_.get)
      .toList
    // Enables many[VarDecl](VAR, varDeclaration)
    implicit def kindToSingletonSet(kind: TokenKind): Set[TokenKind] = Set(kind)

    def parseGoal: Program = {
      val classes: List[ClassDecl] = many(CLASS, classDeclaration)
      val main: MainDecl = mainDecl
      eat(EOF)
      Program(main, classes)
    }

    /** ClassDeclaration ::= class Identifier ( extends Identifier )? { ( VarDeclaration )* ( MethodDeclaration )* }
      */
    def classDeclaration(): ClassDecl = {
      eat(CLASS)
      val id = identifier
      val parent = if (currentToken.kind == EXTENDS) {
        eat(EXTENDS); Some(identifier)
      } else None
      eat(LBRACE)
      val vars = many(VAR, varDeclaration)
      val meths = many(Set(DEF, OVERRIDE), methodDeclaration)
      eat(RBRACE)
      ClassDecl(id, parent, vars, meths)
    }

    /** object Identifier extends { ( VarDeclaration )* Expression (; Expression )* }
      */
    def mainDecl: MainDecl = {
      eat(OBJECT)
      val objId = identifier
      eat(EXTENDS)
      val parent = identifier
      eat(LBRACE)
      val vars = many(VAR, varDeclaration)
      val exprs = exprList(SEMICOLON)
      eat(RBRACE)
      MainDecl(objId, parent, vars, exprs)
    }

    /** VarDeclaration ::= var Identifier : Type = Expression ;
      */
    def varDeclaration(): VarDecl = {
      eat(VAR)
      val id = identifier
      eat(COLON)
      val tp = parseType()
      eat(EQSIGN)
      val expr = expression()
      eat(SEMICOLON)
      VarDecl(tp, id, expr)
    }

    /**  (override) ? def Identifier
      *  ( ( Identifier : Type ( , Identifier : Type )* )? ) : Type =
      *  { ( VarDeclaration )* Expression ( ; Expression )* }
      */
    def methodDeclaration(): MethodDecl = {
      val overrides = currentToken.kind == OVERRIDE
      if (overrides) readToken()
      // Header
      eat(DEF); val name = identifier; eat(LPAREN)
      val args = argList()
      eat(RPAREN); eat(COLON)
      val retType = parseType()
      eat(EQSIGN); eat(LBRACE)
      // Body
      val vars = many(VAR, varDeclaration)
      val exprs = exprList(SEMICOLON)
      eat(RBRACE)
      val retExpr = exprs.last
      MethodDecl(
        overrides,
        retType,
        name,
        args,
        vars,
        exprs.dropRight(1),
        retExpr
      )
    }

    def parseType(): TypeTree = {
      val tpe = currentToken.kind match {
        case BOOLEAN => readToken(); BooleanType()
        case INT     => readToken(); IntType()
        case STRING  => readToken(); StringType()
        case UNIT    => readToken(); UnitType()
        case IDKIND  => identifier
        case _       => expected(BOOLEAN, INT, STRING, UNIT, IDKIND)
      }
      tpe
    }

    def expression(): ExprTree = {
      var e = orOperand()
      while (currentToken.kind == OR) {
        readToken()
        e = Or(e, orOperand())
      }
      e
    }

    def orOperand(): ExprTree = {
      var e = andOperand()
      while (currentToken.kind == AND) {
        readToken()
        e = And(e, andOperand())
      }
      e
    }

    def andOperand(): ExprTree = {
      var e = comparatorOperand()
      while (currentToken.kind == LESSTHAN || currentToken.kind == EQUALS) {
        val opKind = currentToken.kind
        readToken()
        e =
          if (opKind == LESSTHAN) LessThan(e, comparatorOperand())
          else Equals(e, comparatorOperand())
      }
      e
    }

    def comparatorOperand(): ExprTree = {
      var e = term()
      while (currentToken.kind == PLUS || currentToken.kind == MINUS) {
        val opKind = currentToken.kind
        readToken()
        e = if (opKind == PLUS) Plus(e, term()) else Minus(e, term())
      }
      e
    }

    def term(): ExprTree = {
      var e = factor()
      while (currentToken.kind == TIMES || currentToken.kind == DIV) {
        val opKind = currentToken.kind
        readToken()
        e = if (opKind == TIMES) Times(e, factor()) else Div(e, factor())
      }
      e
    }

    def factor(): ExprTree = {
      val expr = innerFactor()
      expressionSuffix(expr)
    }

    def innerFactor(): ExprTree = {
      val tkn = currentToken
      val tree = currentToken.kind match {
        case INTLITKIND =>
          readToken(); IntLit(tkn.asInstanceOf[INTLIT].value)
        case STRLITKIND =>
          readToken(); StringLit(tkn.asInstanceOf[STRLIT].value)
        case TRUE   => readToken(); True()
        case FALSE  => readToken(); False()
        case IDKIND => assignment(identifier)
        case THIS   => readToken(); This()
        case NULL   => readToken(); Null()
        case NEW =>
          readToken(); val id = identifier; eat(LPAREN); eat(RPAREN); New(id)
        case BANG => readToken(); Not(factor())
        case LPAREN =>
          readToken(); val e = expression(); eat(RPAREN); e
        case LBRACE =>
          readToken()
          val exprs =
            if (currentToken.kind == RBRACE) List.empty else exprList(SEMICOLON)
          eat(RBRACE)
          Block(exprs)
        case IF =>
          eat(IF); eat(LPAREN)
          val expr = expression()
          eat(RPAREN)
          val thn = expression()
          val els = currentToken.kind match {
            case ELSE =>
              eat(ELSE); Some(expression())
            case _ => None
          }
          If(expr, thn, els)
        case WHILE =>
          eat(WHILE); eat(LPAREN)
          val cond = expression()
          eat(RPAREN)
          val body = expression()
          While(cond, body)
        case PRINTLN =>
          eat(PRINTLN); eat(LPAREN); val expr = expression(); eat(RPAREN)
          Println(expr)
        case _ =>
          expected(
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
      tree
    }

    def expressionSuffix(expr: ExprTree): ExprTree = {
      var e = expr
      while (currentToken.kind == DOT) {
        readToken()
        val id = identifier
        eat(LPAREN)
        val args = if (currentToken.kind == RPAREN) List() else exprList(COMMA)
        eat(RPAREN)
        e = MethodCall(e, id, args)
      }
      e
    }

    def identifier: Identifier = {
      val id = currentToken
      eat(IDKIND)
      Identifier(id.asInstanceOf[ID].value)
    }

    def assignment(id: Identifier): ExprTree = {
      val tree = currentToken.kind match {
        case EQSIGN =>
          eat(EQSIGN); val expr = expression(); Assign(id, expr)
        case _ => id
      }
      tree
    }

    def argList(): List[Formal] = {
      val args: ListBuffer[Formal] = ListBuffer()
      if (currentToken.kind == IDKIND) {
        val id = identifier; eat(COLON); val tp = parseType()
        args += Formal(tp, id)
        while (currentToken.kind == COMMA) {
          eat(COMMA)
          val id = identifier; eat(COLON); val tp = parseType()
          args += Formal(tp, id)
        }
      }
      args.toList
    }

    def exprList(separator: TokenKind): List[ExprTree] = {
      val exprs: ListBuffer[ExprTree] = ListBuffer()
      exprs += expression()
      while (currentToken.kind == separator) {
        eat(separator)
        exprs += expression()
      }
      exprs.toList
    }

    readToken()
    val tree = parseGoal
//    terminateIfErrors
    tree
  }
}
