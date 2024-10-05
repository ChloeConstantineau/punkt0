package io.punkt0.analyzer

import io.punkt0.{Context, Phase}
import io.punkt0.analyzer.Symbols._
import io.punkt0.analyzer.Types._
import io.punkt0.ast.Trees._

import scala.collection.mutable.ListBuffer

object TypeChecking extends Phase[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
    * attaching types to trees and potentially outputting error messages.
    */

  val primitiveTypes = List(TBoolean, TString, TInt)
  def run(prog: Program)(ctx: Context): Program = {

    def getType(tpe: TypeTree, classSymbol: Option[ClassSymbol]): Type =
      tpe match {
        case BooleanType() => TBoolean
        case StringType()  => TString
        case IntType()     => TInt
        case UnitType()    => TUnit
        case Identifier(value) => {
          if (classSymbol.isDefined)
            TClass(classSymbol.get)
          else {
            val c = prog.classes.find(c => c.id.equals(tpe))
            if (c.isDefined)
              TClass(c.get.getSymbol)
            else
              TUntyped
          }
        }
        case _ => TUntyped
      }

    def tcPlusOverload(e1: Type, e2: Type): Type = {
      e1 match {
        case TInt => {
          e2 match {
            case TInt    => TInt
            case TString => TString
            case any =>
              import io.punkt0.Reporter
              Reporter.fatal("Plus is not overloaded for type" + any)
          }
        }
        case TString => {
          e2 match {
            case TInt    => TString
            case TString => TString
            case any =>
              import io.punkt0.Reporter
              Reporter.fatal("Plus is not overloaded for type" + any)
          }
        }
        case any =>
          import io.punkt0.Reporter
          Reporter.fatal("Plus is not overloaded for type" + any)
      }
    }

    def tcComparisionOverload(e1: Type, e2: Type): Unit = {
      import io.punkt0.Reporter

      //Primitive type compare
      if (
        primitiveTypes.contains(e1) && primitiveTypes.contains(e2) && !e1
          .isSubTypeOf(e2)
      )
        Reporter.fatal("Cannot compare two different primitive types")
      else if (
        (primitiveTypes.contains(e1) && !primitiveTypes.contains(
          e2
        )) || (primitiveTypes.contains(e2) && !primitiveTypes.contains(e1))
      )
        Reporter.fatal("Cannot compare primitive and class type")

      //Both have class types and are subsets of each other
      else if (
        !primitiveTypes.contains(e1) && !primitiveTypes
          .contains(e2) && (!e1.isSubTypeOf(e2) || !e2.isSubTypeOf(e1))
      )
        Reporter.fatal("Error in comparison")
    }

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = getTpeExpr(expr)

      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        import io.punkt0.Reporter
        Reporter.error(
          "Type error: expected: " + expected.toList.mkString(
            " or "
          ) + ", found: " + tpe,
          expr
        )
        expected.head
      } else {
        tpe
      }
    }

    def tcMain(): Unit = {

      //Set type obj
      val mainSymbol = prog.main.getSymbol
      val tpeMain = getType(prog.main.obj, Some(mainSymbol))
      prog.main.setType(tpeMain)

      //Set type parent
      val mainParentSymbol = prog.main.parent.getSymbol
      val tpeParentMain = getType(
        prog.main.parent,
        Some(mainParentSymbol.asInstanceOf[ClassSymbol])
      )
      prog.main.parent.setType(tpeParentMain)

      tcVarDecl(prog.main.vars)
      val mainExprs = prog.main.exprs
      for (i <- 0 until mainExprs.length if mainExprs.length > 0) {
        val tpe = getTpeExpr(mainExprs(i))
        prog.main.exprs(i).setType(tpe)
      }

      //SetType Exprs
      setTpeExprs(prog.main.exprs)

    }
    def tcClasses(): Unit = {
      for (i <- 0 until prog.classes.length) {
        tcClass(prog.classes(i))

        if (prog.classes(i).parent.isDefined) {
          val tpeParentClass = getType(
            prog.classes(i).parent.get,
            Some(prog.classes(i).getSymbol.parent.get)
          )
          prog.classes(i).parent.get.setType(tpeParentClass)
        }
        val tpeClass =
          getType(prog.classes(i).id, Some(prog.classes(i).getSymbol))

        prog.classes(i).setType(tpeClass)
      }
    }

    def tcClass(classDecl: ClassDecl): Unit = {
      tcVarDecl(classDecl.vars)
      tcMethodDecl(classDecl.methods)
    }

    def tcVarDecl(vars: List[VarDecl]): Unit = {
      for (i <- 0 until vars.length if vars.length > 0) {
        val tpe = getType(vars(i).tpe, None)
        tcExpr(vars(i).expr, tpe)
        vars(i).setType(tpe)
        vars(i).id.setType(tpe)
        vars(i).expr.setType(tpe)
      }
    }

    def tcMethodDecl(methods: List[MethodDecl]): Unit = {
      for (i <- 0 until methods.length if methods.length > 0) {

        //SetType VarDecl
        tcVarDecl(methods(i).vars)

        //SetType Exprs
        setTpeExprs(methods(i).exprs)

        //Get Type RETURN TYPE
        val rTpe = getType(methods(i).retType, None)

        //Type check RETURN EXPR with return type
        val methodTpe = tcExpr(methods(i).retExpr, rTpe)

        methods(i).retExpr.setType(methodTpe)
        methods(i).setType(methodTpe)
        for (j <- 0 until methods(i).exprs.length) {
          var rTpe = getTpeExpr(methods(i).exprs(j))
          methods(i).exprs(j).setType(rTpe)
        }
      }
    }

    def setTpeExprs(exprs: List[ExprTree]): Unit = {
      exprs.foreach(x => {
        var exprTpe = getTpeExpr(x)
        x.setType(exprTpe)
      })
    }

    def getTpeExpr(expr: ExprTree): Type = expr match {

      case And(e1, e2) => {
        val rTpe = tcLogicalOperator(e1, e2)
        expr.setType(rTpe)
        rTpe
      }
      case Or(e1, e2) => {
        val rTpe = tcLogicalOperator(e1, e2)
        expr.setType(rTpe)
        rTpe
      }
      case Plus(e1, e2) => {
        val tpE1 = getTpeExpr(e1)
        val tpE2 = getTpeExpr(e2)
        val rTpe = tcPlusOverload(tpE1, tpE2)
        expr.setType(rTpe)
        rTpe
      }
      case Minus(e1, e2) => {
        val rTpe = tcMathOperator(e1, e2, TInt)
        expr.setType(rTpe)
        rTpe
      }
      case Times(e1, e2) => {
        val rTpe = tcMathOperator(e1, e2, TInt)
        expr.setType(rTpe)
        rTpe
      }
      case Div(e1, e2) => {
        val rTpe = tcMathOperator(e1, e2, TInt)
        expr.setType(rTpe)
        rTpe
      }
      case LessThan(e1, e2) => {
        val rTpe = tcMathOperator(e1, e2, TBoolean)
        expr.setType(rTpe)
        rTpe
      }
      case Equals(e1, e2) => {
        val tpE1 = getTpeExpr(e1)
        val tpE2 = getTpeExpr(e2)
        tcComparisionOverload(tpE1, tpE2)

        expr.setType(TBoolean)
        TBoolean
      }
      case MethodCall(obj, meth, args) => {
        //Get and attach type to calling object
        val tpeObj = getTpeExpr(obj)
        obj.setType(tpeObj)

        //Attach method symbol to method called
        //And verify the object class contains signature for method called
        val newMethodSymbol = attachMethodSymbol(obj, meth)
        meth.setSymbol(newMethodSymbol)

        //MethodCall type is equal to the retType of the method

        val rTpe = meth.getSymbol.asInstanceOf[MethodSymbol].tpe
        meth.setType(rTpe)

        //tcArguments of method call with method signature
        tcArgs(meth, args)

        expr.setType(rTpe)
        rTpe
      }
      case New(e) => {
        if (
          !prog.classes.find(c => c.id.value.equals(e.getSymbol.name)).isDefined
        ) {
          import io.punkt0.Reporter
          Reporter.error("Cannot instantiate object of this class", e)
        }

        val classSymbol = e.getSymbol.asInstanceOf[ClassSymbol]
        e.setType(TClass(classSymbol))
        expr.setType(TClass(classSymbol))

        TClass(classSymbol)
      }
      case Not(e) => {
        tcExpr(e, TBoolean)

        expr.setType(TBoolean)
        TBoolean
      }
      case Block(exprList) => {
        //Find and set type of Block to its last expression
        if (exprList.length.equals(0)) {
          expr.setType(TUnit)
          TUnit

        } else {
          val lastExpr = exprList.last

          val rTp = getTpeExpr(lastExpr)

          for (i <- 0 until (exprList.length)) {
            var tpeExpr = getTpeExpr(exprList(i))

          }
          expr.setType(rTp)
          rTp
        }
      }
      case If(cond, body, elseE) => {

        tcExpr(cond, TBoolean)
        cond.setType(TBoolean)

        if (!elseE.isDefined) {
          val bodyTpe = getTpeExpr(body)
          body.setType(bodyTpe)
          expr.setType(TUnit)
          TUnit

        } else {

          val rTpe = getTpeExpr(elseE.get)

          tcExpr(body, rTpe)
          body.setType(rTpe)
          elseE.get.setType(rTpe)
          expr.setType(rTpe)
          rTpe
        }
      }
      case While(cond, body) => {
        tcExpr(cond, TBoolean)
        getTpeExpr(body)

        body.setType(TUnit)
        expr.setType(TUnit)
        TUnit
      }
      case Println(e) => {
        tcExpr(e, TBoolean, TInt, TString)
        val tpe = getTpeExpr(e)
        e.setType(tpe)
        expr.setType(TUnit)
        TUnit
      }
      case Assign(id, e) => {
        val tpeIdentifier = getTpeBasicExpr(id)
        val tpeE = tcExpr(e, tpeIdentifier)
        id.setType(tpeIdentifier)
        e.setType(tpeE)

        isNotReassignParam(id)

        expr.setType(TUnit)
        TUnit
      }
      case _ => getTpeBasicExpr(expr)

    }

    def getTpeBasicExpr(expr: ExprTree): Type = {
      var rTpe: Type = TUntyped

      expr match {
        case StringLit(e) => rTpe = TString
        case IntLit(e)    => rTpe = TInt
        case True()       => rTpe = TBoolean
        case False()      => rTpe = TBoolean
        case Identifier(id) => {
          val symbolClass =
            expr.asInstanceOf[Identifier].getSymbol.getClass.getSimpleName

          if (symbolClass.equals("VariableSymbol"))
            rTpe = expr
              .asInstanceOf[Identifier]
              .getSymbol
              .asInstanceOf[VariableSymbol]
              .tpe
          else if (symbolClass.equals("ClassSymbol"))
            rTpe = TClass(
              expr.asInstanceOf[Identifier].getSymbol.asInstanceOf[ClassSymbol]
            )
        }
        case This() => rTpe = expr.getType
        case Null() => rTpe = TAnyType
        case _ => {
          import io.punkt0.Reporter
          Reporter.error("Type error", expr)
          rTpe = TError
        }
      }
      expr.setType(rTpe)
      rTpe
    }

    def tcLogicalOperator(e1: ExprTree, e2: ExprTree): Type = {
      val tpE1 = getTpeExpr(e1)
      val tpE2 = getTpeExpr(e2)

      if (!tpE1.equals(TBoolean)) {
        import io.punkt0.Reporter
        Reporter.error("Expected: TBoolean, Found: " + tpE1, e1)
        TError

      } else if (!tpE2.equals(TBoolean)) {
        import io.punkt0.Reporter
        Reporter.error("Expected: TBoolean, Found: " + tpE2, e2)
        TError

      } else
        TBoolean
    }

    def tcMathOperator(e1: ExprTree, e2: ExprTree, expected: Type): Type = {
      val tpE1 = getTpeExpr(e1)
      val tpE2 = getTpeExpr(e2)

      if (!tpE1.equals(TInt)) {
        import io.punkt0.Reporter
        Reporter.error("Expected: TInt, Found: " + tpE1, e1)
        TError

      } else if (!tpE2.equals(TInt)) {
        import io.punkt0.Reporter
        Reporter.error("Expected: TInt, Found: " + tpE2, e2)
        TError

      } else
        expected
    }

    def tcArgs(method: Identifier, args: List[ExprTree]): Unit = {
      import io.punkt0.Reporter

      val methodSymbol = method.getSymbol.asInstanceOf[MethodSymbol]
      if (!methodSymbol.params.size.equals(args.length))
        Reporter.error(
          "Number of arguments does not match method signature",
          method
        )

      val params = methodSymbol.params.toList
      val newArgTpe = new ListBuffer[Type]

      for (i <- 0 until params.length) {
        tcExpr(args(i), params(i)._2.tpe)

        //Add args to the methodSymbol
        newArgTpe.+=:(getTpeExpr(args(i)))
      }
      //Set new argTpeList
      methodSymbol.argTpeList = newArgTpe.toList
    }

    def attachMethodSymbol(obj: ExprTree, method: Identifier): MethodSymbol = {
      if (
        !obj.getType
          .asInstanceOf[TClass]
          .classSymbol
          .lookupMethod(method.value)
          .isDefined
      ) {
        import io.punkt0.Reporter
        Reporter.error("This object does not contain such a method", method)
      }

      val methodSymbol = obj.getType
        .asInstanceOf[TClass]
        .classSymbol
        .lookupMethod(method.value)
        .get
      var newMethodSymbol = new MethodSymbol(
        methodSymbol.name,
        methodSymbol.classSymbol,
        methodSymbol.tpe
      )
      newMethodSymbol.params = methodSymbol.params

      newMethodSymbol
    }

    def isNotReassignParam(id: Identifier): Unit = {
      for (i <- 0 until prog.classes.length) {
        for (j <- 0 until prog.classes(i).methods.size) {
          import io.punkt0.Reporter
          if (
            prog
              .classes(i)
              .methods(j)
              .getSymbol
              .params
              .get(id.value)
              .equals(id.getSymbol)
          )
            Reporter.error("Cannot reassign parameter", id)
        }
      }
    }
    tcMain
    tcClasses

    prog
  }

}
