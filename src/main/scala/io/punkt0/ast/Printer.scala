package io.punkt0.ast

import Trees._

import scala.collection.mutable.ListBuffer

object Printer {
  def apply(t: Program): String = {
    val AST = new StringBuilder
    makeClasses(t.classes)
    makeMain(t.main)

    def makeMain(main: MainDecl): Unit = {
      AST.++=("object Main extends " + main.parent.value + " {" + "\n")
      makeVars(main.vars)
      makeExprs(main.exprs)
      AST.++=("}" + "\n")
    }

    def makeClasses(classes: List[ClassDecl]) = {
      if (classes.length.>(0)) {
        for (i <- 0 until classes.length) {
          makeClass(classes(i))
        }
      }
    }
    def makeClass(aClass: ClassDecl): Unit = {
      AST.++=("Class " + aClass.id.value)
      if (aClass.parent.isDefined) {
        AST.append(" extends " + aClass.parent.get.value)
      }
      AST.append(" {" + "\n")
      makeVars(aClass.vars)
      makeMethods(aClass.methods)
      AST.append("}" + "\n")

    }

    def makeVars(vars: List[VarDecl]): Unit = {
      if (vars.length.>(0)) {
        for (i <- 0 until vars.length) {
          AST.++=(
            "\t var " + vars(i).id.value + " : " + getType(vars(i).tpe) + " = "
          )
          getExpr(vars(i).expr)
          if (i != vars.length) {
            AST.+=(';')
          }
          AST.append("\n")
        }
      }
    }

    def makeMethods(methodList: List[MethodDecl]): Unit = {
      for (i <- 0 until methodList.length) {
        makeMethod(methodList(i))
      }
    }

    def makeMethod(method: MethodDecl): Unit = {
      if (method.overrides) {
        AST.append("override ")
      }
      AST.append("def " + method.id.value + "(")
      makeParams(method.args)
      AST.append(") : " + getType(method.retType) + " = {" + "\n")
      makeVars(method.vars)
      var allExprs = new ListBuffer[ExprTree]
      allExprs.++=(method.exprs)
      allExprs.+=(method.retExpr)
      makeExprs(allExprs.toList)
      AST.append("}" + "\n")
    }

    def makeParams(formals: List[Formal]): Unit = {
      if (formals.length.>(0)) {
        for (i <- 0 until formals.length) {
          if (i != formals.length - 1) {
            AST.append(formals(i).id.value + " : " + getType(formals(i).tpe))
            AST.append(", ")
          } else
            AST.append(formals(i).id.value + " : " + getType(formals(i).tpe))
        }
      }

    }

    def getType(tpe: TypeTree): String = tpe match {
      case BooleanType() => "Boolean"
      case StringType()  => "String"
      case IntType()     => "Int"
      case UnitType()    => "Unit"
      case Identifier(s) => s
    }

    def getExpr(expr: ExprTree): Unit = {
      expr match {
        case Not(expr) => {
          AST.append("!")
          getExpr(expr)
        }
        case Println(expr) => {
          AST.append("println(")
          getExpr(expr)
          AST.append(")")
        }
        case While(expr, block) => {
          AST.append("while (")
          getExpr(expr)
          AST.append(")")
          getExpr(block)
        }
        case If(expr, thn, elseExpr) => {
          AST.append("if(")
          getExpr(expr)
          AST.append(")" + "\n")
          getExpr(thn)
          if (elseExpr.isDefined) {
            AST.append("\n" + "else " + "\n")
            getExpr(elseExpr.get)
          }
        }
        case Block(listExpr) => {
          AST.append(" { " + "\n")
          for (i <- 0 until listExpr.length) {
            if (i != listExpr.length - 1) {
              getExpr(listExpr(i))
              AST.append(";" + "\n")
            } else
              getExpr(listExpr(i))
          }
          AST.append(" } " + "\n")
        }
        case Assign(id, expr) => {
          AST.append(id.value + " = ")
          getExpr(expr)
        }
        case And(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" && ")
          getExpr(rhs)
        }
        case Or(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" || ")
          getExpr(rhs)
        }
        case Plus(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" + ")
          getExpr(rhs)
        }
        case Minus(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" - ")
          getExpr(rhs)
        }
        case Times(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" * ")
          getExpr(rhs)
        }
        case Div(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" / ")
          getExpr(rhs)
        }
        case LessThan(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" < ")
          getExpr(rhs)
        }
        case Equals(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" == ")
          getExpr(rhs)
        }
        case MethodCall(obj, method, args) => {
          getExpr(obj)
          AST.append("." + method.value + "(")
          if (args.length.>(0)) {
            for (i <- 0 until args.length) {
              if (i != args.length - 1) {
                getExpr(args(i))
                AST.append(", ")
              } else
                getExpr(args(i))
            }
          }
          AST.append(")")
        }
        case _ => getExprInternal(expr)
      }
    }

    def getExprInternal(expr: ExprTree): Unit = {
      expr match {
        case True()             => AST.append("true")
        case False()            => AST.append("false")
        case Identifier(id)     => AST.append(id)
        case This()             => AST.append("this")
        case Null()             => AST.append("null")
        case New(id)            => AST.append("new " + id.value)
        case StringLit(aString) => AST.append("\"" + aString + "\"")
        case IntLit(int)        => AST.append(int)
        case _                  => println("ERROR")
      }
    }

    def makeExprs(exprs: List[ExprTree]): Unit = {
      if (exprs.length.>(0)) {
        for (i <- 0 until exprs.length) {
          AST.+=('\t')
          if (i != exprs.length - 1) {
            getExpr(exprs(i))
            AST.+=(';')
          } else
            getExpr(exprs(i))
          AST.append("\n")
        }
      }
    }

    AST.toString
  }

  //Use this apply to print the symbols as well

  def applySymbols(t: Program): String = {
    val AST = new StringBuilder
    makeClasses(t.classes)
    makeMain(t.main)

    def makeMain(main: MainDecl): Unit = {
      AST.++=(
        "object " + main.obj.value + "#" + main.getSymbol.id + " extends " + main.parent.value + "#" + main.parent.getSymbol.id + " {" + "\n"
      )
      makeVars(main.vars)
      makeExprs(main.exprs)
      AST.++=("}" + "\n")
    }

    def makeClasses(classes: List[ClassDecl]) = {
      if (classes.length.>(0)) {
        for (i <- 0 until classes.length) {
          makeClass(classes(i))
        }
      }
    }
    def makeClass(aClass: ClassDecl): Unit = {
      AST.++=("Class " + aClass.id.value + "#" + aClass.getSymbol.id)
      if (aClass.parent.isDefined) {
        AST.append(
          " extends " + aClass.parent.get.value + "#" + aClass.getSymbol.parent.get.id
        )
      }
      AST.append(" {" + "\n")
      makeVars(aClass.vars)
      makeMethods(aClass.methods)
      AST.append("}" + "\n")

    }

    def makeVars(vars: List[VarDecl]): Unit = {
      if (vars.length.>(0)) {
        for (i <- 0 until vars.length) {
          AST.++=(
            "\tvar " + vars(i).id.value + "#" + vars(
              i
            ).getSymbol.id + " : " + getType(vars(i).tpe) + " = "
          )
          getExpr(vars(i).expr)
          if (i != vars.length) {
            AST.+=(';')
          }
          AST.append("\n")
        }
        AST.append("\n")
      }
    }

    def makeMethods(methodList: List[MethodDecl]): Unit = {
      for (i <- 0 until methodList.length) {
        makeMethod(methodList(i))
      }
    }

    def makeMethod(method: MethodDecl): Unit = {
      if (method.overrides) {
        AST.append("override ")
      }
      AST.append("def " + method.id.value + "#" + method.getSymbol.id + "(")
      makeParams(method.args)
      AST.append(") : " + getType(method.retType) + " = {" + "\n")
      makeVars(method.vars)
      var allExprs = new ListBuffer[ExprTree]
      allExprs.++=(method.exprs)
      allExprs.+=(method.retExpr)
      makeExprs(allExprs.toList)
      AST.append("}" + "\n")
    }

    def makeParams(formals: List[Formal]): Unit = {
      if (formals.length.>(0)) {
        for (i <- 0 until formals.length) {
          if (i != formals.length - 1) {
            AST.append(
              formals(i).id.value + "#" + formals(
                i
              ).getSymbol.id + " : " + getType(formals(i).tpe)
            )
            AST.append(", ")
          } else
            AST.append(
              formals(i).id.value + "#" + formals(
                i
              ).getSymbol.id + " : " + getType(formals(i).tpe)
            )
        }
      }

    }

    def getType(tpe: TypeTree): String = tpe match {
      case BooleanType() => "Boolean"
      case StringType()  => "String"
      case IntType()     => "Int"
      case UnitType()    => "Unit"
      case Identifier(s) => s
    }

    def getExpr(expr: ExprTree): Unit = {
      expr match {
        case Not(expr) => {
          AST.append("!")
          getExpr(expr)
        }
        case Println(expr) => {
          AST.append("println(")
          getExpr(expr)
          AST.append(")")
        }
        case While(expr, block) => {
          AST.append("while (")
          getExpr(expr)
          AST.append(")")
          getExpr(block)
        }
        case If(expr, thn, elseExpr) => {
          AST.append("if(")
          getExpr(expr)
          AST.append(")" + "\n\t\t")
          getExpr(thn)
          if (elseExpr.isDefined) {
            AST.append("\n\t" + "else " + "\n\t\t")
            getExpr(elseExpr.get)
          }
        }
        case Block(listExpr) => {
          AST.append(" { " + "\n")
          for (i <- 0 until listExpr.length) {
            AST.append("\t\t")
            if (i != listExpr.length - 1) {
              getExpr(listExpr(i))
              AST.append(";" + "\n")
            } else
              getExpr(listExpr(i))
          }
          AST.append(" } " + "\n")
        }
        case Assign(id, expr) => {
          AST.append(id.value + "#" + id.getSymbol.id + " = ")
          getExpr(expr)
        }
        case And(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" && ")
          getExpr(rhs)
        }
        case Or(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" || ")
          getExpr(rhs)
        }
        case Plus(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" + ")
          getExpr(rhs)
        }
        case Minus(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" - ")
          getExpr(rhs)
        }
        case Times(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" * ")
          getExpr(rhs)
        }
        case Div(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" / ")
          getExpr(rhs)
        }
        case LessThan(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" < ")
          getExpr(rhs)
        }
        case Equals(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" == ")
          getExpr(rhs)
        }
        case MethodCall(obj, method, args) => {
          getExpr(obj)
          AST.append("." + method.value + "(")
          if (args.length.>(0)) {
            for (i <- 0 until args.length) {
              if (i != args.length - 1) {
                getExpr(args(i))
                AST.append(", ")
              } else
                getExpr(args(i))
            }
          }
          AST.append(")")
        }
        case _ => getExprInternal(expr)
      }
    }

    def getExprInternal(expr: ExprTree): Unit = {
      expr match {
        case True()  => AST.append("true")
        case False() => AST.append("false")
        case Identifier(id) =>
          AST.append(id + "#" + expr.asInstanceOf[Identifier].getSymbol.id)
        case This() => AST.append("this")
        case Null() => AST.append("null")
        case New(id) => {
          AST.append("new ")
          getExprInternal(id)
        }
        case StringLit(aString) => AST.append("\"" + aString + "\"")
        case IntLit(int)        => AST.append(int)
        case _                  => println("ERROR")
      }
    }

    def makeExprs(exprs: List[ExprTree]): Unit = {
      if (exprs.length.>(0)) {
        for (i <- 0 until exprs.length) {
          AST.+=('\t')
          if (i != exprs.length - 1) {
            getExpr(exprs(i))
            AST.+=(';')
          } else
            getExpr(exprs(i))
          AST.append("\n")
        }
      }
    }

    AST.toString
  }

  //Use this apply to print types

  def applyTypes(t: Program): String = {
    val AST = new StringBuilder
    makeClasses(t.classes)
    makeMain(t.main)

    def makeMain(main: MainDecl): Unit = {
      AST.++=(
        "object " + main.obj.value + " " + main.getType + " extends " + main.parent.value + " " + main.parent.getType + " {" + "\n"
      )
      makeVars(main.vars)
      makeExprs(main.exprs)
      AST.++=("}" + "\n")
    }

    def makeClasses(classes: List[ClassDecl]) = {
      if (classes.length.>(0)) {
        for (i <- 0 until classes.length) {
          makeClass(classes(i))
        }
      }
    }
    def makeClass(aClass: ClassDecl): Unit = {
      AST.++=("Class " + aClass.id.value + " " + aClass.getType)
      if (aClass.parent.isDefined) {
        AST.append(
          " extends " + aClass.parent.get.value + " " + aClass.parent.get.getType
        )
      }
      AST.append(" {" + "\n")
      makeVars(aClass.vars)
      makeMethods(aClass.methods)
      AST.append("}" + "\n")

    }

    def makeVars(vars: List[VarDecl]): Unit = {
      if (vars.length.>(0)) {
        for (i <- 0 until vars.length) {
          AST.++=(
            "\tvar " + vars(i).id.value + " " + vars(
              i
            ).getType + " : " + getType(vars(i).tpe) + " = "
          )
          getExpr(vars(i).expr)
          if (i != vars.length) {
            AST.+=(';')
          }
          AST.append("\n")
        }
        AST.append("\n")
      }
    }

    def makeMethods(methodList: List[MethodDecl]): Unit = {
      for (i <- 0 until methodList.length) {
        makeMethod(methodList(i))
      }
    }

    def makeMethod(method: MethodDecl): Unit = {
      if (method.overrides) {
        AST.append("override ")
      }
      AST.append("def " + method.id.value + " " + method.getType + "(")
      makeParams(method.args)
      AST.append(") : " + getType(method.retType) + " = {" + "\n")
      makeVars(method.vars)
      var allExprs = new ListBuffer[ExprTree]
      allExprs.++=(method.exprs)
      allExprs.+=(method.retExpr)
      makeExprs(allExprs.toList)
      AST.append("}" + "\n")
    }

    def makeParams(formals: List[Formal]): Unit = {
      if (formals.length.>(0)) {
        for (i <- 0 until formals.length) {
          if (i != formals.length - 1) {
            AST.append(
              formals(i).id.value + " " + formals(i).getType + " : " + getType(
                formals(i).tpe
              )
            )
            AST.append(", ")
          } else
            AST.append(
              formals(i).id.value + " " + formals(i).getType + " : " + getType(
                formals(i).tpe
              )
            )
        }
      }

    }

    def getType(tpe: TypeTree): String = tpe match {
      case BooleanType() => "Boolean"
      case StringType()  => "String"
      case IntType()     => "Int"
      case UnitType()    => "Unit"
      case Identifier(s) => s
    }

    def getExpr(e: ExprTree): Unit = {
      e match {
        case Not(expr) => {
          AST.append("!")
          getExpr(expr)
          AST.append("NOT" + e.getType)
        }
        case Println(expr) => {
          AST.append("println(")
          getExpr(expr)
          AST.append(")")
          AST.append("PRINTLN" + e.getType)
        }
        case While(expr, block) => {
          AST.append("while (")
          getExpr(expr)
          AST.append(")")
          AST.append("CONDITION" + expr.getType)
          AST.append("WHILE" + e.getType)
          getExpr(block)
        }
        case If(expr, thn, elseExpr) => {
          AST.append("if(")
          getExpr(expr)
          AST.append(
            ")" + "CONDITION" + expr.getType + " IF" + e.getType + "\n\t\t"
          )
          getExpr(thn)
          if (elseExpr.isDefined) {
            AST.append(
              "\n\t" + "else " + "ELSE" + elseExpr.get.getType + "\n\t\t"
            )
            getExpr(elseExpr.get)
          }
        }
        case Block(listExpr) => {
          AST.append(" { " + "\n")
          for (i <- 0 until listExpr.length) {
            AST.append("\t\t")
            if (i != listExpr.length - 1) {
              getExpr(listExpr(i))
              AST.append(";" + "\n")
            } else
              getExpr(listExpr(i))
          }
          AST.append(" } " + "BLOCK" + e.getType + "\n")
        }
        case Assign(id, expr) => {
          AST.append(id.value + " " + id.getType + " = ")
          getExpr(expr)
          AST.append("ASSIGN" + e.getType)
        }
        case And(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" && ")
          getExpr(rhs)
          AST.append("AND" + e.getType)
        }
        case Or(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" || ")
          getExpr(rhs)
          AST.append("OR" + e.getType)
        }
        case Plus(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" + ")
          getExpr(rhs)
          AST.append("PLUS" + e.getType)
        }
        case Minus(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" - ")
          getExpr(rhs)
          AST.append("MINUS" + e.getType)
        }
        case Times(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" * ")
          getExpr(rhs)
          AST.append("TIMES" + e.getType)
        }
        case Div(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" / ")
          getExpr(rhs)
          AST.append("DIV" + e.getType)
        }
        case LessThan(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" < ")
          getExpr(rhs)
          AST.append("LESSTHAN" + e.getType)
        }
        case Equals(lhs, rhs) => {
          getExpr(lhs)
          AST.append(" == ")
          getExpr(rhs)
          AST.append("EQUALS" + e.getType)
        }
        case MethodCall(obj, method, args) => {
          getExpr(obj)
          AST.append("." + method.value + " METHODCALL" + method.getType + "(")
          if (args.length.>(0)) {
            for (i <- 0 until args.length) {
              if (i != args.length - 1) {
                getExpr(args(i))
                AST.append(", ")
              } else
                getExpr(args(i))
            }
          }
          AST.append(")")
        }
        case _ => getExprInternal(e)
      }
    }

    def getExprInternal(expr: ExprTree): Unit = {
      expr match {
        case True()         => AST.append("true" + expr.getType)
        case False()        => AST.append("false" + expr.getType)
        case Identifier(id) => AST.append(id + " " + expr.getType)
        case This()         => AST.append("this" + expr.getType)
        case Null()         => AST.append("null" + expr.getType)
        case New(id) => {
          AST.append("new ")
          getExprInternal(id)
          AST.append("NEW" + expr.getType)
        }
        case StringLit(aString) =>
          AST.append("\"" + aString + "\"" + expr.getType)
        case IntLit(int) => AST.append(int + " " + expr.getType)
        case _           => println("ERROR")
      }
    }

    def makeExprs(exprs: List[ExprTree]): Unit = {
      if (exprs.length.>(0)) {
        for (i <- 0 until exprs.length) {
          AST.+=('\t')
          if (i != exprs.length - 1) {
            getExpr(exprs(i))
            AST.+=(';')
          } else
            getExpr(exprs(i))
          AST.append("\n")
        }
      }
    }

    AST.toString
  }
}
