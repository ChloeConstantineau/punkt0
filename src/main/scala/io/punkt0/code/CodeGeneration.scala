package io.punkt0.code

import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import io.punkt0.{Context, Phase}
import io.punkt0.analyzer.Symbols._
import io.punkt0.analyzer.Types._
import io.punkt0.ast.Trees.{New => punkt0New, _}

import scala.collection.convert.ImplicitConversions.`map AsJavaMap`
import scala.collection.mutable.ListBuffer

object CodeGeneration extends Phase[Program, Unit] {

  var slotFor: Map[Int, Int] = Map()
  var currentClass: String = ""
  var currentRetType: Type = TUnit

  def run(prog: Program)(ctx: Context): Unit = {
    import cafebabe.AbstractByteCodes.AbstractByteCodeGenerator
    import cafebabe.{ClassFile, CodeHandler}

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(
        sourceName: String,
        ct: ClassDecl,
        dir: String
    ): Unit = {
      import cafebabe.ClassFile
      // TODO: Create code handler, save to files ...

      val className = ct.id.value
      val parentName: Option[String] =
        if (ct.parent.isDefined) Some(ct.parent.get.value)
        else Some("java/lang/Object")

      val classFile = new ClassFile(className, parentName)
      classFile.setSourceFile(sourceName);
      classFile.addDefaultConstructor

      ct.vars.foreach { field: VarDecl =>
        classFile.addField(toByteCodeType(field.getType), field.id.value);
      }

      ct.methods.foreach { method: MethodDecl =>
        import cafebabe.CodeHandler
        val ch: CodeHandler = classFile
          .addMethod(
            toByteCodeType(method.getType),
            method.id.value,
            method.args.map { arg: Formal =>
              toByteCodeType(arg.getType)
            }
          )
          .codeHandler

        currentClass = ct.id.value;
        generateMethodCode(ch, method)
      }

      classFile.writeToFile(dir + className + ".class")
    }

    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {

      slotFor.clear()
      currentRetType = mt.getSymbol.tpe

      for (i <- 1 to mt.args.length) {

        slotFor.put(mt.args(i - 1).getSymbol.id, i)
      }

      mt.vars.foreach { v: VarDecl =>
        slotFor.put(v.getSymbol.id, ch.getFreshVar)
      }

      val optimizedMethodExpr: List[ExprTree] =
        optimizeExprs(mt.exprs, mt.retType.getType)
      (optimizedMethodExpr.map(compileExpr(_, ch)) ::: List(
        compileExpr(mt.retExpr, ch)
      ))
        .foldLeft(ch)((ch, bcg) => ch << bcg)

      ch << {
        mt.retExpr.getType match {
          case TInt =>
            IRETURN
          case TBoolean => IRETURN
          case _        => ARETURN
        }
      }

      ch.print

      ch.freeze
    }

    def toByteCodeType(tpe: Type): String = tpe match {
      case TUnit     => "V"
      case TString   => "Ljava/lang/String;"
      case TBoolean  => "Z"
      case TInt      => "I"
      case TClass(c) => "Lpackage/" + c + ";"
      case _ =>
        import io.punkt0.Reporter
        Reporter.fatal("This type should not be here....")
    }

    def toByteCodeTypes(tpe: TypeTree): String = tpe match {
      case UnitType()    => "V"
      case StringType()  => "Ljava/lang/String;"
      case BooleanType() => "Z"
      case IntType()     => "I"
      case Identifier(c) => "Lpackage/" + c + ";"
      case _ =>
        import io.punkt0.Reporter
        Reporter.fatal("This type tree should not be here....")
    }

    def optimizeExprs(expr: List[ExprTree], retTpe: Type): List[ExprTree] = {

      val cleanExprs = new ListBuffer[ExprTree]

      expr.foreach(x => {
        var tpe = x.getType
        if (tpe.equals(TUnit) || tpe.equals(retTpe))
          cleanExprs.+=(x)
      })

      cleanExprs.toList
    }

    def compileExpr(
        expr: ExprTree,
        ch: CodeHandler
    ): AbstractByteCodeGenerator = { (ch: CodeHandler) =>
      {
        expr match {

          case Block(stats: List[ExprTree]) =>
            ch << Comment("Block, " + stats(0).getLine())
            stats.foldLeft(ch)((ch, bgc) =>
              /*if (!bgc.getType.equals(currentRetType) && !bgc.getType.equals(TUnit)) {
                  ch << NOP
                } else*/
              ch << compileExpr(bgc, ch)
            )

          case If(expr: ExprTree, thn: ExprTree, els: Option[ExprTree]) =>
            ch << Comment("If, " + expr.getLine())
            val afterLabel: String = ch.getFreshLabel("after")

            if (els.isDefined) {

              val elseLabel: String = ch.getFreshLabel("else")
              ch <<
                compileExpr(expr, ch) <<
                IfEq(elseLabel) <<
                compileExpr(thn, ch) <<
                Goto(afterLabel) <<
                Label(elseLabel) <<
                compileExpr(els.get, ch) <<
                Label(afterLabel)
            } else {
              ch <<
                compileExpr(expr, ch) <<
                IfEq(afterLabel) <<
                compileExpr(thn, ch) <<
                Label(afterLabel)
            }

          case While(cond: ExprTree, body: ExprTree) =>
            ch << Comment("While, " + cond.getLine())
            val whileLabel = ch.getFreshLabel("while")
            val afterLabel = ch.getFreshLabel("after")

            ch <<
              Label(whileLabel) <<
              compileExpr(cond, ch) <<
              IfEq(afterLabel) <<
              compileExpr(body, ch) <<
              Goto(whileLabel) <<
              Label(afterLabel)

          case Println(expr: ExprTree) =>
            ch << Comment("Println, " + expr.getLine())
            expr.getType match {

              case TString =>
                ch <<
                  GetStatic(
                    "java/lang/System",
                    "out",
                    "Ljava/io/PrintStream;"
                  ) <<
                  compileExpr(expr, ch) <<
                  InvokeVirtual(
                    "java/io/PrintStream",
                    "println",
                    "(Ljava/lang/String;)V"
                  )

              case TInt =>
                ch <<
                  GetStatic(
                    "java/lang/System",
                    "out",
                    "Ljava/io/PrintStream;"
                  ) <<
                  compileExpr(expr, ch) <<
                  InvokeVirtual("java/io/PrintStream", "println", "(I)V")

              case TBoolean =>
                ch <<
                  GetStatic(
                    "java/lang/System",
                    "out",
                    "Ljava/io/PrintStream;"
                  ) <<
                  compileExpr(expr, ch) <<
                  InvokeVirtual("java/io/PrintStream", "println", "(Z)V")

              case _ =>
                sys.error("Println cannot print this type, wrong code gen");
            }

          case Assign(id: Identifier, expr: ExprTree) =>
            ch << Comment("Assign, " + id.getLine())
            //local variable or argument
            if (slotFor.contains(id.getSymbol.id)) {
              ch <<
                compileExpr(expr, ch) << {

                  id.getType match {
                    case TInt     => IStore(slotFor(id.getSymbol.id))
                    case TBoolean => IStore(slotFor(id.getSymbol.id))
                    case TString  => AStore(slotFor(id.getSymbol.id))
                    case _        => AStore(slotFor(id.getSymbol.id)) //Class obj
                  }
                }
            } else { //is a field
              ch <<
                ALOAD_0 <<
                compileExpr(expr, ch) <<
                PutField(currentClass, id.value, toByteCodeType(id.getType))
            }
          case And(lhs: ExprTree, rhs: ExprTree) =>
            ch << Comment("And, " + lhs.getLine())
            val elseLabel = ch.getFreshLabel("else")
            val afterLabel = ch.getFreshLabel("after")
            ch <<
              compileExpr(lhs, ch) <<
              IfEq(elseLabel) <<
              compileExpr(rhs, ch) <<
              Goto(afterLabel) <<
              Label(elseLabel) <<
              ICONST_0 <<
              Label(afterLabel)

          case Or(lhs: ExprTree, rhs: ExprTree) =>
            ch << Comment("Or, " + lhs.getLine())
            val elseLabel = ch.getFreshLabel("else")
            val afterLabel = ch.getFreshLabel("after")
            ch <<
              compileExpr(lhs, ch) <<
              IfNe(elseLabel) <<
              compileExpr(rhs, ch) <<
              Goto(afterLabel) <<
              Label(elseLabel) <<
              ICONST_1 <<
              Label(afterLabel)

          case Plus(lhs: ExprTree, rhs: ExprTree) =>
            ch << Comment("Plus, " + lhs.getLine())
            lhs.getType match {
              case TInt =>
                rhs.getType match {

                  case TInt =>
                    ch <<
                      compileExpr(lhs, ch) <<
                      compileExpr(rhs, ch) <<
                      IADD
                  case TString =>
                    ch <<
                      DefaultNew("java/lang/StringBuilder") <<
                      compileExpr(lhs, ch) <<
                      InvokeVirtual(
                        "java/lang/StringBuilder",
                        "append",
                        "(I)Ljava/lang/StringBuilder;"
                      ) <<
                      compileExpr(rhs, ch) <<
                      InvokeVirtual(
                        "java/lang/StringBuilder",
                        "append",
                        "(Ljava/lang/String;)Ljava/lang/StringBuilder;"
                      ) <<
                      InvokeVirtual(
                        "java/lang/StringBuilder",
                        "toString",
                        "()Ljava/lang/String;"
                      )
                  case _ =>
                    sys.error(
                      "Internal Error: Wrong type for plus in Code generation"
                    )
                }

              case TString =>
                rhs.getType match {

                  case TInt =>
                    ch <<
                      DefaultNew("java/lang/StringBuilder") <<
                      compileExpr(lhs, ch) <<
                      InvokeVirtual(
                        "java/lang/StringBuilder",
                        "append",
                        "(Ljava/lang/String;)Ljava/lang/StringBuilder;"
                      ) <<
                      compileExpr(rhs, ch) <<
                      InvokeVirtual(
                        "java/lang/StringBuilder",
                        "append",
                        "(I)Ljava/lang/StringBuilder;"
                      ) <<
                      InvokeVirtual(
                        "java/lang/StringBuilder",
                        "toString",
                        "()Ljava/lang/String;"
                      )
                  case TString =>
                    ch <<
                      DefaultNew("java/lang/StringBuilder") <<
                      compileExpr(lhs, ch) <<
                      InvokeVirtual(
                        "java/lang/StringBuilder",
                        "append",
                        "(Ljava/lang/String;)Ljava/lang/StringBuilder;"
                      ) <<
                      compileExpr(rhs, ch) <<
                      InvokeVirtual(
                        "java/lang/StringBuilder",
                        "append",
                        "(Ljava/lang/String;)Ljava/lang/StringBuilder;"
                      ) <<
                      InvokeVirtual(
                        "java/lang/StringBuilder",
                        "toString",
                        "()Ljava/lang/String;"
                      )

                  case _ =>
                    sys.error(
                      "Internal Error: Wrong type for plus in Code generation"
                    )
                }
              case _ =>
                sys.error(
                  "Internal Error: Wrong type for plus in Code generation"
                )
            }

          case Minus(lhs: ExprTree, rhs: ExprTree) =>
            ch << Comment("Minus, " + lhs.getLine())
            ch <<
              compileExpr(lhs, ch) <<
              compileExpr(rhs, ch) <<
              ISUB

          case Times(lhs: ExprTree, rhs: ExprTree) =>
            ch << Comment("Times, " + lhs.getLine())
            ch <<
              compileExpr(lhs, ch) <<
              compileExpr(rhs, ch) <<
              IMUL

          case Div(lhs: ExprTree, rhs: ExprTree) =>
            ch << Comment("Div, " + lhs.getLine())
            ch <<
              compileExpr(lhs, ch) <<
              compileExpr(rhs, ch) <<
              IDIV

          case LessThan(lhs: ExprTree, rhs: ExprTree) =>
            ch << Comment("LessThan, " + lhs.getLine())
            val trueLabel = ch.getFreshLabel("true")
            val afterLabel = ch.getFreshLabel("after")
            ch <<
              compileExpr(lhs, ch) <<
              compileExpr(rhs, ch) <<
              If_ICmpLt(trueLabel) <<
              ICONST_0 <<
              Goto(afterLabel) <<
              Label(trueLabel) <<
              ICONST_1 <<
              Label(afterLabel)

          case Equals(lhs: ExprTree, rhs: ExprTree) =>
            ch << Comment("Equals, " + lhs.getLine())
            val equalLabel = ch.getFreshLabel("equal")
            val afterLabel = ch.getFreshLabel("after")

            lhs.getType match {

              case TInt =>
                ch <<
                  compileExpr(lhs, ch) <<
                  compileExpr(rhs, ch) <<
                  If_ICmpEq(equalLabel) <<
                  ICONST_0 <<
                  Goto(afterLabel) <<
                  Label(equalLabel) <<
                  ICONST_1 <<
                  Label(afterLabel)

              case TBoolean =>
                ch <<
                  compileExpr(lhs, ch) <<
                  compileExpr(rhs, ch) <<
                  If_ICmpEq(equalLabel) <<
                  ICONST_0 <<
                  Goto(afterLabel) <<
                  Label(equalLabel) <<
                  ICONST_1 <<
                  Label(afterLabel)

              case _ =>
                ch <<
                  compileExpr(lhs, ch) <<
                  compileExpr(rhs, ch) <<
                  If_ACmpEq(equalLabel) <<
                  ICONST_0 <<
                  Goto(afterLabel) <<
                  Label(equalLabel) <<
                  ICONST_1 <<
                  Label(afterLabel)
            }

          case MethodCall(
                obj: ExprTree,
                meth: Identifier,
                args: List[ExprTree]
              ) =>
            ch << Comment("MethodCall, " + obj.getLine())

            val ms = meth.getSymbol match {
              case m: MethodSymbol => m
              case _               => sys.error("Method call failed at code generation")
            }

            val methodName = meth.value;
            val className = ms.classSymbol.name
            val argList = ms.argTpeList
            val methodSig = "(" + ms.argTpeList.foldLeft("")((acc, arg) =>
              acc +
                toByteCodeType(arg)
            ) + ")" + toByteCodeType(ms.tpe)

            val s =
              (List(compileExpr(obj, ch)) ::: args.map(compileExpr(_, ch)))
            s.foldLeft(ch)((ch, bcg) => ch << bcg) <<
              InvokeVirtual(className, methodName, methodSig)

          case IntLit(value: Int) =>
            ch << Comment("IntLit, " + expr.getLine())
            ch << Ldc(value)

          case StringLit(value: String) =>
            ch << Comment("StringLit, " + expr.getLine())
            ch << Ldc(value)

          case True() =>
            ch << Comment("True, " + expr.getLine())
            ch << ICONST_1

          case False() =>
            ch << Comment("False, " + expr.getLine())
            ch << ICONST_0

          case id: Identifier =>
            ch << Comment("Identifier, " + expr.getLine())
            // If it's a local var or an arg
            if (slotFor.contains(id.getSymbol.id)) {
              id.getType match {
                case TInt =>
                  ch << ILoad(slotFor(id.getSymbol.id))
                case TBoolean =>
                  ch << ILoad(slotFor(id.getSymbol.id))
                case _ =>
                  ch << ALoad(slotFor(id.getSymbol.id))
              }
            } // is a field
            else {
              ch <<
                ALOAD_0 <<
                GetField(currentClass, id.value, toByteCodeType(id.getType))
            }

          case This() =>
            ch << Comment("This, " + expr.getLine())
            ch << ALOAD_0

          case punkt0New(tpe: Identifier) =>
            ch << Comment("New, " + tpe.getLine())
            ch << DefaultNew(tpe.value)

          case Not(expr: ExprTree) =>
            ch << Comment("Not, " + expr.getLine())
            val trueLabel = ch.getFreshLabel("true")
            val afterLabel = ch.getFreshLabel("after")
            ch <<
              compileExpr(expr, ch) <<
              IfEq(trueLabel) <<
              ICONST_0 <<
              Goto(afterLabel) <<
              Label(trueLabel) <<
              ICONST_1 <<
              Label(afterLabel)

          case _ => sys.error("Internal error in code generation")
        }
      }
    }

    def generateMainMethodCode(
        ch: CodeHandler,
        stmts: List[ExprTree],
        cname: String
    ): Unit = {

      stmts.map(compileExpr(_, ch)).foldLeft(ch)((ch, bcg) => ch << bcg)
      ch << RETURN
      ch.freeze
    }

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.get.getName

    // output code
    prog.classes foreach { ct =>
      generateClassFile(sourceName, ct, outDir)
    }

    // Now do the main declaration
    // ...

    currentRetType = TUnit
    val mainName = prog.main.obj.value
    val parentMainName = prog.main.parent.value

    val mainFile = new ClassFile(mainName, None)
    mainFile.setSourceFile(sourceName)
    mainFile.addDefaultConstructor

    val mainHandler = mainFile.addMainMethod.codeHandler

    val mainMethodOptimized = optimizeExprs(prog.main.exprs, prog.main.getType)
    generateMainMethodCode(mainHandler, mainMethodOptimized, mainName)
    mainFile.writeToFile(outDir + mainName + ".class")
  }

}
