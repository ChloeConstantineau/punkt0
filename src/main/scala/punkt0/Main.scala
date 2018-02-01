package punkt0

import java.io.File
import lexer._
import ast._
import analyzer._
import code._

object Main {

  def processOptions(args: Array[String]): Context = {
    var ctx = Context()

    def processOption(args: List[String]): Unit = args match {

      case "--tokens" :: args =>
        ctx = ctx.copy(doTokens = true)
        processOption(args)

      case "--help" :: args =>
        ctx = ctx.copy(doHelp = true)
        displayHelp()
        processOption(args)

      case "-d" :: out :: args =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(args)

      case "--ast" :: args =>
        ctx = ctx.copy(doAST = true) //TODO make/add the function
        processOption(args)

      case "--print" :: args =>
        ctx = ctx.copy(doPrintPrettyTree = true) //TODO make/add the function
        processOption(args)

      case f :: args =>
        ctx = ctx.copy(file = Some(new File(f)))
        processOption(args)

      case List() =>

    }

    processOption(args.toList)

    ctx
  }

  def printTokens(it: Iterator[Token]) {
    var tokenList = it.toList;
    for (i <- 0 until tokenList.length) {
      println(tokenList(i) + "(" + tokenList(i).posString + ")");
    }
  }

  def displayHelp(): Unit = {
    println("Usage: <punkt0c> [options] <file>")
    println("Options include:")
    println(" --help        displays this help")
    println(" -d <outdir>   generates class files in the specified directory")
    println(" --tokens       prints all the token found")
    println(" --ast          prints out the AST")
    println(" --print        pretty prints the AST")
  }

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args)

    //LEXER
    val tokens = Lexer.run(ctx.file.get)(ctx)

    if (ctx.doTokens) {
      printTokens(tokens)
      sys.exit(0)
    }

    //PARSER
    val ast = Parser.run(tokens)(ctx)

    if (ctx.doAST)
      println(ast)

    if (ctx.doPrintPrettyTree)
      Printer.apply(ast)

    if (ctx.doAST || ctx.doTokens)
      sys.exit(0)

  /*  //EVERYTHING
    val allPhases = Lexer andThen Parser andThen NameAnalysis andThen TypeChecking andThen CodeGeneration
    allPhases.run(ctx.file.get)(ctx)
  */  
    val name = NameAnalysis.run(ast)(ctx)
    val typeCk = TypeChecking.run(name)(ctx)
    CodeGeneration.run(typeCk)(ctx)

  }
}
