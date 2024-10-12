package io.punkt0

import io.punkt0.ast._
import io.punkt0.lexer._

import java.io.File
import scala.annotation.tailrec

object Main {

  @tailrec
  private def processOptions(args: List[String], context: Context): Context =
    args match {

      case "--tokens" :: args =>
        processOptions(args, context.copy(doTokens = true))

      case "--help" :: args =>
        println("""
            |Usage: <punkt0c> [options] <file>"
            |Options include:"
            | --help        displays this help"
            | -d <outdir>   generates class files in the specified directory"
            | --tokens       prints all the token found"
            | --ast          prints out the AST"
            | --print        pretty prints the AST"
            |""".stripMargin)
        processOptions(args, context.copy(doHelp = true))

      case "-d" :: out :: args =>
        processOptions(args, context.copy(outDir = Some(new File(out))))

      case "--ast" :: args =>
        processOptions(
          args,
          context.copy(doAST = true)
        )

      case "--print" :: args =>
        processOptions(
          args,
          context.copy(doPrintPrettyTree = true)
        )

      case f :: args =>
        processOptions(args, context.copy(file = Some(new File(f))))

      case List() => context
    }

  def main(args: Array[String]): Unit = {
    val context = processOptions(args.toList, Context())

    //LEXER
    val tokens = Lexer.run(context.file.get)(context)
    if (context.doTokens)
      tokens.toList.foreach(println(_))

    //PARSER
    val ast = Parser.run(tokens)(context)
    if (context.doAST)
      println(ast)
  }
}
