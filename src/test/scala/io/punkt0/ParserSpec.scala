package io.punkt0

import io.punkt0.ast.Parser
import io.punkt0.lexer.Lexer
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.wordspec.AnyWordSpecLike

import java.io.File
import scala.io.Source

class ParserSpec extends AnyWordSpecLike:

    private val directory  = new File("./testprograms/lab3/valid/")
    private val readFiles  = (suffix: String) => directory.listFiles.filter(_.getName.endsWith(suffix))
    private val computeAST = (file: File) =>
      (Lexer andThen Parser).run(file)(Context(file = Some(file))).toString

    "Parser" should:
        val contentFiles = readFiles(".p0").sorted
        val assertFiles  = readFiles(".p0.ast").sorted

        contentFiles.zip(assertFiles).foreach { file =>
          s"generate AST for ${file._1.getName}" in:
              val AST            = computeAST(file._1)
              val sourceExpected = Source.fromFile(file._2)
              val expected       = sourceExpected.getLines().mkString
              sourceExpected.close()

              assert(AST == expected)
        }
