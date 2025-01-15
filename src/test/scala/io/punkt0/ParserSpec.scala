package io.punkt0

import io.punkt0.ast.Parser
import io.punkt0.lexer.Lexer
import org.scalatest.matchers.should.Matchers.*

import java.io.File
import scala.io.Source

class ParserSpec extends UnitTest:

    private val directory  = (folder: String) => new File(s"./test-programs/ast/$folder/")
    private val readFiles  = (folder: String, suffix: String) =>
      directory(folder).listFiles.filter(_.getName.endsWith(suffix))
    private val computeAST = (file: File) =>
      (Lexer andThen Parser).run(file)(Context(file = Some(file))).toString

    "Parser" should:
        val contentFiles = readFiles("valid", ".p0").sorted
        val assertFiles  = readFiles("valid", ".p0.ast").sorted

        // Valid test programs
        contentFiles.zip(assertFiles).foreach { file =>
          s"generate AST for ${file._1.getName}" in:
              val AST            = computeAST(file._1)
              val sourceExpected = Source.fromFile(file._2)
              val expected       = sourceExpected.getLines().mkString
              sourceExpected.close()

              assert(AST == expected)
        }

        // Invalid test programs
        readFiles("invalid", ".p0").foreach { file =>
          s"fail for ${file.getName}" in:
              assertThrows[Throwable](computeAST(file))
        }
