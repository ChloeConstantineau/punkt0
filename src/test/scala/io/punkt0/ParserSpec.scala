package io.punkt0

import io.punkt0.ast.Parser
import io.punkt0.lexer.Lexer
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpecLike

import java.io.File
import scala.io.Source

class ParserSpec extends AnyWordSpecLike {

  private val directory = new File("./testprograms/lab3/valid/")
  private val readFiles: String => Array[File] = (suffix: String) =>
    directory.listFiles.filter(_.getName.endsWith(suffix))

  "Parser" should {
    "parse correctly all valid test files" in {
      val contentFiles = readFiles(".p0").sorted
      val assertFiles = readFiles(".p0.ast").sorted

      val ASTs = contentFiles.map(file =>
        (Lexer andThen Parser).run(file)(Context(file = Some(file))).toString
      )
      val asserts = assertFiles.map(f => {
        val source = Source.fromFile(f)
        val content = source.getLines().mkString
        source.close()
        content
      })

      assert(ASTs.zip(asserts).forall(res => res._1 == res._2))
    }
  }
}
