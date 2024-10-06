package io.punkt0.analyzer

import Symbols._
import scala.collection.mutable.ListBuffer

object Types {

  trait Typed {
    def tpe: TUntyped.type = TUntyped
  }

  abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case class TClass(classSymbol: ClassSymbol) extends Type {
    private val classTree = getClassTree(classSymbol)

    override def isSubTypeOf(tpe: Type): Boolean = {
      classTree.contains(tpe.toString)
    }

    override def toString: String = "[" + classSymbol.name + "]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == TInt
    override def toString = "[Int]"
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == TBoolean
    override def toString = "[Boolean]"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == TString
    override def toString = "[String]"
  }

  case object TUnit extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == TUnit
    override def toString = "[Unit]"
  }

  case object TAnyType extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == TAnyType
    override def toString = "[AnyType]"
  }

  private def getClassTree(classSymbol: ClassSymbol): List[String] = {
    var listBuf = new ListBuffer[String]
    var hasParent = classSymbol.parent.isDefined
    var parent: ClassSymbol = classSymbol

    //Add class to tree
    listBuf.+=("[TAnyType]")
    listBuf.+=("[" + classSymbol.name + "]")

    while (hasParent) {
      parent = parent.parent.get
      listBuf.+=("[" + parent.name + "]")
      hasParent = parent.parent.isDefined
    }

    listBuf.toList
  }
}
