//package io.punkt0.analyzer
//
//import Symbols._
//import scala.collection.mutable.ListBuffer
//
//object Types {
//
//  trait Typed {
//    private var _tpe: Type = TUntyped
//
//    def setType(tpe: Type): this.type = { _tpe = tpe; this }
//    def getType: Type = _tpe
//  }
//
//  abstract class Type {
//    def isSubTypeOf(tpe: Type): Boolean
//  }
//
//  case object TError extends Type {
//    override def isSubTypeOf(tpe: Type): Boolean = true
//    override def toString = "[error]"
//  }
//
//  case object TUntyped extends Type {
//    override def isSubTypeOf(tpe: Type): Boolean = false
//    override def toString = "[untyped]"
//  }
//
//  case class TClass(classSymbol: ClassSymbol) extends Type {
//    private val classTree = getClassTree(classSymbol)
//
//    override def isSubTypeOf(tpe: Type): Boolean = {
//      classTree.contains(tpe.toString())
//    }
//
//    override def toString = "[" + classSymbol.name + "]"
//  }
//
//  case object TInt extends Type {
//    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
//      case TInt => true
//      case _    => false
//    }
//    override def toString = "[Int]"
//  }
//
//  case object TBoolean extends Type {
//    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
//      case TBoolean => true
//      case _        => false
//    }
//    override def toString = "[Boolean]"
//  }
//
//  case object TString extends Type {
//    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
//      case TString => true
//      case _       => false
//    }
//    override def toString = "[String]"
//  }
//
//  case object TUnit extends Type {
//    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
//      case TUnit => true
//      case _     => false
//    }
//    override def toString = "[Unit]"
//  }
//
//  case object TAnyType extends Type {
//    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
//      case TClass(e) => true
//      case _         => false
//    }
//    override def toString = "[AnyType]"
//  }
//
//  def getClassTree(classSymbol: ClassSymbol): List[String] = {
//    var list = new ListBuffer[String]
//    var hasParent = classSymbol.parent.isDefined
//    var parent: ClassSymbol = classSymbol
//
//    //Add class to tree
//    list.+=("[TAnyType]")
//    list.+=("[" + classSymbol.name + "]")
//
//    while (hasParent) {
//      parent = parent.parent.get
//      list.+=("[" + parent.name + "]")
//      hasParent = parent.parent.isDefined
//    }
//
//    list.toList
//  }
//}
