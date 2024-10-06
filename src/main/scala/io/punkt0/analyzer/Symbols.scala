//package io.punkt0.analyzer
//
//import io.punkt0.analyzer.Types._
//import io.punkt0.positioned.PositionedT
//
//object Symbols {
//
//  trait Symbolic[S <: Symbol] {
//    private var _sym: Option[S] = None
//
//    def setSymbol(sym: S): this.type = {
//      _sym = Some(sym)
//      this
//    }
//
//    def getSymbol: S = _sym match {
//      case Some(s) => s
//      case None    => sys.error("Accessing undefined symbol.")
//    }
//  }
//
//  abstract class Symbol extends PositionedT {
//    val id: Int = ID.next
//    val name: String
//  }
//
//  private object ID {
//    private var c: Int = 0
//
//    def next: Int = {
//      val ret = c
//      c = c + 1
//      ret
//    }
//  }
//
//  class GlobalScope {
//    var mainClass: ClassSymbol = _
//    var classes = Map[String, ClassSymbol]()
//
//    def lookupClass(n: String): Option[ClassSymbol] = {
//      classes.get(n)
//    }
//  }
//
//  class ClassSymbol(val name: String) extends Symbol {
//    var parent: Option[ClassSymbol] = None
//    var methods = Map[String, MethodSymbol]()
//    var members = Map[String, VariableSymbol]()
//
//    def lookupMethod(n: String): Option[MethodSymbol] = {
//      // Method exists in current class
//      if (methods.contains(n))
//        methods.get(n)
//      else {
//        lookupMethodInParentTree(n)
//      }
//    }
//
//    def lookupMethodInParentTree(n: String): Option[MethodSymbol] = {
//      //Look for Method in parent tree
//      var upperParent = parent
//      var continue: Boolean = true
//      var matchingMethod: Option[MethodSymbol] = None
//
//      while (continue && upperParent.isDefined) {
//        if (upperParent.get.methods.contains(n)) {
//          continue = false
//          matchingMethod = upperParent.get.methods.get(n)
//
//        } else
//          upperParent = upperParent.get.parent
//      }
//      matchingMethod
//    }
//    def lookupVar(n: String): Option[VariableSymbol] = {
//      // Var exists in current class
//      if (members.contains(n))
//        members.get(n)
//      else {
//        //Look for Var in parent tree
//        var upperParent = parent
//        var continue: Boolean = true
//        var matchingMember: Option[VariableSymbol] = None
//
//        while (continue && upperParent.isDefined) {
//          if (upperParent.get.members.contains(n)) {
//            continue = false
//            matchingMember = upperParent.get.members.get(n)
//
//          } else
//            upperParent = upperParent.get.parent
//        }
//        matchingMember
//      }
//    }
//  }
//
//  class MethodSymbol(
//      val name: String,
//      val classSymbol: ClassSymbol,
//      val tpe: Type
//  ) extends Symbol {
//    var params = Map[String, VariableSymbol]()
//    var members = Map[String, VariableSymbol]()
//    var argTpeList: List[Type] = Nil
//    var overridden: Option[MethodSymbol] = None
//
//    def lookupVar(n: String): Option[VariableSymbol] = {
//      if (params.contains(n)) {
//        params.get(n)
//      } else if (members.contains(n)) {
//        members.get(n)
//      } else {
//        None
//      }
//    }
//  }
//
//  class VariableSymbol(val name: String, val tpe: Type) extends Symbol
//
//}
