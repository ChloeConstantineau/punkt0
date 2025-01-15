package io.punkt0.analyzer

import io.punkt0.PositionT

object Symbols:

    trait Type

    trait Symbolic[S <: Symbol]

    abstract class Symbol extends PositionT

    case class GlobalScope(mainClass: ClassSymbol, classes: Map[String, ClassSymbol] = Map.empty):
        def lookupClass(n: String): Option[ClassSymbol] = classes.get(n)

    case class ClassSymbol(
        name: String,
        parent: Option[ClassSymbol] = None,
        methods: Map[String, MethodSymbol] = Map.empty,
        members: Map[String, VariableSymbol] = Map.empty,
    ) extends Symbol:
        private def lookupMethod(n: String): Option[MethodSymbol] =
          methods.get(n).orElse(lookupMethodInParent(n))
        private def lookupVar(n: String): Option[VariableSymbol]  =
          members.get(n).orElse(lookupVarInParent(n))

        // TODO Test if it recurses as expected
        private def lookupMethodInParent(n: String): Option[MethodSymbol] =
          parent.fold(None)(p => p.lookupMethod(n))
        private def lookupVarInParent(n: String): Option[VariableSymbol]  =
          parent.fold(None)(p => p.lookupVar(n))

    case class MethodSymbol(
        name: String,
        `class`: ClassSymbol,
        tpe: Type,
        params: Map[String, VariableSymbol] = Map.empty,
        members: Map[String, VariableSymbol] = Map.empty,
        argTpeList: List[Type] = Nil,
        overridden: Option[MethodSymbol] = None,
    ) extends Symbol:
        def lookupVar(n: String): Option[VariableSymbol] = params.get(n).orElse(members.get(n))

    case class VariableSymbol(name: String, tpe: Type) extends Symbol
