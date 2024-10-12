package io.punkt0.analyzer

import io.punkt0.PositionT

object Symbols:

    trait Type

    trait Symbolic[S <: Symbol]

    abstract class Symbol extends PositionT

    case class ClassSymbol(name: String) extends Symbol

    case class MethodSymbol(
        name: String,
        classSymbol: ClassSymbol,
        tpe: Type,
    ) extends Symbol

    case class VariableSymbol(name: String, tpe: Type) extends Symbol
