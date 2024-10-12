// Formatting in scala
// See .scalafmt.conf for configuration details.
// Formatting takes place before the project is compiled.
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")

// Refactoring and linting tool for Scala
// sbt> scalafix
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.13.0")

// Apply sane default Scala compile options
addSbtPlugin("org.typelevel" % "sbt-tpolecat" % "0.5.2")
