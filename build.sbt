import org.typelevel.sbt.tpolecat.{CiMode, DevMode}
import org.typelevel.scalacoptions.ScalacOptions

// *****************************************************************************
// Global settings
// *****************************************************************************

ThisBuild / version             := "1.0"
ThisBuild / scalaVersion        := "3.5.1"
ThisBuild / scalacOptions ++= Seq("-deprecation", "-unchecked")
ThisBuild / semanticdbEnabled   := true // For scalafix
ThisBuild / semanticdbVersion   := scalafixSemanticdb.revision
ThisBuild / tpolecatDevModeOptions ++= additionalScalacOptions
ThisBuild / tpolecatOptionsMode := {
  if (insideCI.value) CiMode else DevMode
}

addCommandAlias("lint", "scalafmtSbt; scalafmtAll; scalafixAll")
addCommandAlias(
  "lintCheck",
  "scalafmtSbtCheck; scalafmtCheckAll; scalafixAll --check",
)

// *****************************************************************************
// Project
// *****************************************************************************

lazy val `punkt0-root` =
  project
    .in(file("."))
    .settings(
      run / fork                             := true,
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",
    )

// *****************************************************************************
// Options
// *****************************************************************************
lazy val additionalScalacOptions = Set(
  ScalacOptions.release("21"),
  ScalacOptions.source3,
  ScalacOptions.verboseTypeDiffs,
  ScalacOptions.verboseImplicits,
  ScalacOptions.warnNonUnitStatement,
  // With these two flags, we enforce optional braces around template bodies and method arguments. scalafmt `rewrite.scala3.removeOptionalBraces` isn't enough.
  // See: https://dotty.epfl.ch/docs/reference/other-new-features/indentation.html#settings-and-rewrites-1
  ScalacOptions.other("-rewrite", _ => true),
  ScalacOptions.other("-indent", _ => true),
)
