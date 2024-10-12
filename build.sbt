name := "punkt0"

version := "1.0"

scalaVersion := "2.13.14"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

run / fork := true

addCommandAlias("lint", "scalafmtSbt; scalafmtAll; scalafixAll")
addCommandAlias(
  "lintCheck",
  "scalafmtSbtCheck; scalafmtCheckAll; scalafixAll --check"
)
