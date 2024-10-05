name := "punkt0"

version := "1.0"

scalaVersion := "2.13.14"

scalacOptions ++= Seq("-deprecation", "-unchecked")

addCommandAlias("lint", "scalafmtSbt; scalafmtAll; scalafixAll")
addCommandAlias(
  "lintCheck",
  "scalafmtSbtCheck; scalafmtCheckAll; scalafixAll --check"
)
