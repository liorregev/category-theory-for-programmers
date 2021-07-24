name := "category_theory"

version := "0.1"

scalaVersion := "2.13.6"

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.0" cross CrossVersion.full)

libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"
