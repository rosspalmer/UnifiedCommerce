ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "UnifiedCommerce",
    idePackagePrefix := Some("com.palmer.data")
  )

libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"
