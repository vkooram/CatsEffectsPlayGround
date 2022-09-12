name := "CatsEffects"

version := "0.1"

scalaVersion := "2.13.8"

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.5.3" withSources() withJavadoc()

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-language:higherKinds")
  //"-Ypartial-unification")