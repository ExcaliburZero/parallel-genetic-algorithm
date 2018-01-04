name := "geneticparallel"

version := "1.0"

scalaVersion := "2.12.1"
//scalaVersion := "2.11.8"
sbtVersion := "0.13.13"

// https://groups.google.com/forum/#!topic/scalafx-users/MzHb19SISHQ
unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/ext/jfxrt.jar"))

resolvers += Opts.resolver.sonatypeSnapshots

// Packages

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.102-R11"

mainClass in Compile := Some("geneticparallel.Main")

// Linting
resolvers += Resolver.sonatypeRepo("snapshots")
addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1-SNAPSHOT")
