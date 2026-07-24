/*scalaVersion := "2.12.13"

scalacOptions ++= Seq(
  "-feature",
  "-language:reflectiveCalls",
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases")
)

// Chisel 3.5
addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.0" cross CrossVersion.full)
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.5.0"
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.5.0"*/
// import scalanative.sbt._
scalaVersion := "2.13.12"

// scalaVersion := "2.12.12"

scalacOptions := Seq("-deprecation", "-Xsource:2.13")

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)
parallelExecution in Compile := true
// Chisel 3.4
// libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.4.3"
// libraryDependencies += "edu.berkeley.cs" %% "chisel-iotesters" % "1.5.3"
// libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.3.3"

addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.6.1" cross CrossVersion.full)
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.6.1"
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.6.1"
libraryDependencies += "org.apache.tika" % "tika-core" % "2.9.1"
// libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "5.0-SNAPSHOT"

// addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "5.3.0" cross CrossVersion.full)
// libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "5.3.0"
// libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.5.6"
javaOptions += "-Xmx8G"

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", "versions", "9", "module-info.class") => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}
// nativeLinkOptions ++= Seq("-static-libgcc", "-static-libstdc++")
lazy val root = (project in file("."))
  .settings(
    name := "demo",
    assembly / mainClass := Some("BackendCodeEmitter.TestMapParse"),
    assembly / assemblyJarName := "demo.jar"
  )

// lazy val root = (project in file("."))
//   .settings(
//     name := "demosim",
//     assembly / mainClass := Some("mycgratemporal.Main"),
//     assembly / assemblyJarName := "demosim.jar"
//   )
