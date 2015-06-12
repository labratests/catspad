import sbt._
import sbt.Keys._
import AssemblyKeys._
import bintray.Plugin.bintraySettings
import bintray.Keys._
import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "buildinfo"
  )

name := "catspad"

organization := "es.weso"

version := "0.0.1"

scalaVersion := "2.11.6"

val snapshots = "Sonatype Snapshots"  at "https://oss.sonatype.org/content/repositories/snapshots"

val algebraVersion = "0.2.0-SNAPSHOT"
val catsVersion    = "0.1.0-SNAPSHOT"

val algebra    = "org.spire-math" %% "algebra" % algebraVersion
val algebraStd = "org.spire-math" %% "algebra-std" % algebraVersion

val cats       = "org.spire-math" %% "cats-core" % catsVersion
val catsStd    = "org.spire-math" %% "cats-std" % catsVersion

scalaVersion := "2.11.6"

libraryDependencies ++=
  Seq(
    algebra, algebraStd,
    cats, catsStd
  )

resolvers += snapshots

libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
  , "commons-configuration" % "commons-configuration" % "1.7"
  , "org.rogach" %% "scallop" % "0.9.5" 
  , "com.typesafe" % "config" % "1.0.1"
  , "jline" % "jline" % "2.12.1"
  , "org.scala-lang" % "scala-compiler" % scalaVersion.value  
  , "org.apache.jena" % "jena-arq" % "2.13.0" excludeAll(ExclusionRule(organization = "org.slf4j"))
  , "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"
  ,	"org.scalacheck" %% "scalacheck" % "1.11.4" % "test"
  , "com.github.axel22" %% "scalameter" % "0.5-M2" % "test"
  , "es.weso" % "wesin_2.11" % "0.3.1" excludeAll(ExclusionRule(organization = "org.slf4j"))
  , "org.slf4j" % "slf4j-simple" % "1.6.4"
//  , "org.w3" % "banana-rdf_2.11" % "0.8.1"
)

autoCompilerPlugins := true

// For performance test...

lazy val PerfTest = config("perf").extend(Test)

testFrameworks in PerfTest += new TestFramework("org.scalameter.ScalaMeterFramework")

logBuffered in PerfTest := false

parallelExecution in PerfTest := false

seq(bintraySettings:_*)

deploymentSettings

publishMavenStyle := true

net.virtualvoid.sbt.graph.Plugin.graphSettings

// publish <<= publish.dependsOn(publish in config("universal"))

packageArchetype.java_application


resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

//resolvers += "Sonatype OSS Snapshots" at
//  "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

// Eclipse

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed

// Publishing settings to BinTray

bintraySettings

repository in bintray := "weso-releases"

bintrayOrganization in bintray := Some("weso")

licenses += ("MPL-2.0", url("http://opensource.org/licenses/MPL-2.0"))

resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

// Scalariform (default disables formatting)

defaultScalariformSettings

// to enable formatting, use: 
// scalariformSettings
