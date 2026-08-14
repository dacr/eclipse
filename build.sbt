ThisBuild / organization     := "fr.janalyse"
ThisBuild / organizationName := "JAnalyse"
ThisBuild / homepage         := Some(url("https://github.com/dacr/eclipse"))
ThisBuild / licenses         := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers       := List(
  Developer("dacr", "David Crosson", "crosson.david@gmail.com", url("https://github.com/dacr"))
)
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.8.4"

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked"
)

lazy val munit = "org.scalameta" %% "munit" % "1.3.5" % Test

// a forked jvm starts in the base directory of its own project, and prints in whatever encoding
// the locale happens to define : both are pinned here so that a relative path given on the command
// line means what it looks like, and so that degrees stay degrees
lazy val utf8Options = Seq("-Dfile.encoding=UTF-8", "-Dstdout.encoding=UTF-8", "-Dstderr.encoding=UTF-8")

lazy val commonSettings = Seq(
  libraryDependencies += munit,
  Test / fork          := true,
  Test / baseDirectory := (ThisBuild / baseDirectory).value,
  Test / javaOptions ++= Seq("-Xmx2g") ++ utf8Options
)

// Image processing toolbox, packaged under the sotohp namespace : this module is
// a local copy of sotohp's imaging code, enriched with additions designed to be
// pushed back upstream. Everything here must stay free of any eclipse specific concept.
lazy val imaging = project
  .in(file("modules/imaging"))
  .settings(commonSettings)
  .settings(
    name := "eclipse-imaging"
  )

// Domain model : angles, coordinates, frames, discs, ...
lazy val model = project
  .in(file("modules/model"))
  .settings(commonSettings)
  .settings(
    name := "eclipse-model"
  )

// Astronomy : where was the sun, seen from here, at that very instant ?
lazy val astro = project
  .in(file("modules/astro"))
  .dependsOn(model)
  .settings(commonSettings)
  .settings(
    name := "eclipse-astro"
  )

// From a RAW/JPEG file to a measured frame : metadata, sun disc, obscuration.
lazy val frames = project
  .in(file("modules/frames"))
  .dependsOn(model, imaging, astro)
  .settings(commonSettings)
  .settings(
    name := "eclipse-frames",
    libraryDependencies += "com.drewnoakes" % "metadata-extractor" % "2.21.0"
  )

// Frame selection, layouts and composite rendering.
lazy val composer = project
  .in(file("modules/composer"))
  .dependsOn(frames)
  .settings(commonSettings)
  .settings(
    name := "eclipse-composer"
  )

lazy val cli = project
  .in(file("modules/cli"))
  .dependsOn(composer)
  .settings(commonSettings)
  .settings(
    name          := "eclipse-cli",
    Compile / mainClass := Some("fr.janalyse.eclipse.cli.Main"),
    run / fork    := true,
    run / javaOptions ++= Seq("-Xmx6g") ++ utf8Options,
    // a forked run would otherwise start in modules/cli, and every relative path given on the
    // command line - photos-eclipse being the obvious one - would silently point nowhere
    run / baseDirectory := (ThisBuild / baseDirectory).value,
    run / connectInput  := true
  )

lazy val root = project
  .in(file("."))
  .aggregate(imaging, model, astro, frames, composer, cli)
  .settings(
    name           := "eclipse",
    publish / skip := true
  )
