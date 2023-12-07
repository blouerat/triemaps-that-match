ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root =
  project
    .in(file("."))
    .settings(
      name := "triemaps",
      idePackagePrefix := Some("triemaps")
    )

lazy val docs = project       // new documentation project
  .in(file("myproject-docs")) // important: it must not be docs/
  .settings(
     mdocVariables := Map(
       "VERSION" -> version.value
     )
  )
  .dependsOn(root)
  .enablePlugins(MdocPlugin)
