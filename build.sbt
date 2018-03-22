organization := "fr.iscpif"
name := "mgo-bench"

scalaVersion := "2.12.4"
crossScalaVersions := Seq("2.12.4")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full)
scalacOptions += "-Xplugin-require:macroparadise"

resolvers += Resolver.sonatypeRepo("public")
resolvers += Resolver.sonatypeRepo("staging")
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "fr.iscpif" %% "mgo" % "3.8-SNAPSHOT"

javaOptions += "-Djava.library.path=/home/raimbault/ComplexSystems/MGO/mgo-benchmark"

fork := true