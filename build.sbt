organization := "fr.iscpif"
name := "mgo-bench"

//scalaVersion := "2.12.4"
scalaVersion := "3.3.0"
//crossScalaVersions := Seq("2.10.4")

//addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
//addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full)
//scalacOptions += "-Xplugin-require:macroparadise"

resolvers += Resolver.sonatypeRepo("public")
resolvers += Resolver.sonatypeRepo("staging")
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.mavenCentral

//resolvers += "jitpack" at "https://jitpack.io"
//resolvers += "Jzy3d Maven Release Repository" at "http://maven.jzy3d.org/releases"
//resolvers += "BeDataDriven" at "https://nexus.bedatadriven.com/content/groups/public"

//libraryDependencies += ("fr.iscpif" % "mgo_2.12" % "3.32").exclude("org.typelevel","cats-kernel_2.12")
libraryDependencies += "org.openmole" % "mgo_3" % "3.55"
libraryDependencies += "org.scalanlp" % "breeze_3" % "2.1.0"
//libraryDependencies += ("org" % "akka-multiswarm_2.12" % "0.1-SNAPSHOT").exclude("org.scalanlp", "breeze_2.12") // locally published lib
// ! not compatible with scala3
//libraryDependencies += "net.cilib" % "cilib-core_2.12" % "2.0.1-30-g5ca4090"
//libraryDependencies += "net.cilib" % "cilib-exec_2.12" % "2.0.1-30-g5ca4090"
//libraryDependencies += "net.cilib" % "cilib-ga_2.12" % "2.0.1-30-g5ca4090"
//libraryDependencies += "net.cilib" % "cilib-pso_2.12" % "2.0.1-30-g5ca4090"
//libraryDependencies += "net.cilib" % "benchmarks_2.12" % "0.1.1"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"

// ascii plot
libraryDependencies += "org.sameersingh.scalaplot" % "scalaplot" % "0.0.4"


//libraryDependencies += "com.chuusai" % "shapeless_2.10" % "2.10.4"
//libraryDependencies += "com.chuusai" % "shapeless_2.10" % "1.2.4"
//libraryDependencies += "org.scalanlp" % "breeze_2.10" % "0.12" exclude("com.chuusai", "shapeless_2.10.4")
//libraryDependencies += "org.jzy3d" % "jzy3d-api" % "0.9.1"
//libraryDependencies += "org.renjin" % "renjin-script-engine" % "0.9.2643"
//libraryDependencies += "com.github.transcendent-ai-labs.DynaML" % "dynaml-core_2.11" % "v1.5.3-beta.2"
//libraryDependencies += "net.cilib" % "cilib_2.11" % "2.0.0-M1"

//javaOptions += "-Djava.library.path="+sys.env.get("CS_HOME")+"/NoisyEA/Models/mgo-benchmark/lib/coco/build"
//javaOptions += "-Djava.library.path=/Users/juste/ComplexSystems/NoisyEA/Models/mgo-benchmark/lib/coco/build"
javaOptions += "-Djava.library.path=lib/coco/build"
//scalacOptions += "-cp ."

//mainClass in (Compile, run) := Some("mgobench.Run")
mainClass in (Compile, run) := Some("mgobench.Launcher")

/*
val parrun = taskKey[Unit]("All")
parrun := {
  (run in Runtime).inputTaskValue
}
*/

fork := true
