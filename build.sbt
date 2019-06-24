scalaVersion := "2.12.3"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "Maven" at "http://maven.daumcorp.com/content/groups/dk-contentshub-group"
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scalactic" %% "scalactic" % "3.0.5" % "test",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)