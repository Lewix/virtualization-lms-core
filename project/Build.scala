import sbt._
import java.io.File

object LMSBuild extends Build {

  // -DshowSuppressedErrors=false
  System.setProperty("showSuppressedErrors", "false")

  val mavenLocal = "Maven Local" at "file://"+Path.userHome+"/.m2/repository" // for custom-built scala version

  //val prereleaseScalaTest = "Pre-Release ScalaTest" at "https://scala-webapps.epfl.ch/jenkins/view/2.10.x/job/community-nightly-2.10.0/ws/target/repositories/bccb0f4eb0bf6024577064915da437e3574281cb/"

  val scalaTest = "org.scalatest" %% "scalatest" % "2.0" % "test"

  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.2-RC1")

  lazy val lms = Project("LMS", file("."))
}
