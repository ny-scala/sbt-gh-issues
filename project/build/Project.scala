import sbt._
class Project(info: ProjectInfo) extends DefaultProject(info) {
  val dbjs = "net.databinder" %% "dispatch-lift-json" % "0.7.8"
}
