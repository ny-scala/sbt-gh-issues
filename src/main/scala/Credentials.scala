package gh

/** convenience tuple producer for (<gh-user>, <gh-password>).
 *  uses an external .gh file located in user home dir to store credentials */
object LocalGhCreds {
  import sbt._

  def apply(log: Logger) =
    try {
      val p = new java.util.Properties()
      FileUtilities.readStream((Path.userHome / ".gh").asFile, log) { stm =>
        p.load(stm)
        None
      }
      (p.getProperty("username"), p.getProperty("password"))
    } catch { case _ => error("missing file %s" format (Path.userHome / ".gh")) }
}
