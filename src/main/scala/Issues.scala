package gh

object Issues {
  import dispatch.liftjson.Js._
  import net.liftweb.json.JsonAST._

  def many(js: JValue) = for(JArray(issue) <- js \ "issues") yield issue //'issues ? ary
  def one(js: JValue) = for (JField("issue", field) <- js) yield field

  val user = 'user ? str
  val gravatar = 'gravatar_id ? str
  val updatedAt = 'updated_at ? str
  val votes = 'votes ? int
  val number = 'number ? int
  val position = 'position ? double
  val title = 'title ? str
  val body = 'body ? str
  val state = 'state ? str
  val createdAt = 'created_at ? str
  val closedAt = 'closed_at ? str
}

object Labels {
  import dispatch.liftjson.Js._
  import net.liftweb.json.JsonAST._
  def many(js: JValue) = for(JArray(value) <- js \ "labels"; JString(label) <- value) yield label
}

case class Issue(user: String, gravatar: String, updatedAt: String, votes: BigInt, number: BigInt,
               position: Double, title: String, body: String, state: String,
               createdAt: String)

/** convenience tuple producer. uses an external .gh file to store credentials */
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

trait LabelTasks extends sbt.Project with IssuesApi {
  import net.liftweb.json.JsonAST._
  import java.lang.{String => JString}
  implicit def manyLabels[JString](js: JValue): List[String] =
    for(l <- Labels.many(js)) yield l.toString //?

  lazy val ghLabels = task {
    labels {
      (_: List[String]).foreach(println)
    }
    None
  }

  lazy val ghAddLabel = task { _ match {
    case Array(label, num) => try {
      task {
        println("Labels")
        addLabel(label, num.toLong) {
          (_: List[String]).foreach(println)
        }
        None
      }
    } catch { case _ => task {  Some("invalid arguments label: %s, num: %s" format(label, num)) } }
    case _ => task { Some("usage: gh-add-label <label> <num>") }
  } } describedAs("Adds a label to a gh issue")

  lazy val ghRemoveLabel = task { _ match {
    case Array(label, num) => try {
      task {
        removeLabel(label, num.toLong) {
          (_: List[String]).foreach(println)
        }
        None
      }
    } catch { case _ => task {  Some("invalid arguments label: %s, num:%s" format(label, num)) } }
    case _ => task { Some("usage: gh-remove-label <label> <num>") }
  } } describedAs("Removes a label from a gh issue")

}

trait IssueTasks extends sbt.Project with IssuesApi {
  import net.liftweb.json.JsonAST._

  implicit def one[Issue](js: JValue) =
    try {
      (for {
        f        <- Issues.one(js)
        grav     <- Issues.gravatar(f)
        position <- Issues.position(f)
        votes    <- Issues.votes(f)
        created  <- Issues.createdAt(f)
        body     <- Issues.body(f)
        title    <- Issues.title(f)
        updated  <- Issues.updatedAt(f)
        state    <- Issues.state(f)
        user     <- Issues.user(f)
        number   <- Issues.number(f)
      } yield {
        Issue(user, grav, updated, votes, number, position, title, body, state, created)
      }).headOption
    } catch { case dispatch.StatusCode(c, _) => None }

  implicit def many[Issue](js: JValue) =
    try {
      for {
        is       <- Issues.many(js)
        f        <- is
        grav     <- Issues.gravatar(f)
        position <- Issues.position(f)
        votes    <- Issues.votes(f)
        created  <- Issues.createdAt(f)
        body     <- Issues.body(f)
        title    <- Issues.title(f)
        updated  <- Issues.updatedAt(f)
        state    <- Issues.state(f)
        user     <- Issues.user(f)
        number   <- Issues.number(f)
      } yield {
        Issue(user, grav, updated, votes, number, position, title, body, state, created)
      } } catch { case dispatch.StatusCode(c,_) => Nil }

  lazy val ghIssue = task {
    _ match {
      case Array(num) => issue(num.toLong) { (_: Option[Issue]) match {
        case Some(is) => {
          println("%s %s (@%s)\n%s" format(is.number, is.title, is.user, is.body))
          None
        }
        case _ => task { Some("Github knows no issue %s" format num) }
      } }
      case _ => task { Some("usage: gh-issue <num>") }
    }
  } describedAs("Shows a github issue by number")

  lazy val ghIssues = task {
    issues { (_: List[Issue]) match {
      case Nil => println("This project has no issues (at least no documented issues)")
      case l =>
        println("open issues")
        for(is <- l) println("%s %s (@%s)" format(is.number, is.user, is.title))
    } }
    None
  } describedAs("Lists open github issues")

  lazy val ghClosedIssues = task {
    closedIssues { (_: List[Issue]) match {
      case Nil => println("This project has no closed issues.")
      case l =>
        println("closed issues")
        for(is <- l) println("%s %s (@%s)" format(is.number, is.title, is.user))
    } }
    None
  } describedAs("List closed github issues")

  lazy val ghSearchOpenIssues = task { _ match {
    case Array() => task { Some("usage: gh-search-open-issues 'terms to search for'") }
    case terms => task {
      searchOpen(terms.mkString(" ")) { (_: List[Issue]) match {
        case Nil => println("no open issues with the terms %s" format terms.mkString(" "))
        case l =>
          println("%s search results" format l.size)
          for(is <- l) println("%s %s (@%s)" format(is.number, is.title, is.user))
      } }
      None
    }
  } } describedAs("Search for open gh issues by terms")

  lazy val ghSearchClosedIssues = task { _ match {
    case Array() => task { Some("usage: gh-search-closed-issues 'terms to search for'") }
    case terms => task {
      searchClosed(terms.mkString(" ")) { (_: List[Issue]) match {
        case Nil => println("no closed issues with the terms %s" format terms.mkString(" "))
        case l =>
          println("%s search results" format l.size)
        for(is <- l) println("%s %s (@%s)" format(is.number, is.title, is.user))
      } }
      None
    }
  } } describedAs("Search for closed gh issues by terms")

  lazy val ghOpen = task { _ match {
    case Array(title, desc) =>
      openIssue(title, desc) { (_: Option[Issue]) match {
        case Some(is) => task {
          println("created issue %s %s (@%s)" format(is.number, is.title, is.user))
          None
        }
        case _ => task { Some("error creating issue") }
      } }
    case _ => task { Some("usage: gh-open '<title>' '<description>'") }
  } } describedAs("Open gh issue")

  lazy val ghClose = task { _ match {
    case Array(num) =>
      closeIssue(num.toLong) { (_: Option[Issue]) match {
        case Some(is) => task {
          println("closed issue %s %s (@%s)" format(is.number, is.title, is.user))
          None
        }
        case _ => task { Some("error creating issue") }
      } }
    case _ => task { Some("usage: gh-close <num>") }
  } } describedAs("Close gh issue")
}

/** Mixin for github.com issue tracking and labeling */
trait Issues extends IssueTasks with LabelTasks

private [gh] trait IssuesApi {
  import dispatch._
  import dispatch.Http._
  import dispatch.liftjson.Js._
  import net.liftweb.json._
  import net.liftweb.json.JsonAST._
  import java.net.URLEncoder

  type One[A] = JValue => Option[A]
  type Many[A] = JValue => List[A]

  private [gh] val github = :/("github.com") / "api" / "v2" / "json" / "issues"

  private [gh] val http = new Http {
    /** quiet dispatch request logging */
    override def make_logger = new Logger {
      def info(msg: String, items: Any*) = ()
    }
  }

  /** (username, reponame) */
  def ghRepository: (String, String)

  lazy val ghRepo = ghRepository._2

  lazy val ghUser = ghRepository._1

  /** (username, password) */
  def ghCredentials: (String, String)

  lazy val auth = ghCredentials

  def issue[A, B](num: Long)(f: Option[A] => B)(implicit one: One[A]) =
    http(github / "show" / ghUser / ghRepo / num.toString ># { js =>
      f(one(js))
    })

  def issues[A, B](f: List[A] => B)(implicit many: Many[A]) =
    http(github / "list" / ghUser / ghRepo / "open" ># { js =>
      f(many(js))
    })

  def closedIssues[A, B](f: List[A] => B)(implicit many: Many[A]) =
    http(github / "list" / ghUser / ghRepo / "closed" ># { js =>
      f(many(js))
    })

  def searchOpen[A, B](term: String)(f: List[A] => B)(implicit many: Many[A]) =
    http(github / "search" / ghUser / ghRepo / "open" / URLEncoder.encode(term, "utf-8") ># { js =>
      f(many(js))
    })

  def searchClosed[A, B](term: String)(f: List[A] => B)(implicit many: Many[A]) =
    http(github / "search" / ghUser / ghRepo / "closed" / URLEncoder.encode(term, "utf-8") ># { js =>
      f(many(js))
    })

  def openIssue[A, B](title: String, body: String)(f: Option[A] => B)(implicit one: One[A]) = {
    val (user, pass) = auth
    http(github.POST.as_!(user, pass) / "open" / ghUser / ghRepo << Map(
      "title" -> title, "body" -> body
    ) ># { js =>
      f(one(js))
    })
  }

  def closeIssue[A, B](num: Long)(f: Option[A] => B)(implicit one: One[A]) = {
    val (user, pass) = auth
    http(github.POST.as_!(user, pass) / "close" / ghUser / ghRepo / num.toString ># { js =>
      f(one(js))
    })
  }

  def labels[A, B](f: List[A] => B)(implicit many: Many[A]) =
    http(github / "labels" / ghUser / ghRepo ># { js =>
      f(many(js))
     })

  def addLabel[A, B](label: String, num: Long)(f: List[A] => B)(implicit many: Many[A]) = {
    val (user, pass) = auth
    http(github.POST.as_!(user, pass) / "label" / "add" / ghUser / ghRepo / label / num.toString ># { js =>
      f(many(js))
    })
  }

  def removeLabel[A, B](label: String, num: Long)(f: List[A] => B)(implicit many: Many[A]) = {
    val (user, pass) = auth
    http(github.POST.as_!(user, pass) / "label" / "remove" / ghUser / ghRepo / label / num.toString ># { js =>
      f(many(js))
    })
  }
}
