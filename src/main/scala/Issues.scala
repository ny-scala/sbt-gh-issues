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

case class Issue(user: String, gravatar: String, updatedAt: String, votes: BigInt, number: BigInt,
               position: Double, title: String, body: String, state: String,
               createdAt: String)

trait Issues extends sbt.Project with IssuesApi {

  lazy val ghIssue = task {
    _ match {
      case Array(num) => issue(num.toLong) { _ match {
        case Some(is) => task {
          println("%s %s (@%s)\n%s" format(is.number, is.title, is.user, is.body))
          None
        }
        case _ => task { Some("Github knows no issue %s" format num) }
      } }
      case _ => task { Some("usage: gh-issue <num>") }
    }
  } describedAs("Shows a github issue by number")

  lazy val ghIssues = task {
    issues { _ match {
      case Nil => println("This project has no issues. At least no documented issues")
      case l =>
        println("open issues")
        for(is <- l) println("%s %s (@%s)" format(is.number, is.user, is.title))
    } }
    None
  } describedAs("Lists open github issues")

  lazy val ghClosedIssues = task {
     closedIssues { _ match {
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
      searchOpen(terms.mkString(" ")) { _ match {
        case Nil => println("no open issues with the terms %s" format terms.mkString(" "))
        case l =>
          println("%s search results" format l.size)
          for(is <- l) println("%s %s (@%s)" format(is.number, is.title, is.user))
      } }
      None
    }
  } } describedAs("Search for gh issues by terms")

  lazy val ghSearchClosedIssues = task { _ match {
    case Array() => task { Some("usage: gh-search-closed-issues 'terms to search for'") }
    case terms => task {
      searchClosed(terms.mkString(" ")) { _ match {
        case Nil => println("no closed issues with the terms %s" format terms.mkString(" "))
        case l =>
          println("%s search results" format l.size)
          for(is <- l) println("%s %s (@%s)" format(is.number, is.title, is.user))
      } }
      None
    }
  } } describedAs("Search for gh issues by terms")

  lazy val ghOpen = task { _ match {
    case Array(title, desc) =>
      openIssue(title, desc) { _ match {
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
      closeIssue(num.toLong) { _ match {
        case Some(is) => task {
          println("closed issue %s %s (@%s)" format(is.number, is.title, is.user))
          None
        }
        case _ => task { Some("error creating issue") }
      } }
    case _ => task { Some("usage: gh-close <num>") }
  } } describedAs("Close gh issue")

}

private [gh] trait IssuesApi {
  import dispatch._
  import dispatch.Http._
  import dispatch.liftjson.Js._
  import net.liftweb.json._
  import net.liftweb.json.JsonAST._
  import java.net.URLEncoder

  private [gh] val github = :/("github.com") / "api" / "v2" / "json" / "issues"

  /** (username, reponame) */
  def ghRepository: (String, String)

  lazy val ghRepo = ghRepository._2

  lazy val ghUser = ghRepository._1

  /** (username, password) */
  def ghCredentials: (String, String)

  lazy val auth = ghCredentials

  def issue[T](num: Long)(fn:  Option[Issue] => T) =
    Http(github / "show" / ghUser / ghRepo / num.toString ># { js =>
      fn(oneIssue(js))
    })

  def issues[T](fn: List[Issue] => T) =
    Http(github / "list" / ghUser / ghRepo / "open" ># { js =>
      fn(manyIssues(js))
    })

  def closedIssues[T](f: List[Issue] => T) =
    Http(github / "list" / ghUser / ghRepo / "closed" ># { js =>
      f(manyIssues(js))
    })

  def searchOpen[T](term: String)(f: List[Issue] => T) =
    Http(github / "search" / ghUser / ghRepo / "open" / URLEncoder.encode(term, "utf-8") ># { js =>
      f(manyIssues(js))
    })

  def searchClosed[T](term: String)(f: List[Issue] => T) =
    Http(github / "search" / ghUser / ghRepo / "closed" / URLEncoder.encode(term, "utf-8") ># { js =>
      f(manyIssues(js))
    })

  def openIssue[T](title: String, body: String)(f: Option[Issue] => T) = {
    val (user, pass) = auth
    Http(github.POST.as_!(user, pass) / "open" / ghUser / ghRepo << Map(
      "title" -> title, "body" -> body
    ) ># { js =>
      f(oneIssue(js))
    })
  }

  def closeIssue[T](num: Long)(f: Option[Issue] => T) = {
    val (user, pass) = auth
    Http(github.POST.as_!(user, pass) / "close" / ghUser / ghRepo / num.toString ># { js =>
      f(oneIssue(js))
    })
  }

  def labels[T](f: Any => T) =
    Http(github / "labels" / ghUser / ghRepo ># { js =>
      f(js)
    })

  def addLabel[T](label: String, num: Long)(f: Any => T) = {
    val (user, pass) = auth
    Http(github.POST.as_!(user, pass) / "label" / "add" / ghUser / ghRepo / label / num.toString ># { js =>
      f(js)
    })
  }

  def removeLabel[T](label: String, num: Long)(f: Any => T) = {
    val (user, pass) = auth
    Http(github.POST.as_!(user, pass) / "label" / "remove" / ghUser / ghRepo / label / num.toString ># { js =>
      f(js)
    })
  }

  def oneIssue(js: JValue): Option[Issue] = try {
    (for {
      f <- Issues.one(js)
      grav <- Issues.gravatar(f)
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
  } catch { case dispatch.StatusCode(c,_) => None }

 def manyIssues(js: JValue): List[Issue] = try {
   for {
     is <- Issues.many(js)
     f <- is
     grav <- Issues.gravatar(f)
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
}
