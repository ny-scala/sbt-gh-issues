package gh

object Issues {
  import dispatch.liftjson.Js._
  import net.liftweb.json.JsonAST._

  def many(js: JValue) = for(JArray(issue) <- js \ "issues") yield issue
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
  val labels = 'labels ? ary
}

object Labels {
  import dispatch.liftjson.Js._
  import net.liftweb.json.JsonAST._

  def many(js: JValue) = for(JArray(value) <- js \ "labels"; JString(label) <- value) yield label
}

object Comments {
  import dispatch.liftjson.Js._
  import net.liftweb.json.JsonAST._

  def many(js: JValue) = for(JArray(comment) <- js \ "comments") yield comment
  def one(js: JValue) = for (JField("comment", field) <- js) yield field

  val gravatar = 'gravatar_id ? str
  val createdAt = 'created_at ? str
  val body = 'body ? str
  val updatedAt = 'updated_at ? str
  val id = 'id ? int
  val user = 'user ? str
}

case class Issue(user: String, gravatar: String, updatedAt: String, votes: BigInt, number: BigInt,
               position: Double, title: String, body: String, state: String,
               createdAt: String, labels: List[String])

case class Comment(id: BigInt, user: String, gravatar: String, body: String, createdAt: String, updatedAt: String)

trait LabelTasks extends sbt.Project with IssuesApi with ColorizedLogging {
  import net.liftweb.json.JsonAST._

  implicit def manyLabels[String](js: JValue) =
    for(l <- Labels.many(js)) yield l

  lazy val ghLabels = task {
    labels {
      println("Labels for %s/%s" format(ghUser, ghRepo))
      (_: List[String]).foreach(labelListing)
    }
    None
  } describedAs("Lists Labels associated with Github repo %s/%s" format(ghUser, ghRepo))

  lazy val ghAddLabel = task { _ match {
    case Array(label, num) => try {
      task {
        addLabel(label, num.toLong) { labels: List[String] =>
          println("""Added label "%s" to Issue %s""" format(label, num))
        }
        None
      }
    } catch { case _ => task {  Some("invalid arguments label: %s, num: %s" format(label, num)) } }
    case _ => task { Some("""usage: gh-add-label "<label>" <num>""") }
  } } describedAs("Adds a Label to a Github Issue")

  lazy val ghRemoveLabel = task { _ match {
    case Array(label, num) => try {
      task {
        removeLabel(label, num.toLong) { labels: List[String] =>
          println("""Removed label "%s" from Issue %s""" format(label, num))
        }
        None
      }
    } catch { case _ => task {  Some("invalid arguments label: %s, num:%s" format(label, num)) } }
    case _ => task { Some("usage: gh-remove-label <label> <num>") }
  } } describedAs("Removes a Label from a gh issue")

}

trait IssueTasks extends sbt.Project with IssuesApi with ColorizedLogging {
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
        labels   <- Some((for(JString(label) <- Issues.labels(f)) yield label): List[String])
      } yield {
        Issue(user, grav, updated, votes, number, position, title, body, state, created, labels)
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
        labels   <- Some((for(JString(label) <- Issues.labels(f)) yield label): List[String])
      } yield {
        Issue(user, grav, updated, votes, number, position, title, body, state, created, labels)
      } } catch { case dispatch.StatusCode(c,_) => Nil }

  lazy val ghIssue = task { _ match {
      case Array(num) => issue(num.toLong) { (_: Option[Issue]) match {
        case Some(is) => task {
          issueDetail(is)
          None
        }
        case _ => task { Some("This project has no issue %s" format num) }
      } }
      case _ => task { Some("usage: gh-issue <num>") }
    }
  } describedAs("Shows a Github Issue by number")

  lazy val ghIssues = task {
    issues { (_: List[Issue]) match {
      case Nil => println("This project has no issues (at least no documented issues)")
      case l =>
        println("Open issues")
      for(is <- l) issueListing(is)
    } }
    None
  } describedAs("Lists open github issues")

  lazy val ghClosedIssues = task {
    closedIssues { (_: List[Issue]) match {
      case Nil => println("This project has no closed issues.")
      case l =>
        println("Closed issues")
        for(is <- l) issueListing(is)
    } }
    None
  } describedAs("Lists closed Github Issues")

  lazy val ghSearchOpenIssues = task { _ match {
    case Array() => task { Some("usage: gh-search-open-issues 'terms to search for'") }
    case terms => task {
      searchOpen(terms.mkString(" ")) { (_: List[Issue]) match {
        case Nil => println("no open issues with the terms %s" format terms.mkString(" "))
        case l =>
          println("%s search results" format l.size)
          for(is <- l) issueListing(is)
      } }
      None
    }
  } } describedAs("Search for open Github Issues by terms")

  lazy val ghSearchClosedIssues = task { _ match {
    case Array() => task { Some("usage: gh-search-closed-issues 'terms to search for'") }
    case terms => task {
      searchClosed(terms.mkString(" ")) { (_: List[Issue]) match {
        case Nil => println("no closed issues with the terms %s" format terms.mkString(" "))
        case l =>
          println("%s search results" format l.size)
          for(is <- l) issueListing(is)
      } }
      None
    }
  } } describedAs("Search for closed Github Issues by terms")

  lazy val ghOpen = task { _ match {
    case Array(title, desc) =>
      openIssue(title, desc) { (_: Option[Issue]) match {
        case Some(is) => task {
          println("""Opened issue %s "%s" as @%s""" format(is.number, is.title, is.user))
          None
        }
        case _ => task { Some("error creating issue") }
      } }
    case _ => task { Some("""usage: gh-open "<title>" "<description>" """) }
  } } describedAs("Opens a new Github Issue")

  lazy val ghClose = task { _ match {
    case Array(num) =>
      closeIssue(num.toLong) { (_: Option[Issue]) match {
        case Some(is) => task {
          println("""Closed issue %s "%s" as @%s""" format(is.number, is.title, is.user))
          None
        }
        case _ => task { Some("error creating issue") }
      } }
    case _ => task { Some("usage: gh-close <num>") }
  } } describedAs("Closes a Github Issue")
}

trait CommentTasks extends sbt.Project with IssuesApi with ColorizedLogging {
  import net.liftweb.json.JsonAST._

  implicit def oneComment[Issue](js: JValue) =
    try {
      (for {
        f        <- Comments.one(js)
        id        <- Comments.id(f)
        user      <- Comments.user(f)
        gravatar  <- Comments.gravatar(f)
        body      <- Comments.body(f)
        createdAt <- Comments.createdAt(f)
        updatedAt <- Comments.updatedAt(f)
      } yield {
        Comment(id, user, gravatar, body, createdAt, updatedAt)
      }).headOption
    } catch { case dispatch.StatusCode(c, _) => None }

  implicit def manyComments[Comment](js: JValue) =
   try {
     for {
       c         <- Comments.many(js)
       f         <- c
       id        <- Comments.id(f)
       user      <- Comments.user(f)
       gravatar  <- Comments.gravatar(f)
       body      <- Comments.body(f)
       createdAt <- Comments.createdAt(f)
       updatedAt <- Comments.updatedAt(f)
     } yield {
       Comment(id, user, gravatar, body, createdAt, updatedAt)
     }
   } catch { case dispatch.StatusCode(c, _) => Nil }

  lazy val ghComments = task { _ match {
    case Array() => task { Some("usage: gh-comments <num>") }
    case Array(num) => try {
      task {
        println("Comments on issue %s" format num)
        comments(num.toLong) { (_: List[Comment]) match {
          case Nil => println("There were no comments on this issue")
          case l => for (c <-l) commentListing(c)
        } }
        None
      } } catch { case _ => task { Some("invalid arguments %s" format num) } }
  } } describedAs("Lists Comments on a Github Issue")

  lazy val ghComment = task { _ match {
    case Array() => task { Some("""usage: gh-comment <num> ""comment"" """) }
    case Array(num, comm) => try {
      task {
        comment(num.toLong, comm) { (_: Option[Comment]) match {
          case Some(c) => println("""Posted comment "%s" on issue %s as @%s""" format(c.body, num, c.user))
          case _ => println("Comment not posted")
        } }
        None
      } } catch { case _ => task { Some("invalid arguments %s %s" format(num, comm)) } }
  } } describedAs("Posts a new Comment on a Github Issue")

}

/** Mixin for github.com issue tracking and labeling */
trait Issues extends IssueTasks with LabelTasks with CommentTasks

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
    try {
      http(github / "show" / ghUser / ghRepo / num.toString ># { js =>
        f(one(js))
      })
    } catch { case StatusCode(c, _) => f(None) }

  def issues[A, B](f: List[A] => B)(implicit many: Many[A]) =
    try {
      http(github / "list" / ghUser / ghRepo / "open" ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }

  def closedIssues[A, B](f: List[A] => B)(implicit many: Many[A]) =
    try {
      http(github / "list" / ghUser / ghRepo / "closed" ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }

  def searchOpen[A, B](term: String)(f: List[A] => B)(implicit many: Many[A]) =
    try {
      http(github / "search" / ghUser / ghRepo / "open" / URLEncoder.encode(term, "utf-8") ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }

  def searchClosed[A, B](term: String)(f: List[A] => B)(implicit many: Many[A]) =
    try {
      http(github / "search" / ghUser / ghRepo / "closed" / URLEncoder.encode(term, "utf-8") ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }

  def openIssue[A, B](title: String, body: String)(f: Option[A] => B)(implicit one: One[A]) = {
    val (user, pass) = auth
    try {
      http(github.POST.as_!(user, pass) / "open" / ghUser / ghRepo << Map(
        "title" -> title, "body" -> body
      ) ># { js =>
        f(one(js))
      })
    } catch { case StatusCode(c, _) => f(None) }
  }

  def closeIssue[A, B](num: Long)(f: Option[A] => B)(implicit one: One[A]) = {
    val (user, pass) = auth
    try {
      http(github.POST.as_!(user, pass) / "close" / ghUser / ghRepo / num.toString ># { js =>
        f(one(js))
      })
    } catch { case StatusCode(c, _) => f(None) }
  }

  def labels[A, B](f: List[A] => B)(implicit many: Many[A]): B =
    try {
      http(github / "labels" / ghUser / ghRepo ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }

  def addLabel[A, B](label: String, num: Long)(f: List[A] => B)(implicit many: Many[A]): B = {
    val (user, pass) = auth
    try {
      http(github.POST.as_!(user, pass) / "label" / "add" / ghUser / ghRepo / label / num.toString ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }
  }

  def removeLabel[A, B](label: String, num: Long)(f: List[A] => B)(implicit many: Many[A]): B = {
    val (user, pass) = auth
    try {
      http(github.POST.as_!(user, pass) / "label" / "remove" / ghUser / ghRepo / label / num.toString ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }
  }

  def comments[A, B](num: Long)(f: List[A] => B)(implicit many: Many[A]): B =
    try {
      http(github  / "comments" / ghUser / ghRepo / num.toString ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }

  def comment[A, B](id: Long, comment: String)(f: Option[A] => B)(implicit one: One[A]): B = {
    val (user, pass) = auth
    try {
      http(github.POST.as_!(user, pass) / "comment" / ghUser / ghRepo / id.toString << Map(
        "comment" -> comment
      ) ># { js =>
        f(one(js))
      })
    } catch { case StatusCode(c, _) => f(None) }
  }
}
