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

trait Issues {
  import dispatch._
  import dispatch.Http._
  import dispatch.liftjson.Js._
  import net.liftweb.json._
  import net.liftweb.json.JsonAST._
  import java.net.URLEncoder

  val github = :/("github.com") / "api" / "v2" / "json" / "issues"

  def ghUser: String

  def ghRepo: String

  def ghCredentials: (String, String)

  lazy val auth = ghCredentials

  def issue[T](num: Long)(fn:  Option[Issue] => T) =
    Http(github / "show" / ghUser / ghRepo / num.toString ># { js =>
      fn(oneIssue(js).headOption)
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
    Http(github / "search" / ghUser / ghRepo / "closed" / URLEncoder.encode(term, "utf-8") ># { js =>
      f(manyIssues(js))
    })

  def searchClosed[T](term: String)(f: List[Issue] => T) =
    Http(github / "search" / ghUser / ghRepo / "closed" / URLEncoder.encode(term, "utf-8") ># { js =>
      f(manyIssues(js))
    })

  def openIssue[T](title: String, body: String)(f: Any => T) = {
    val (user, pass) = auth
    Http(github.POST.as_!(user, pass) / "open" / ghUser / ghRepo << Map(
      "title" -> title, "body" -> body
    ) ># { js =>
      f(js)
    })
  }

  def closeIssue[T](num: Long)(f: Any => T) = {
    val (user, pass) = auth
    Http(github.POST.as_!(user, pass) / "close" / ghUser / ghRepo / num.toString ># { js =>
      f(js)
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

  def oneIssue(js: JValue) =
    for {
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
      }

 def manyIssues(js: JValue) =
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
   }
}
