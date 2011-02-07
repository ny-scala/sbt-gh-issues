# sbt gh issues

[sbt]() plugin for [github](http://github.com) [issues](http://develop.github.com/p/issues.html)

## usage

This plugin actions several Github Issues related tasks to your project.

These tasks are broken down into three traits

### gh.IssueTasks

    gh-issues # lists open issues
    gh-closed-issues # lists closed issues
    gh-issue <num> # shows the details of a gh issue by number
    gh-open <title> <desc> # opens a new gh issue
    gh-close <num> # closes a gh issue by number
    gh-search-closed-issues <term> # Search for closed gh issues by terms
    gh-search-open-issues <term> # Search for open gh issues by terms

### gh.LabelTasks

    gh-labels # Lists a labels for the current repo
    gh-add-label <label> <num> # Adds a label to a gh issue
    gh-remove-label <label> <num> # Removes a label from a gh issue

### gh. CommentTasks

     gh-comments <num> # lists all comments on a gh issue
     gh-comment <num> <comment> # adds a comment on a gh issue

You for simplicity you can can just mix in `gh.Issues` for all three.

    > cat project/plugins/plugins.scala
    import sbt._

    class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
      val lessis = "less is repo" at "http://repo.lessis.me"
      val ghIssues = "me.lessis" % "gh-sbt-plugin" % "0.0.1"
    }

    > cat project/build/project.scala
    import sbt._

    class Project(info: ProjectInfo) extends DefaultProject(info) with gh.Issues {
       def ghCredentials = (<your-gh-username>, <your-gh-password>)
       def ghRepository = (<gh-user>, <gh-repo>)
    }

Obviously, you will not want to check in your github username and password with your source code and push that to github :). To supply
the `ghCredentials` tuple. You can use the `LocalGhCreds` function which assumes you have a externally defined `.gh`
file containing your github username and password. This should follow the standard java properties file format

    > cat path/to/home/.gh
    username=your-gh-username
    password=your-gh-password
