package objsets

/// Class instance of TweetReader has the 
object TweetReader {

  object ParseTweets {
    def regexParser(s: String): List[Map[String, Any]] = {
      // In real life. you would use an actual JSON library...
      val tweetRegex = """^\{ .*"user": "([^"]+)", "text": "([^"]+)", "retweets": ([\\.0-9]+) \},?$""".r
      s.split("\r?\n").toList.tail.init.map {
        case tweetRegex(user, text, retweets) => Map("user" -> user, "text" -> text, "retweets" -> retweets.toDouble)
      }
    }

    def getTweets(user: String, json: String): List[Tweet] =
      for (map <- regexParser(json)) yield {
        val text = map("text")
        val retweets = map("retweet_count")
        new Tweet(user, text.toString, retweets.toString.toDouble.toInt)
      }

    def getTweetData(user: String, json: String): List[Tweet] = {
      // is list
      val l = regexParser(json)
      for (map <- l) yield {
        val text = map("text")
        val retweets = map("retweets")
        new Tweet(user, text.toString, retweets.toString.toDouble.toInt)
      }
    }
  }

  def toTweetSet(l: List[Tweet]): TweetSet = {
    l.foldLeft(new Empty: TweetSet)(_.incl(_))
  }

  def unparseToData(tws: List[Tweet]): String = {
    val buf = new StringBuffer
    for (tw <- tws) {
      val json = "{ \"user\": \"" + tw.user + "\", \"text\": \"" +
                                    tw.text.replaceAll(""""""", "\\\\\\\"") + "\", \"retweets\": " +
                                    tw.retweets + ".0 }"
      buf.append(json + ",\n")
    }
    buf.toString
  }

  // list of all sources
  val sites = List("gizmodo", "TechCrunch", "engadget", "amazondeals", "CNET", "gadgetlab", "mashable")

  // lists of tweets from a specific source
  private val gizmodoTweets = TweetReader.ParseTweets.getTweetData("gizmodo", TweetData.gizmodo)
  private val techCrunchTweets = TweetReader.ParseTweets.getTweetData("TechCrunch", TweetData.TechCrunch)
  private val engadgetTweets = TweetReader.ParseTweets.getTweetData("engadget", TweetData.engadget)
  private val amazondealsTweets = TweetReader.ParseTweets.getTweetData("amazondeals", TweetData.amazondeals)
  private val cnetTweets = TweetReader.ParseTweets.getTweetData("CNET", TweetData.CNET)
  private val gadgetlabTweets = TweetReader.ParseTweets.getTweetData("gadgetlab", TweetData.gadgetlab)
  private val mashableTweets = TweetReader.ParseTweets.getTweetData("mashable", TweetData.mashable)

  // list of all tweets from all sources
  private val sources = List(gizmodoTweets, techCrunchTweets, engadgetTweets, amazondealsTweets, cnetTweets, gadgetlabTweets, mashableTweets)

  // mapping a twwet source to all tweets of that source
  val tweetMap: Map[String, List[Tweet]] =
    Map() ++ Seq((sites(0) -> gizmodoTweets),
                 (sites(1) -> techCrunchTweets),
                 (sites(2) -> engadgetTweets),
                 (sites(3) -> amazondealsTweets),
                 (sites(4) -> cnetTweets),
                 (sites(5) -> gadgetlabTweets),
                 (sites(6) -> mashableTweets))

  // List of tweetsets from all sources collected on a list
  val tweetSets: List[TweetSet] = sources.map(tweets => toTweetSet(tweets))

  // zip of sources and tweetsets from that source
  private val siteTweetSetMap: Map[String, TweetSet] =
    Map() ++ (sites zip tweetSets)

  // take a list of tweet sets and gathers them all in one set
  private def unionOfAllTweetSets(curSets: List[TweetSet], acc: TweetSet): TweetSet =
    if (curSets.isEmpty) acc
    else unionOfAllTweetSets(curSets.tail, acc.union(curSets.head))

  // constant TweetSet alltweet returns one big set of all tweets
  val allTweets: TweetSet = unionOfAllTweetSets(tweetSets, new Empty)
}
