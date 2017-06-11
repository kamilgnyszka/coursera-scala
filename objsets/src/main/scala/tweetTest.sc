import objsets.{Empty, Tweet, TweetList, TweetSet, TweetReader}

val tn = null.asInstanceOf[Tweet]
val t = new Tweet("a","aa",20)

if (tn != null)
t.retweets > tn.retweets


val set1 = new Empty
val set2 = set1.incl(new Tweet("a", "android", 10))
val set3 = set2.incl(new Tweet("b", "b galaxy", 20))
val c = new Tweet("c", "c nexus", 50)
val d = new Tweet("d", "d nexus", 9)
val set4c = set3.incl(c)
val set4d = set3.incl(d)
val set5 = set4c.incl(d)

set5.descendingByRetweet foreach println


set5.mostRetweeted

val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(t => google.exists(p => t.text.contains(p)))
lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(t => apple.exists(p => t.text.contains(p)))

googleTweets foreach println
appleTweets foreach println

