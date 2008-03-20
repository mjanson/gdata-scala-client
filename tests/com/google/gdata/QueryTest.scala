package com.google.gdata

import org.junit._

class QueryTest {
  import CategoryQuery._

  @Test def testQuery {
     
    val q = (Query.empty / "Comedy" / (cat("animals") | !cat("fun"))
             suchThat Text("carlin") & !Text("george")).startIndex(25).alt("atom")
    
    Assert.assertEquals(q.mkUrl("http://gdata.youtube.com/feeds/api/videos"),
        "http://gdata.youtube.com/feeds/api/videos/-/Comedy/animals%7C-fun?start-index=25&alt=atom&q=carlin+-george")
  }
  
  @Test def testCategoryQ {
    val q = Query.empty / "Comedy" / cat("http://gdata.youtube.com/schemas/2007/keywords.cat", "food")
    val q2 = Query.empty / "bass" / !cat("fish") / !cat("fishing")
    Assert.assertEquals(q.mkUrl("http://gdata.youtube.com/feeds/api/videos"),
    "http://gdata.youtube.com/feeds/api/videos/-/Comedy/%7Bhttp%3A%2F%2Fgdata.youtube.com%2Fschemas%2F2007%2Fkeywords.cat%7Dfood")
    Assert.assertEquals(q2.mkUrl("http://gdata.youtube.com/feeds/api/videos"),
    "http://gdata.youtube.com/feeds/api/videos/-/bass/-fish/-fishing")
  }
  
  @Test def testSearchQ {
    val q = Query.empty suchThat (Text("football") & !Text("soccer"))
    Assert.assertEquals(q.mkUrl("http://gdata.youtube.com/feeds/api/videos"),
    "http://gdata.youtube.com/feeds/api/videos?q=football+-soccer")
  } 
}

object QueryTestMain {
  def main(args: Array[String]) {
    val qt = new QueryTest
    qt.testQuery
    qt.testCategoryQ
    qt.testSearchQ
  }
}
