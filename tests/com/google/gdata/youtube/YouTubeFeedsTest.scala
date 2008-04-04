/* Copyright (c) 2008 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


package com.google.gdata.youtube

import com.google.xml.combinators.PicklerAsserts
import com.google.xmldiff.{SimplePath}
import com.google.gdata.FeedFileTest

import org.junit._

/**
 * Test a various feeds by unpickling a real (saved) feed and pickling back to XML.
 * The two documents are then compared for similarity.
 */
class YouTubeFeedsTest extends AnyRef with FeedFileTest {
  
  @Test def testVideoFeed {
    testRoundtrip("feeds/video-feed-in.xml", (new StdVideoFeed).feedPickler, "//rating")
  }
  
  @Test def testContactsFeed {
    testRoundtrip("feeds/contacts-feed-in.xml", (new StdContactsFeed).feedPickler)
  }

  @Test def testCommentsFeed {
    testRoundtrip("feeds/comments-feed-in.xml", (new StdCommentsFeed).feedPickler)
  }
  
  @Test def testPlaylistFeed {
    testRoundtrip("feeds/playlist-feed-in.xml", (new StdPlaylistFeed).feedPickler, 
        "/feed/group", // no media:group in feeds yet.
        "/feed/entry/rating")
  }
  
  @Test def testUserPlaylistsFeed {
    testRoundtrip("feeds/userplaylists-feed-in.xml", (new StdUserPlaylistsFeed).feedPickler)
  }
  
  @Test def testUserProfileEntry {
    testRoundtrip("feeds/userprofile-entry-in.xml", (new StdUserProfileEntry).entryPickler)
  }
  
  @Test def testSubscriptionsFeed {
    testRoundtrip("feeds/subscriptions-feed-in.xml", (new StdSubscriptionFeed).feedPickler)
  }
}
