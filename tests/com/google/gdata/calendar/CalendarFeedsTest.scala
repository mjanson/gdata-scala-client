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


package com.google.gdata.calendar

import com.google.gdata.FeedFileTest

import org.junit._

/**
 * Test a various feeds by unpickling a real (saved) feed and pickling back to XML.
 * The two documents are then compared for similarity.
 */
class CalendarFeedsTest extends AnyRef with FeedFileTest {
  
  @Test def testCalendarsFeed {
    testRoundtrip("feeds/calendars-feed-in.xml", (new StdCalendarsFeed).feedPickler)
  }
  
}
