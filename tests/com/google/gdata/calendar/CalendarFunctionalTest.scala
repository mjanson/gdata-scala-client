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

import com.google.gdata.data.util.DateTime
import com.google.gdata.data.{Text, TextContent}
import data.kinds.{When, EventEntries}

import org.junit.{Test, Before, Assert}

import java.net.URL

/**
 * A set of functional tests for the Calendar API. It requires an internet connection.
 * 
 * It creates 6 random events, iterates over them and updates the content field, than
 * deletes them.
 */
class CalendarFunctionalTest {
  import Assert._
  
  val service: CalendarService = new CalendarService("google-test")
  
  @Before def setUp {
    service.setUserCredentials("iulian.test@gmail.com", "elanroz1")
  }
  
 /** 
  * Generate `n' random events in the next three days, with a random 
  * duration of up to 3 hours. Returns a sequence with the events returned
  * by the server.
  */
  def generateEntries(n: Int): Seq[service.eventsFeed.Entry] = {
    val random = new java.util.Random
    val now = new DateTime(new java.util.Date())

    for (i <- List.range(0, n)) yield {
      val start = now + (random.nextInt(4 * 24)) * DateTime.MILLIS_IN_HOUR
      val end = start + (random.nextInt(4) * DateTime.MILLIS_IN_HOUR)
      addEntry(events(i % events.length), start, end)
    }
  }

  lazy val events = List("Meeting with Sergey", 
      "Meeting with Chuck Norris", 
      "Jogging with Jessica Alba", 
      "Salsa with J Lo",
      "Beer with Pam")

  /** Add an entry with no description. */
  def addEntry(title: String, start: DateTime, end: DateTime): service.eventsFeed.Entry = {
    import service.eventsFeed._

    val url = (CalendarService.FEEDS 
        + "/" + "default" 
        + "/" + CalendarService.PRIVATE
        + "/" + CalendarService.FULL)

    val entry = new Entry(title, "", start, end)
    entry.eventStatus = Some(EventEntries.CONFIRMED)
    service.insert(new java.net.URL(url), entry, entryPickler)
  } 
  
  /**
   * Generate 6 random events in the default calendar. Update each one to have a description,
   * then delete them one by one.
   */
  @Test def calendarTest {
    val events = generateEntries(6)
    val newEvents = for (e <- events) yield {
      val ctnt = Some(TextContent("At the usual place"))
      e.content = ctnt
      val e1 = service.updateEvent(e)
      assertEquals("Update has not updated the content.", ctnt, e1.content)
      e1
    }
    for (e <- newEvents) e.editLink match {
      case Some(href) => service.delete(new URL(href))
      case None => fail("No edit link found on entry " + e)
    }
  }
}
