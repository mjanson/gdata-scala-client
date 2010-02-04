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


package cal

import com.google.gdata.calendar._
import com.google.gdata.data._
import kinds._
import util.DateTime
import com.google.xml.combinators._
import java.net.URL

/**
 * An illogical collection of things one can do with the Calendar API.
 */
object Main extends Application {

  val s = new CalendarService("google-test") {}

  // put some real username and password here
  s.setUserCredentials("johndoe@gmail.com", "secretthang")

  addEntry("Tennis with Pete Sampras", 
           DateTime(2008, 4, 4, 18, 30, 0, None, 0),
           DateTime(2008, 4, 4, 20, 30, 0, None, 0))

  val q = new CalendarQuery
  clean(q matching com.google.gdata.Text("Pete Sampras"))


//  println("getting cals: ")
//  getCals

  def getCals {
    println(s.getCalendars("http://www.google.com/calendar/feeds/default/allcalendars/full"))
    println(s.getAllUserCalendars)
    println(s.getOwnedUserCalendars)
  }

  /** Clean entries what match `q' */
  def clean(q: CalendarQuery) {
    val events = s.getEvents("private", "full", q orderBy "starttime")
    for (e <- events) {
      print(e.title + " is " + e.eventStatus)
      print(", starting at: ")
      for (when <- e.when; st <- when.startTime) print(st)
      print("Deleting entry.. ")
      e.link("edit") match {
        case Some(url) => s.delete(new URL(url.href)); println("[ok]")
        case None => println("[No link 'edit' found]")
      }
    }
  }

  updateTennisLocations("Montbenon")

  def updateTennisLocations(loc: String) {
    val q = new CalendarQuery
    for (e <- s.getEvents("private", "full", q matching com.google.gdata.Text("Tennis"))) {
      e.locations = new Where[s.contacts.Entry](loc, kinds.Schemas.EVENT) :: e.locations
      s.updateEvent(e)
    }
  }

  generateEntries(6)

  /** Generate `n' random events in the next three days, with a random duration of up to 3 hours */
  def generateEntries(n: Int) {
    val random = new java.util.Random
    val now = new DateTime(new java.util.Date())

    for (i <- 0 until n) {
      val start = now + (random.nextInt(4 * 24)) * DateTime.MILLIS_IN_HOUR
      val end = start + (random.nextInt(4) * DateTime.MILLIS_IN_HOUR)
      println("adding event at " + start)
      addEntry(events(i % events.length), start, end)
    }
  }

  lazy val events = List("Meeting with Sergey", 
                    "Meeting with Chuck Norris", 
                    "Jogging with Jessica Alba", 
                    "Salsa with J Lo",
                    "Beer with Pam")

  def addEntry(title: String, start: DateTime, end: DateTime): s.eventsFeed.Entry = {
    import s.eventsFeed._

    val url = (CalendarService.FEEDS 
        + "/" + "default" 
        + "/" + CalendarService.PRIVATE
        + "/" + CalendarService.FULL)

    val entry = new Entry(title, "", start, end)
    entry.eventStatus = Some(EventEntries.CONFIRMED)
    s.insert(new java.net.URL(url), entry, entryPickler)
  }
}
