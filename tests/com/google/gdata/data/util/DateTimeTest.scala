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

   
package com.google.gdata.data.util;

import org.junit.{Test, Assert}
import java.text.ParseException

/**
 * Tests for custom DateTime (parsing/printing)
 */
class DateTimeTest {
   import Assert._
   
   @Test def testValidTz1 {
     val str = "2008-02-15T16:16:02+01:00"
     val out = "2008-02-15T16:16:02.000+01:00"
     val dt = DateTime.parseDateTime(str)
     assertEquals(out, dt.toString)
   }

   @Test def testValidNegativeTz {
     val str = "2008-02-15T16:16:02-01:00"
     val out = "2008-02-15T16:16:02.000-01:00"
     val dt = DateTime.parseDateTime(str)
     assertEquals(out, dt.toString)
   }

   @Test def testValidZulu {
     val str = "2008-02-15T16:16:02Z"
     val out = "2008-02-15T16:16:02.000Z"
     val dt = DateTime.parseDateTime(str)
     assertEquals(out, dt.toString)
   }

   @Test def testValidZulu1 {
     val str = "2008-01-15T23:59:02Z"
     val out = "2008-01-15T23:59:02.000Z"
     val dt = DateTime.parseDateTime(str)
     assertEquals(out, dt.toString)
   }
   
   @Test def testValidFractional {
     val str = "2003-12-13T18:30:02.250+01:00"
     val dt = DateTime.parseDateTime(str)
     assertEquals(str, dt.toString)
   }
   
   def testInvalidDate(date: String) {
     try {
       val dt = DateTime.parseDateTime(date)
       fail("Parsed invalid date")
     } catch {
       case e: ParseException => ()
     }     
   }
   
   @Test def testValidDateOnly {
     val str = "2008-12-25"
     val dt = DateTime.parseDate(str)
     assertTrue("dateOnly flag not set.", dt.dateOnly)
     assertEquals(str, dt.toString)
   }

   @Test def testValidDateOrDateTime {
     val str = "2008-12-25"
     val str1 = "2008-12-25T14:12:55.000Z"
     val dt = DateTime.parseDateOrDateTime(str)
     assertTrue("dateOnly flag not set when parsing " + str, dt.dateOnly)
     assertEquals(str, dt.toString)
     
     val dt1 = DateTime.parseDateOrDateTime(str1)
     assertFalse("dateOnly flag set when parsing " + str1, dt1.dateOnly)
     assertEquals(str1, dt1.toString)
   }
   
   @Test def testInvalidDates {
     testInvalidDate("2008-00-15T16:16:02Z")
     testInvalidDate("2008-12-01T1:16:02Z")
     testInvalidDate("20080-12-01T1:16:02Z")
     testInvalidDate("2008-01-33T16:16:02Z")
     testInvalidDate("1910-02-02T24:00:02Z")
   }
}
