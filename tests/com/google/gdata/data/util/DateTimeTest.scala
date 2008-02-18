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
     val dt = DateTime.parse(str)
     assertEquals(str, dt.toString)
   }

   @Test def testValidNegativeTz {
     val str = "2008-02-15T16:16:02-01:00"
     val dt = DateTime.parse(str)
     assertEquals(str, dt.toString)
   }

   @Test def testValidZulu {
     val str = "2008-02-15T16:16:02Z"
     val dt = DateTime.parse(str)
     assertEquals(str, dt.toString)
   }

   @Test def testValidZulu1 {
     val str = "2008-01-15T23:59:02Z"
     val dt = DateTime.parse(str)
     assertEquals(str, dt.toString)
   }
   
   def testInvalidDate(date: String) {
     try {
       val dt = DateTime.parse(date)
       fail("Parsed invalid date")
     } catch {
       case e: ParseException => ()
     }     
   }
   
   @Test def testInvalidDates {
     testInvalidDate("2008-00-15T16:16:02Z")
     testInvalidDate("2008-12-01T1:16:02Z")
     testInvalidDate("20080-12-01T1:16:02Z")
     testInvalidDate("2008-01-33T16:16:02Z")
     testInvalidDate("1910-02-02T24:00:02Z")
   }
}
