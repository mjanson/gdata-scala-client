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

/** Test NormalPlayTime parsing. */
class NormalPlayTimeTest {
  import Assert._
  
  @Test def testNow {
    val play = "  now  "
    assertEquals("Failed parsing 'now'", Now, NormalPlayTime.fromNptString(play))
  }

  def assertParsedTime(in: String, expected: Long) {
    NormalPlayTime.fromNptString(in) match {
      case SpecificTime(value) => 
        if (value != expected) 
          fail("Error parsing '" + in + "', expected: " + expected + " but " + value + " found")  
      case npt => fail("Error parsing: " + npt) 
    }
  }
  
  @Test def testMin {
    val play = "0:12:00"
    assertParsedTime(play, 720000)
  }
  
  @Test def testHour1 {
    assertParsedTime("1:0:00", 3600000)
  }

  @Test def testHour2 {
    assertParsedTime("01:00:00", 3600000)
  }

  @Test def testFrac1 {
    assertParsedTime("0:00:00.1", 100)
  }

  @Test def testFrac2 {
    assertParsedTime("0:00:00.12345", 123)
  }
}

object TimeTest extends NormalPlayTimeTest with Application {
  testHour2
}