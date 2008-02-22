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


package com.google.gdata.data.util

import java.text.ParseException
import java.util.{GregorianCalendar, TimeZone, Calendar}
import java.text.ParseException

import scala.util.parsing.combinator.{Parsers, ImplicitConversions}
import scala.util.parsing.input.CharArrayReader


/**
 * A class for representing normal play times, as defined by RFC 2326. It does not support
 * ranges, and fractional time is restricted to milliseconds (Media RSS needs).
 *
 * @see http://www.ietf.org/rfc/rfc2326.txt
 * @see http://search.yahoo.com/mrss
 * @author Iulian Dragos
 */
abstract class NormalPlayTime {
  /** The value in milliseconds. */
  def value: Long
  
  /** Is this object the special time 'now'? */
  def isNow: Boolean
}

/** A fixed time offset. */
case class SpecificTime(value: Long) extends NormalPlayTime {
  assert(value > 0, "Invalid time offset")
  
  def isNow = false
  
  /** A time offset with given hour, minute, second and milliseconds */
  def this(h: Int, m: Int, s: Int, millis: Int) = {
    this(millis + s * 1000 + m * 60000 + h * 3600000)
    if (!(h >= 0))
      throw new IllegalArgumentException("Invalid hour: " + h)
    if (!(m >= 0 && m < 60))
      throw new IllegalArgumentException("Invalid minute: " + m)
    if (!(s >= 0 && s < 60))
      throw new IllegalArgumentException("Invalid second: " + s)
  }
}

/** The specially designated time instant 'now'. @see http://www.ietf.org/rfc/rfc2326.txt */
case object Now extends NormalPlayTime {
  def value: Long = error("No 'value' in milliseconds for Now")
  
  def isNow = true
}

object NormalPlayTime {
  
  def fromNptString(nptString: String): NormalPlayTime = {
    import DateParser._

    var str = nptString.trim
    if (str == "now") 
      Now
    else {
      val p = ((DateParser.intLiteral <~ ':')
            ~ (DateParser.oneOrTwoDigits <~ ':')
            ~ DateParser.oneOrTwoDigits
            ~ opt(secFraction) ^^ {
              case hour ~ min ~ second ~ Some(fraction) => 
                new SpecificTime(hour, min, second, (fraction * 1000).toInt)
              case hour ~ min ~ second ~ None => 
                new SpecificTime(hour, min, second, 0)
      })
      p(new CharArrayReader(str.toArray)) match {
        case Success(v, _) => v
        case f: NoSuccess => throw new ParseException(f.toString, 0)
      }
    }
  }
}
