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

import java.util.{Date, Calendar, GregorianCalendar, TimeZone}
 
/**
 * Simple class to represent Date/Time. It holds the time as the
 * number of milliseconds from the Epoch (similar to java.util.Date)
 * and keeps track of the timezone shift. The number of milliseconds 
 * is normalized to UTC, so time shift has to be applied when using
 * this date. 
 *
 * @param value The number of milliseconds from the Epoch.
 * @param tzShift The time zone shift, in milliseconds. 
 */
class DateTime(private var value: Long, private var tzShift: Long) extends Ordered[DateTime] {
  
  /** 
   * Compare this DateTime to another. Date d1 is less than d2 iff the UTC time of d1
   * is less than the UTC time of d2. This translates to comparing the 'value' field
   * of the two dates (time zones are not taken into account).
   */
  def compare(that: DateTime) = (this.value - that.value).toInt
  
  /** 
   * Create a DateTime from the specificed Java Date. The given date is considered
   * to be in UTC time (zero time zone shift).
   */
  def this(date: Date) {
    this(date.getTime(), 0)
  }
  
  /** Create a DateTime from the given Java Date. */
  def this(date: Date, tz: TimeZone) {
    this(date.getTime(), tz.getOffset(date.getTime()))
  }
  
  def toDate: java.util.Date =
    new java.util.Date(value)
  
  override def toString(): String = {
    import DateTime._
    val sb = new StringBuffer()
    calendar.setTimeInMillis(value + tzShift)
    
    import Calendar._
    
    padInt(sb, calendar.get(YEAR), 4).append('-')
    padInt(sb, calendar.get(MONTH) + 1, 2).append('-')  // Calendar month is 0-based
    padInt(sb, calendar.get(DAY_OF_MONTH), 2).append('T')
    padInt(sb, calendar.get(HOUR_OF_DAY), 2).append(':')
    padInt(sb, calendar.get(MINUTE), 2).append(':')
    padInt(sb, calendar.get(SECOND), 2)
    if (calendar.isSet(MILLISECOND)) {
      val millis = calendar.get(MILLISECOND)
      if (millis > 0)
        sb.append('.').append(millis)
    }
    if (tzShift == 0)
      sb.append('Z')
    else {
      var absoluteShift = tzShift
      if (absoluteShift > 0) 
        sb.append('+') 
      else {
        sb.append('-')
        absoluteShift = -absoluteShift
      }
      padInt(sb, absoluteShift / (60 * 60 * 1000), 2).append(':')
      padInt(sb, (absoluteShift % (60 * 60 * 1000)) / (60 * 1000), 2)
    }
    
    sb.toString
  }
  
  /** 
   * Adds leading zeroes to a given int to fill 'digits', and appends it to the StringBuffer.
   * Assumes positive integers.
   */
  private def padInt(sb: StringBuffer, n: Long, digits: Int): StringBuffer = {
    val str = n.toString
    var delta = digits - str.length
    
    assert(delta >= 0, "Not enough digits to pad number: " + n + " on " + digits + " digits")
    
    while (delta > 0) {
      sb.append('0')
      delta = delta - 1
    }
    sb.append(str)
  }
    
  private lazy val calendar = new GregorianCalendar(TimeZone.getTimeZone("GMT"))
}

object DateTime {
  /**  
   * Parse a date time, according to RFC3339
   * 
   * @throws ParseException when the string is not properly parsed. 
   */
  def parse(str: String) =
    DateParser.parse(str)
    
  /**
   * Create a date object with the given coordinates. The date is considered to be shifted 
   * from UTC with the amount of milliseconds given in 'offset'. 
   * <p/>
   * It checks that the month is between
   * 1 and 12, day of the month between 1 and 31 and normal hour and minutes check. It doesn't
   * do more fancy checks (check day of month w.r.t. to the given month, leap year/minute/second).
   */
  def apply(year: Int, month: Int, day: Int, 
      hour: Int, min: Int, sec: Int, frac: Option[Double], 
      offset: Long) = {
    assert(month > 0 && month < 13)
    assert(day > 0 && day < 32)
    assert(hour >= 0 && hour < 24)
    assert(min >= 0 && min < 61)
    assert(sec >= 0 && sec < 61)
    
    val calendar = new GregorianCalendar(TimeZone.getTimeZone("GMT"))
    calendar.clear
    calendar.set(year, month - 1, day, hour, min, sec) // month is 0-based
    frac match {
      case Some(fraction) => calendar.set(Calendar.MILLISECOND, (fraction * 1000).toInt)
      case None => ()
    }
    new DateTime(calendar.getTimeInMillis() - offset, offset) 
  }
}