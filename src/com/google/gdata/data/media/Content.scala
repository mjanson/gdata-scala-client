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


package com.google.gdata.data.media;

import com.google.xml.combinators.{Picklers, ~}
import com.google.xml.combinators.Picklers._
import com.google.gdata.data.Uris.mediaNs
import com.google.util.Utility.printOptional

/**
 * A media:content element. It describes the media object being syndicated.
 * 
 * @see http://search.yahoo.com/mrss
 * @author Iulian Dragos
 */
class Content {
  /** Direct url to media object. */
  var url: Option[String] = None
  
  /** File size in bytes. */
  var fileSize: Option[Int] = None
  
  /** MIME type of object. */
  var tpe: Option[String] = None
  
  /** Type of object: image | audio | video | document | executable. */
  var medium: Option[String] = None
  
  /** Is this the default object for the enclosing media group? */
  var isDefault: Option[Boolean] = None
  
  /** Is the object a sample of full version? sample | full | nonstop. */
  var expression: String = "full"
  
  /** Bitrate in kbs. */
  var bitrate: Option[Int] = None
  
  /** Number of frames per second */
  var framerate: Option[Double] = None
  
  /** Number of samples per second, in kHz. */
  var samplingrate: Option[Double] = None
  
  /** Number of audio channels. */
  var channels: Option[Int] = None
  
  /** Time length in seconds. */
  var duration: Option[Int] = None
  
  /** Height of the media object. */
  var height: Option[Int] = None
  
  /** Width of the media object. */
  var width: Option[Int] = None
  
  /** Primary language of the media object. */
  var lang: Option[String] = None
  
  /** Additional optional elements. */
  var group: Group = new Group
  
  /** Fill fields if this object. */
  def fillOwnFields(url: Option[String], fileSize: Option[Int], tpe: Option[String], medium: Option[String],
      isDefault: Option[Boolean], expression: String, bitrate: Option[Int], framerate: Option[Double],
      samplingrate: Option[Double], channels: Option[Int], duration: Option[Int], height: Option[Int],
      width: Option[Int], lang: Option[String], group: Group): this.type = {
    this.url = url
    this.fileSize = fileSize
    this.tpe = tpe
    this.medium = medium
    this.isDefault = isDefault
    this.expression = expression
    this.bitrate = bitrate
    this.framerate = framerate
    this.samplingrate = samplingrate
    this.channels = channels
    this.duration = duration
    this.height = height
    this.width = width
    this.lang = lang
    this.group = group
    this
  }
  
  override def toString = {
    val sb = new StringBuilder(256) // override default buffer size
    sb.append("Content: \n\t")
    printOptional(sb, " url", url)
    printOptional(sb, " fileSize", fileSize)
    printOptional(sb, " tpe", tpe)
    printOptional(sb, " medium", medium)
    printOptional(sb, " isDefault", isDefault)
    sb.append(" expression: ").append(expression)
    printOptional(sb, " bitrate", bitrate)
    printOptional(sb, " framerate", framerate)
    printOptional(sb, " samplingrate", samplingrate)
    printOptional(sb, " channels", channels)
    printOptional(sb, " duration", duration)
    printOptional(sb, " height", height)
    printOptional(sb, " width", width)
    printOptional(sb, " lang", lang)
    sb.append(group)
    sb.toString
  }
}

object Content {
  
  /** A pickler for media:content objects. @see com.google.gdata.data.media.Content */
  val pickler: Pickler[Content] = 
    (wrap (elem("content", opt(attr("url", text)) ~ opt(attr("fileSize", intVal)) ~ opt(attr("type", text))
        ~ opt(attr("medium", text)) ~ opt(attr("isDefault", boolVal)) ~ default(attr("expression", text), "full")
        ~ opt(attr("bitrate", intVal)) ~ opt(attr("framerate", doubleVal)) ~ opt(attr("samplingrate", doubleVal))
        ~ opt(attr("channels", intVal)) ~ opt(attr("duration", intVal)) ~ opt(attr("height", intVal))
        ~ opt(attr("width", intVal)) ~ opt(attr("lang", text)) ~ Group.contents) (mediaNs)) ({
        case url ~ fileSize ~ tpe ~ medium ~ isDefault ~ expression ~ bitrate ~ framerate 
            ~ samplingrate ~ channels ~ duration ~ height ~ width ~ lang ~ group => 
          (new Content).fillOwnFields(url, fileSize, tpe, medium, isDefault, expression, bitrate, 
              framerate, samplingrate, channels, duration, height, width, lang, group)
       }) (fromContent))
  
  def fromContent(c: Content) = 
    (new ~(c.url, c.fileSize) ~ c.tpe ~ c.medium ~ c.isDefault ~ c.expression ~ c.bitrate
        ~ c.framerate ~ c.samplingrate ~ c.channels ~ c.duration ~ c.height ~ c.width ~ c.lang ~ c.group)
}

