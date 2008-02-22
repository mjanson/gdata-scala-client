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


package com.google.gdata.data.media

import com.google.xml.combinators.~
import com.google.xml.combinators.Picklers._
  
import scala.xml.{NamespaceBinding, TopScope}

/**
 * A text element, as defined by Media RSS. It can be used for media:title and
 * media:description elements.  @see http://search.yahoo.com/mrss
 *
 * @author Iulian Dragos
 */
case class Text(tpe: String, value: String)

object Text {
  val mediaNs = new NamespaceBinding("media", Uris.MEDIA, TopScope)

  /** Return a pickle for the given element name. */
  def pickler(elemName: String): Pickler[Text] =
    (wrap (elem(elemName, default(attr("type", text), "plain") ~ text)(mediaNs)) 
        (Text.apply)
        (t => new ~(t.tpe, t.value)))
}