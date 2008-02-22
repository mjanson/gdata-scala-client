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


package com.google.gdata.data;

import com.google.xml.combinators.{Picklers, ~}
import com.google.gdata.data.util.DateTime

import Picklers._
import Atom._

/** An Atom source. */
class Source {
  var authors: List[Person] = Nil
  var categories: List[Category] = Nil
  var contributors: List[Person] = Nil
  var generator: Option[Generator] = None
  var icon: Option[String] = None
  var id: Option[String] = None
  var links: List[Link] = Nil
  var logo: Option[String] = None
  var rights: Option[String] = None
  var subtitle: Option[Text] = None
  var title: Option[Text] = None
  var updated: Option[DateTime] = None

  override def toString = {
    val sb = new StringBuffer
    sb.append("Authors: ").append(authors.mkString("", ", ", ""))
      .append("\nCategories: ").append(categories.mkString("", ", ", ""))
      .append("\nContributors: ").append(contributors.mkString("", ", ", ""))
      .append("\nGenerator: ").append(generator)
      .append("\nIcon: ").append(icon)
      .append("\nId: ").append(id)
      .append("\nLinks: ").append(links.mkString("", ", ", ""))
      .append("\nLogo: ").append(logo)
      .append("\nRights: ").append(rights)
      .append("\nSubtitle: ").append(subtitle)
      .append("\nTitle: ").append(title)
      .append("\nUpdated: ").append(updated)
      .toString
  }
}

object Source {
  lazy val atomSourceContents =
    interleaved(
        rep(atomPerson("author"))
      ~ rep(Category.pickler)
      ~ rep(atomPerson("contributor"))
      ~ opt(Generator.pickler)
      ~ opt(elem("id", text))
      ~ rep(Link.pickler)
      ~ opt(elem("logo", text))
      ~ opt(elem("rights", text))
      ~ opt(atomText("subtitle"))
      ~ opt(atomText("title"))
      ~ opt(elem("updated", dateTime)))
        
  lazy val pickler: Pickler[Source] = wrap (elem("source", atomSourceContents)) ({
    case authors ~ cats ~ contribs ~ generator ~ id
         ~ links ~ logo ~ rights ~ subtitle ~ title ~ updated => 
      val e = new Source
      e.authors = authors
      e.categories = cats
      e.contributors = contribs
      e.generator = generator
      e.id = id
      e.links = links
      e.logo = logo
      e.rights = rights
      e.subtitle = subtitle
      e.title = title
      e.updated = updated
      e
  }) (fromSource)

  private def fromSource(e: Source) = (new ~(e.authors, e.categories) 
      ~ e.contributors
      ~ e.generator
      ~ e.id
      ~ e.links
      ~ e.logo
      ~ e.rights
      ~ e.subtitle
      ~ e.title
      ~ e.updated)
      
}