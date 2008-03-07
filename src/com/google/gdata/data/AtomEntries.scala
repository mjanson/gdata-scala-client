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

import scala.xml.{NamespaceBinding, TopScope}

import Picklers._
import Atom._

/** 
 * An interface for abstract entries. It is further refined by providing lower
 * bounds for the Entry type. @see AtomEntries.
 *
 * @author Iulian Dragos
 */
trait Entries {
  /** The abstract type for entries. */
  type Entry
  
  /** A pickler for the abstract type Entry. */
  def entryPickler: Pickler[Entry] = elem("entry", entryContentsPickler)(Uris.atomNs)
  
  /** An abstract pickler for entries. Subclasses need to implement this. */
  def entryContentsPickler: Pickler[Entry]
}

/**
 * Atom Entries provides a type of Atom entries and a pickler for the AtomEntry. The
 * Entry type remains abstract, to allow further refinements.
 *
 * @author Iulian Dragos
 */
trait AtomEntries extends Entries {
  type Entry <: AtomEntry
  
  /** An Atom Entry. */
  class AtomEntry  {
    var authors: List[Person] = Nil
    var categories: List[Category] = Nil
    var content: Option[Content] = None
    var contributors: List[Person] = Nil
    var id: String = ""
    var links: List[Link] = Nil
    var published: Option[DateTime] = None
    var rights: Option[String] = None
    var source: Option[Source] = None
    var summary: Option[Text] = None
    var title: Text = NoText
    var updated: DateTime = new DateTime(new java.util.Date())
    
    def fillOwnFields(authors: List[Person],
        cats: List[Category],
        content: Option[Content],
        contributors: List[Person],
        id: String,
        links: List[Link],
        published: Option[DateTime],
        rights: Option[String],
        source: Option[Source],
        summary: Option[Text],
        title: Text,
        updated: DateTime): this.type = {
      this.authors = authors
      this.categories = cats
      this.content = content
      this.contributors = contributors
      this.id = id
      this.links = links
      this.published = published
      this.rights = rights
      this.source = source
      this.summary = summary
      this.title = title
      this.updated = updated
      this
    }
  
    def fromAtomEntry(e: AtomEntry) {
      fillOwnFields(e.authors, e.categories, e.content, e.contributors, e.id, e.links, e.published,
          e.rights, e.source, e.summary, e.title, e.updated)
    }
    
    override def toString = {
      val sb = new StringBuffer(256) // override the ridiculous default size of 16-chars
      sb.append("\n---- Entry -----\n")      
        .append("Authors: ").append(authors.mkString("", ", ", ""))
        .append("\nCategories: ").append(categories.mkString("", ", ", ""))
        .append("\nContent: ").append(content)
        .append("\nContributors: ").append(contributors.mkString("", ", ", ""))
        .append("\nId: ").append(id)
        .append("\nLinks: ").append(links.mkString("", ", ", ""))
        .append("\nPublished: ").append(published)
        .append("\nRights: ").append(rights)
        .append("\nSource: ").append(source)
        .append("\nSummary: ").append(summary)
        .append("\nTitle: ").append(title)
        .append("\nUpdated: ").append(updated)
        .toString
    }
  }

  private lazy val atomEntryContents = {
    implicit val ns = Uris.atomNs
    interleaved(
        rep(atomPerson("author"))
      ~ rep(Category.pickler)
      ~ opt(Content.pickler)
      ~ rep(atomPerson("contributor"))
      ~ elem("id", text)
      ~ rep(Link.pickler)
      ~ opt(elem("published", dateTime))
      ~ opt(elem("rights", text))
      ~ opt(Source.pickler)
      ~ opt(atomText("summary"))
      ~ atomText("title")
      ~ elem("updated", dateTime))
  }
        
  lazy val atomEntryContentsPickler: Pickler[AtomEntry] = wrap (atomEntryContents) ({
    case authors ~ cats ~ content ~ contribs ~ id ~ links
         ~ published ~ rights ~ src ~ summary ~ title ~ updated => 
      (new AtomEntry).fillOwnFields(authors, cats, content, contribs, id, links, published, 
          rights, src, summary, title, updated)
  }) (fromEntry)

  private def fromEntry(e: AtomEntry) = (new ~(e.authors, e.categories) 
      ~ e.content
      ~ e.contributors
      ~ e.id
      ~ e.links
      ~ e.published
      ~ e.rights
      ~ e.source
      ~ e.summary
      ~ e.title
      ~ e.updated)
}