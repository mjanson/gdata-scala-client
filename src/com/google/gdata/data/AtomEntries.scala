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

import com.google.xml.combinators.{Picklers, ~, HasStore}
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
  type Entry <: HasStore
  
  /** A pickler for the abstract type Entry. */
  def entryPickler: Pickler[Entry] = elem("entry", makeExtensible(entryContentsPickler))(Uris.atomNs)
  
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
  class AtomEntry extends AnyRef with LinkNavigation with HasStore {
    var authors: List[Person] = Nil
    var categories: List[Category] = Nil
    var content: Option[Content] = None
    var contributors: List[Person] = Nil
    var id: Option[String] = None
    var links: List[Link] = Nil
    var published: Option[DateTime] = None
    var rights: Option[String] = None
    var source: Option[Source] = None
    var summary: Option[Text] = None
    var title: Text = NoText
    var updated: DateTime = new DateTime(new java.util.Date())
    
    /**
     * Fill fields declared by this class, from the given parameters. Subclasses should
     * implement a similar method, along with the right 'fromX' method. This way 
     * subclasses and picklers only have to care about fields introduced by the
     * current class.
     */
    def fillOwnFields(authors: List[Person], cats: List[Category], content: Option[Content],
        contributors: List[Person], id: Option[String], links: List[Link],
        published: Option[DateTime], rights: Option[String], source: Option[Source], 
        summary: Option[Text], title: Text, updated: DateTime): this.type = {
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
  
    /**
     * Copy known data from the given AtomEntry. Useful when subclassing AtomEntry to add
     * new data. Subclasses can call this method to fill standard entry fields. They should
     * also provide a similar 'fromSubclass' method, to anticipate further subclassing. 
     */
    def fromAtomEntry(e: AtomEntry): this.type = {
      fillOwnFields(e.authors, e.categories, e.content, e.contributors, e.id, e.links, e.published,
          e.rights, e.source, e.summary, e.title, e.updated)
    }
    
    override def toString = {
      val sb = new StringBuffer(256) // override the ridiculous default size of 16-chars
      sb.append("Entry:")      
        .append("\n\tAuthors: ").append(authors.mkString("", ", ", ""))
        .append("\n\tId: ").append(id)
        .append("\n\tTitle: ").append(title)
        .append("\n\tUpdated: ").append(updated)
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
      ~ opt(elem("id", text))
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