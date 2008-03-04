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

/**
 * A module for Feeds. Use this by mixing it in a class that has an Entries implementation.
 *
 * This module provides a feed and a feed pickler.
 * This module requires an entry type and an entry pickler.
 *
 * @author Iulian Dragos
 */
trait Feeds { this: Feeds with Entries =>
  type Feed
  
  def feedPickler: Pickler[Feed]
}

/**
 * Atom feeds refines Feeds with Atom-like feeds.
 */
trait AtomFeeds extends Feeds { this: AtomFeeds with Entries =>
  type Feed <: AtomFeed
  
  /** A pickler for Feed objects. */
  def feedPickler: Pickler[Feed]
  
  /** An Atom Feed. */
  class AtomFeed extends AnyRef {
    var authors: List[Person] = Nil
    var categories: List[Category] = Nil
    var contributors: List[Person] = Nil
    var generator: Option[Generator] = None
    var icon: Option[String] = None
    var id: String = ""
    var links: List[Link] = Nil
    var logo: Option[String] = None
    var rights: Option[String] = None
    var subtitle: Option[Text] = None
    var title: Text = NoText
    var updated: DateTime = new DateTime(new java.util.Date())
    var entries: List[Entry] = Nil
    
    /** Initialization method to fill all known fields. */
    def fillOwnFields(authors: List[Person],
        categories: List[Category],
        contributors: List[Person],
        generator: Option[Generator],
        icon: Option[String],
        id: String,
        links: List[Link],
        logo: Option[String],
        rights: Option[String],
        subtitle: Option[Text],
        title: Text,
        updated: DateTime,
        entries: List[Entry]): this.type = {
      this.authors = authors
      this.categories = categories
      this.contributors = contributors
      this.generator = generator
      this.icon = icon
      this.id = id
      this.links = links
      this.logo = logo
      this.rights = rights
      this.subtitle = subtitle
      this.title = title
      this.updated = updated
      this.entries = entries
      this
    }

    /** Copy known fields from another AtomFeed. */
    def fromAtomFeed(af: AtomFeed) {
      fillOwnFields(af.authors, af.categories, af.contributors, af.generator, af.icon, af.id,
          af.links, af.logo, af.rights, af.subtitle, af.title, af.updated, af.entries)
    }
    
    override def toString = {
      val sb = new StringBuffer(256) // override the ridiculous 16-chars default size
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
        .append("\nEntries: ").append(entries.mkString("", "\n\t", ""))
        .toString
    }
  }

  lazy val atomFeedContents = {
    implicit val ns = Uris.atomNs
    
    interleaved(
        rep(atomPerson("author"))
      ~ rep(Category.pickler)
      ~ rep(atomPerson("contributor"))
      ~ opt(Generator.pickler)
      ~ opt(elem("icon", text))
      ~ elem("id", text)
      ~ rep(Link.pickler)
      ~ opt(elem("logo", text))
      ~ opt(elem("rights", text))
      ~ opt(atomText("subtitle"))
      ~ atomText("title")
      ~ elem("updated", dateTime)
      ~ rep(entryPickler))
  }

  lazy val atomFeedPickler: Pickler[AtomFeed] = wrap (atomFeedContents) ({
    case authors ~ cats ~ contribs ~ generator ~ icon ~ id
         ~ links ~ logo ~ rights ~ subtitle ~ title ~ updated ~ entries => 
      (new AtomFeed).fillOwnFields(authors, cats, contribs, generator, icon, id,
          links, logo, rights, subtitle, title, updated, entries)
  }) (fromFeed)

  private def fromFeed(e: AtomFeed) = (new ~(e.authors, e.categories) 
      ~ e.contributors
      ~ e.generator
      ~ e.icon
      ~ e.id
      ~ e.links
      ~ e.logo
      ~ e.rights
      ~ e.subtitle
      ~ e.title
      ~ e.updated
      ~ e.entries)
}
