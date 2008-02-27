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
import com.google.gdata.data.Uris.mediaNs  
import com.google.util.Utility.printOptional

/**
 * A media:group as defined by Media RSS. @see http://search.yahoo.com/mrss
 *
 * @author Iulian Dragos
 */
class Group {
  /** Declares permissible audience. */
  var rating: Option[Rating] = None
  
  /** Title of the media object. */
  var title: Option[SimpleText] = None
  
  /** Short description, usually one sentence. */
  var description: Option[SimpleText] = None
  
  /** Relevant keywords describing the media object. */
  var keywords: List[String] = Nil
  
  /** Representative images for this media object. */
  var thumbnails: List[Thumbnail] = Nil
  
  /** Indicates the type of media content. */
  var category: Option[Category] = None
  
  /** Hash of the binary media object. */
  var hash: Option[Hash] = None
  
  /** Allows the media object to be accessed through a web browser player. */
  var player: Option[Player] = None
  
  /** Contributors to the creation of this media object. */
  var credits: List[Credit] = Nil
  
  /** Copyright information. */
  var copyright: Option[Copyright] = None
  
  /** Additional text transcripts, captions or lyrics. */
  var text: List[Text] = Nil
  
  /** Restrictions on the aggregator rendering this media object. Purely informational. */
  var restrictions: List[Restriction] = Nil
  
  /** Content descriptions. */
  var contentEntries: List[Content] = Nil
  
  /** Fill own fields. */
  def fillOwnFields(rating: Option[Rating], title: Option[SimpleText], description: Option[SimpleText], keywords: List[String],
      thumbnails: List[Thumbnail], category: Option[Category], hash: Option[Hash], player: Option[Player], credits: List[Credit],
      copyright: Option[Copyright], text: List[Text], restrictions: List[Restriction], contentEntries: List[Content]): this.type = {
    this.rating = rating
    this.title = title
    this.description = description
    this.keywords = keywords
    this.thumbnails = thumbnails
    this.category = category
    this.hash = hash
    this.player = player
    this.credits = credits
    this.copyright = copyright
    this.text = text
    this.restrictions = restrictions
    this.contentEntries = contentEntries
    this
  }
  
  override def toString: String = {
    val sb = new StringBuilder(256) // override default size of the internal buffer
    sb.append("Group: \n\t")
    printOptional(sb, " rating", rating)
    printOptional(sb, " title", title)
    printOptional(sb, " description", description)
    sb.append(" keywords: ").append(keywords.mkString("", ", ", ""))
    sb.append(" thumbnails: ").append(thumbnails)
    printOptional(sb, " category", category)
    printOptional(sb, " hash", hash)
    printOptional(sb, " player", player)
    sb.append(" credits: ").append(credits.mkString("", ", ", ""))
    printOptional(sb, " copyright", copyright)
    sb.append(" text: ").append(text.mkString("", ", ", ""))
    sb.append(" restrictions: ").append(restrictions.mkString("", ", ", ""))
    sb.append(" content: ").append(contentEntries.mkString("", ", ", ""))
    sb.toString
  }
}


object Group {
  implicit val ns = mediaNs
  
  /** A pickler for the contents of a group. It has no enclosing 'media:group' element. */
  val contents: Pickler[Group] = 
    (wrap (interleaved(opt(Rating.pickler) ~ opt(SimpleText.pickler("title"))
        ~ opt(SimpleText.pickler("description")) ~ default(elem("keywords", Keywords.keywordsPickler), Nil) 
        ~ rep(Thumbnail.pickler) ~ opt(Category.pickler) ~ opt(Hash.pickler) ~ opt(Player.pickler)  
        ~ rep(Credit.pickler) ~ opt(Copyright.pickler) ~ rep(Text.pickler) ~ rep(Restriction.pickler)
        ~ rep(Content.pickler))) ({
      case rating ~ title ~ description ~ keywords ~ thumbnails ~ category ~ hash ~ player 
          ~ credits ~ copyright ~ text ~ restrictions ~ content =>
        (new Group).fillOwnFields(rating, title, description, keywords, thumbnails, category, hash,
            player, credits, copyright, text, restrictions, content)
    }) (fromGroup))
  
  /** A pickler for 'media:group' elements. */
  val pickler: Pickler[Group] = elem("group", contents)
  
  def fromGroup(g: Group) = (new ~(g.rating, g.title) ~ g.description ~ g.keywords ~ g.thumbnails ~ g.category
      ~ g.hash ~ g.player ~ g.credits ~ g.copyright ~ g.text ~ g.restrictions ~ g.contentEntries)
}
