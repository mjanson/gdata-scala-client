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


package com.google.gdata.youtube;

import com.google.gdata.data.{AtomEntries, Entries}
import com.google.gdata.data.media.{Text, Rating, Group}
import com.google.xml.combinators.{~}
import com.google.xml.combinators.Picklers._
 
trait MediaEntries extends AtomEntries {  
  type Entry <: MediaEntry
  
  class MediaEntry extends AtomEntry {
    var media: Group = _ 
    
    def fillOwnFields(media: Group): this.type = {
      this.media = media
      this
    }
    
    def fromMediaEntry(me: MediaEntry) {
      this.fromAtomEntry(me)
      fillOwnFields(me.media)
    }
    
    override def toString = {
      super.toString + " media: " + media
    }
  }
  
  def mediaEntryPickler: Pickler[MediaEntry] =
    wrap (atomEntryPickler ~ Group.pickler) ({
      case ae ~ media => 
        val me = new MediaEntry
        me.fromAtomEntry(ae)
        me.fillOwnFields(media)
    }) (fromMediaEntry)
    
  def fromMediaEntry(me: MediaEntry) = new ~(me, me.media)
}
