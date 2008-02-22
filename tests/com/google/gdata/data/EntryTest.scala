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

import com.google.xml.combinators.{Picklers, PicklerAsserts, LinearStore}
import scala.xml.NamespaceBinding
import scala.xml.Utility

import org.junit._

class EntryTest extends PicklerAsserts {
  import Picklers._
  
  val inputEntry =
    <entries>
    <entry xmlns="http://www.w3.org/2005/Atom">
      <title>Atom-Powered Robots Run Amok</title>
      <link href="http://example.org/2003/12/13/atom03"/>
      <link href="http://example.org/2003/12/13/atom04"/>
      <link href="http://example.org/2003/12/13/atom05"/>
      <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
      <updated>2003-12-13T18:30:02Z</updated>
      <summary>Some text.</summary>
    </entry>
    <entry xmlns="http://www.w3.org/2005/Atom">
      <title>Atom-Powered Robots Run Amok</title>
      <link href="http://example.org/2003/12/13/atom03"/>
      <link href="http://example.org/2003/12/13/atom04"/>
      <link href="http://example.org/2003/12/13/atom05"/>
      <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
      <updated>2003-12-13T18:30:02Z</updated>
      <summary>Some text.</summary>
    </entry>
    <entry xmlns="http://www.w3.org/2005/Atom">
      <title>Atom-Powered Robots Run Amok</title>
      <link href="http://example.org/2003/12/13/atom03"/>
      <link href="http://example.org/2003/12/13/atom04"/>
      <link href="http://example.org/2003/12/13/atom05"/>
      <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
      <updated>2003-12-13T18:30:02Z</updated>
      <summary>Some text.</summary>
    </entry>
    <entry xmlns="http://www.w3.org/2005/Atom">
      <title>Atom-Powered Robots Run Amok</title>
      <link href="http://example.org/2003/12/13/atom03"/>
      <link href="http://example.org/2003/12/13/atom04"/>
      <link href="http://example.org/2003/12/13/atom05"/>
      <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
      <updated>2003-12-13T18:30:02Z</updated>
      <summary>Some text.</summary>
    </entry>
    <entry xmlns="http://www.w3.org/2005/Atom">
      <title>Atom-Powered Robots Run Amok</title>
      <link href="http://example.org/2003/12/13/atom03"/>
      <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
      <updated>2003-12-13T18:30:02Z</updated>
      <summary>Some text.</summary>
    </entry>
    </entries>
    
  import Picklers._
  
  object atomEntry extends Object with AtomEntries {
    implicit val atomNs = new NamespaceBinding("atom", Uris.ATOM, scala.xml.TopScope)
    type Entry = AtomEntry
    def entryPickler = elem("entry", atomEntryPickler)
  }
    
  @Test def testEntry1 {
    
    rep(atomEntry.entryPickler).unpickle(LinearStore.enterElem(inputEntry)) match {
      case Success(v, in1) => 
        println(v.length) 
        
        println((new scala.xml.PrettyPrinter(80, 2)).format(rep(atomEntry.entryPickler).pickle(v, LinearStore.empty).rootNode))
      case f  => println(f)
    }
  }
}

object Test extends EntryTest with Application {
  testEntry1
}
