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


package com.google.xml.combinators;

import com.google.gdata.data.{Atom, Uris, Person, Text}
import org.junit._

/**
 * This class tests picklers for some GData core types.
 */
class GDataCoreTest extends PicklerAsserts {
  import Picklers._
  import Atom._
  import Uris._

  
  val input = 
    <atom:feed xmlns:atom = "http://www.w3.org/2005/Atom">
      <atom:title type="text">This is the title I want to see</atom:title>
    </atom:feed>
    
  @Test def testTitleFeedU {
    val pTitle = elem("atom", ATOM, "feed", atomText("title"))
    assertSucceedsWith("Failed unpickling atom text", 
        Text(Some("text"), "This is the title I want to see"),
        pTitle.unpickle(LinearStore.fromElem(input)))
  }
  
  @Test def testPersonFeedU {
    val input = 
      <atom:feed xmlns:atom="http://www.w3.org/2005/Atom" xml:lang="en">
        <atom:name>Kenny</atom:name>
        <atom:email>kenny@southpark.org</atom:email>
      </atom:feed>
      
      val pPerson = atomPerson("feed")
      assertSucceedsWith("Failed unpickling person",
          Person("Kenny", None, Some("kenny@southpark.org")),
          pPerson.unpickle(LinearStore.fromElem(input)))
  }
  
  @Test def testExtendedPersonU {
    val input = 
      <atom:person xmlns:atom="http://www.w3.org/2005/Atom" xml:lang="en">
        <atom:name>Kenny</atom:name>
        <atom:email>kenny@southpark.org</atom:email>
        <atom:im type="yahoo">kenny@yahoomessenger.com</atom:im>
      </atom:person>
      
      val pPerson = extend(atomPerson("person"), elem("im", text))
      assertSucceedsWith("Failed unpickling person",
          new ~(Person("Kenny", None, Some("kenny@southpark.org")), 
              "kenny@yahoomessenger.com"),
          pPerson.unpickle(LinearStore.fromElem(input)))
  }
  
}
