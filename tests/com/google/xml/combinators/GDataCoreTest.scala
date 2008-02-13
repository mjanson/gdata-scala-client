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

import org.junit._

/**
 * This class tests picklers for some GData core types.
 */
class GDataCoreTest extends Picklers with PicklerAsserts {
  final val ATOM_URI = "http://www.w3.org/2005/Atom"
  
  case class Text(tpe: String, content: String)
  
  def atomText(elemName: String): Pickler[Text] =
    (wrap ({t: Text => new ~(t.tpe, t.content)}) (Text) 
        (elem("atom", ATOM_URI, elemName, 
          attr("atom", ATOM_URI, "type",  text)
        ~ text)))
  
  case class Person(name: String, uri: Option[String], email: Option[String])
  
  def atomPerson: Pickler[Person] = 
    (wrap ( {p: Person => new ~ (new ~ (p.name, p.uri), p.email)})
          (Person)  
          (elem("atom", ATOM_URI, "name", text)
     ~ opt(elem("atom", ATOM_URI, "uri", text))
     ~ opt(elem("atom", ATOM_URI, "email", text))))
        
  val input = 
    <atom:feed xmlns:atom = "http://www.w3.org/2005/Atom">
      <atom:title atom:type="text">This is the title I want to see</atom:title>
    </atom:feed>
    
  @Test def testTitleFeedU {
    val pTitle = elem("atom", ATOM_URI, "feed", atomText("title"))
    assertSucceedsWith("Failed unpickling atom text", 
        Text("text", "This is the title I want to see"),
        pTitle.unpickle(LinearStore.fromElem(input)))
  }
  
  @Test def testPersonFeedU {
    val input = 
      <atom:feed xmlns:atom="http://www.w3.org/2005/Atom">
        <atom:name>Kenny</atom:name>
        <atom:email>kenny@southpark.org</atom:email>
      </atom:feed>
      
      val pPerson = elem("atom", ATOM_URI, "feed", atomPerson)
      assertSucceedsWith("Failed unpickling person",
          Person("Kenny", None, Some("kenny@southpark.org")),
          pPerson.unpickle(LinearStore.fromElem(input)))
  }
}
