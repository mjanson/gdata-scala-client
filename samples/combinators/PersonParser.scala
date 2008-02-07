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


package samples.combinators

import com.google.xml.combinators._
import scala.xml._

case class Person(name: String, address: String, tpe: String)

/** Simple example of using XML Pickler combinators
 *  
 *  @author Iulian Dragos (iuliandragos@google.com) 
 */
object PersonParser extends Picklers with Application {
  import Picklers._
  
  final val URI = "persons-uri"

  /** A pickler for Person. The 'elem' and 'attr' methods define the XML structure.
   *  The library handles already the '~' type (@see ~), but does not know about our Person
   *  class. We use 'wrap' to provide a deconstructor and constructor of Person to/from the 
   *  ~ objects. The same '~' is used as an operator for constructing sequences, denoting that
   *  the 'name' element should be followed by an 'address' element. The basic 'text' pickler
   *  handles plain text nodes. Hopefully, the new release of Scala (2.7.0) will improve 
   *  the inferencer and make some code below not necessary.
   */
  def person: Pickler[Person] = 
    (wrap ({p: Person => new ~(p.name, new ~(p.address, p.tpe))}) (fun3ToPpairR(Person))
          (elem("p", URI, "person", 
              elem("p", URI, "name", text)
            ~ elem("p", URI, "address", text ~ attr("p", URI, "type", text)))))

  val input = 
    <p:person xmlns:p="persons-uri">
      <p:name>Cartman</p:name>
      <p:address p:type="home">South Park, 90210</p:address>
    </p:person>

  val pp = new PrettyPrinter(80, 4)

  val pickled = person.pickle(Person("Kenny", "Southpark, 90211", "home"), emptySt)
  println(pp.format(pickled.rootNode, pickled.ns))

  person.unpickle(PicklerState.fromElem(input)) match {
    case Success(v, _) => 
      println("Got: " + v)
    case Failure(msg) =>
      println("error: " + msg)
  }
    
}

