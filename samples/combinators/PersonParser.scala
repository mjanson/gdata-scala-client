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

import scala.xml.combinators._
import scala.xml._

case class Person(name: String, address: String, tpe: String)

/** Simple example of using XML Pickler combinators
 *  
 *  @author Iulian Dragos (iuliandragos@google.com) 
 */
object PersonParser extends Picklers with Application {
  import Picklers._
  
  final val URI = "persons-uri"

  def person: Pickler[Person] = 
    (wrap ({p: Person => new ~(p.name, new ~(p.address, p.tpe))}) (Person) 
          (elem("p", URI, "person", 
              permute(elem("p", URI, "name", text),
                      elem("p", URI, "address", text ~ attr("p", URI, "type", text))))))

  val input = 
    <p:person xmlns:p="persons-uri">
      <p:name>Iulian Dragos</p:name>
      <p:address p:type="home">Av. de la Rochelle, 10, Prilly</p:address>
    </p:person>

  val pp = new PrettyPrinter(80, 4)

  def pSeq2: Pickler[~[String, String]] = 
    elem("p", URI, "pair", 
        elem("p", URI, "a", text) ~ elem("p", URI, "b", text))
          
  val inputPair =
    <p:pair xmlns:p="testing-uri">
      <p:a>alfa</p:a>
      <p:b>omega</p:b>
    </p:pair>
  val pair = new ~("alfa", "omega")

  def testSequencePickle {
      val pickled = pSeq2.pickle(pair, PicklerState.empty)
      println(pp.format(pickled.rootNode))
  }
  
  testSequencePickle
  System.exit(1)
  
  val pickled = person.pickle(Person("Iulian Dragos", "Av. de la Rochelle, 10", "home"), emptySt)
  println("namespaces: " + pickled.ns)
  println(pp.format(pickled.rootNode, pickled.ns))

  person.unpickle(PicklerState.fromElem(input)) match {
    case Success(v, _) => 
      println("Got: " + v)
    case Failure(msg) =>
      println("error: " + msg)
  }
    
}

