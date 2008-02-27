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

   
package com.google.xml.combinators

import scala.xml.PrettyPrinter
import org.junit.{Test, Assert, Ignore}

/** 
 * This class tests simple XML pickler combinators.
 *
 * @author Iulian Dragos (iuliandragos@google.com) 
 */
class PicklerTest extends PicklerAsserts {
  import Picklers._
  
  final val URI = "testing-uri"

  val pprinter = new PrettyPrinter(80, 4)
    
  def pSeq2: Pickler[~[String, String]] = 
    elem("p", URI, "pair", 
        elem("p", URI, "a", text) ~ elem("p", URI, "b", text))
          
  val input =
    (<p:pair xmlns:p="testing-uri">
       <p:a>alfa</p:a>
       <p:b>omega</p:b>
     </p:pair>)
  val pair = new ~("alfa", "omega")

  @Test def testSequencePickle {
      val pickled = pSeq2.pickle(pair, LinearStore.empty)
      Assert.assertEquals("Sequence pickling failed", normalize(input), normalize(pickled.rootNode))
  }
  
  @Test def testSequenceUnpickle {
    assertSucceedsWith("Sequence unpickling failed", pair, input, pSeq2)
  }
  
  def pSeq3: Pickler[String ~ String ~ String] =
    elem("p", URI, "triple",
        elem("p", URI, "a", text)
      ~ elem("p", URI, "b", text)
      ~ elem("p", URI, "c", text))
      
  val triple = new ~(new ~("alfa", "beta"), "gamma") 
  val inputTriple =
    (<m:triple xmlns:m="testing-uri">
       <m:a>alfa</m:a>
       <m:b>beta</m:b>
       <m:c>gamma</m:c>
     </m:triple>)
  
  @Test def testSequence3Unpickle {
    assertSucceedsWith("Sequence 3 unpickling failed", triple, inputTriple, pSeq3)
  }

  def pStrings = elem("p", URI, "strings", rep(elem("p", URI, "str", text)))
  
  @Test def testRepetition0Unpickle {
    val inputRep = (<p:strings xmlns:p="testing-uri"></p:strings>)
      
    val strings = List()
    assertSucceedsWith("Repetition with empty sequence failed",
        strings, inputRep, pStrings)
  }

  @Test def testRepetition1Unpickle {
    val inputRep = 
      (<p:strings xmlns:p="testing-uri">
         <p:str>one</p:str>
       </p:strings>)
      
    val strings = List("one")
    assertSucceedsWith("Repetition with one element failed",
        strings, inputRep, pStrings)
  }
  
  @Test def testRepetition3Unpickle {
    val inputRep = 
      (<p:strings xmlns:p="testing-uri">
         <p:str>one</p:str>
         <p:str>two</p:str>
         <p:str>three</p:str>
       </p:strings>)

    val strings = List("one", "two", "three")
    assertSucceedsWith("Repetition failed", strings, inputRep, pStrings)
  }
  
  @Test def testRepetition0Pickle {
    val inputRep = (<p:strings xmlns:p="testing-uri"></p:strings>)
      
    val strings = List()
    val pickled = pStrings.pickle(strings, LinearStore.empty)
    Assert.assertEquals("Empty repetition pickling", normalize(inputRep), normalize(pickled.rootNode))
  }

  @Test def testRepetition1Pickle {
    val inputRep = 
      (<p:strings xmlns:p="testing-uri">
         <p:str>one</p:str>
       </p:strings>)
      
    val strings = List("one")
    val pickled = pStrings.pickle(strings, LinearStore.empty)
    Assert.assertEquals("Repetition of 1 element, pickling", normalize(inputRep), normalize(pickled.rootNode))
  }

  @Test def testRepetition3Pickle {
    val inputRep = 
      (<p:strings xmlns:p="testing-uri">
         <p:str>one</p:str>
         <p:str>two</p:str>
         <p:str>three</p:str>
       </p:strings>)
      
    val strings = List("one", "two", "three")
    val pickled = pStrings.pickle(strings, LinearStore.empty)
    Assert.assertEquals("Repetition of 3 elements, pickling", normalize(inputRep), normalize(pickled.rootNode))
  }
}

