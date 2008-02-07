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

import scala.xml._
import org.junit.{Test, Assert, Ignore}

/** This class tests simple XML pickler combinators.
 *
 *  @author Iulian Dragos (iuliandragos@google.com) 
 */
class PicklerTest extends Picklers with PicklerAsserts {
  
  final val URI = "testing-uri"

  val pprinter = new PrettyPrinter(80, 4)
    
  /** Return a string representation without unnecessary white-space.
   *  Useful when comparing XML documents.
   */
  private def normalize(n: Node) = Utility.trim(n).toString
    
  def pSeq2: Pickler[~[String, String]] = 
    elem("p", URI, "pair", 
        elem("p", URI, "a", text) ~ elem("p", URI, "b", text))
          
  val input =
    <p:pair xmlns:p="testing-uri">
      <p:a>alfa</p:a>
      <p:b>omega</p:b>
    </p:pair>
  val pair = new ~("alfa", "omega")

  @Test def testSequencePickle {
      val pickled = pSeq2.pickle(pair, PicklerState.empty)
      Assert.assertEquals("Sequence pickling failed", normalize(input), normalize(pickled.rootNode))
  }
  
  @Test def testSequenceUnpickle {
    assertSucceedsWith("Sequence unpickling failed",
        pair,
        pSeq2.unpickle(PicklerState.fromElem(input)))
  }
  
  def pSeq3: Pickler[String ~ String ~ String] =
    elem("p", URI, "triple",
        elem("p", URI, "a", text)
      ~ elem("p", URI, "b", text)
      ~ elem("p", URI, "c", text))
      
  val triple = new ~(new ~("alfa", "beta"), "gamma") 
  val inputTriple =
    <m:triple xmlns:m="testing-uri">
      <m:a>alfa</m:a>
      <m:b>beta</m:b>
      <m:c>gamma</m:c>
    </m:triple>
  
  @Test def testSequence3Unpickle {
    assertSucceedsWith("Sequence 3 unpickling failed", 
        triple,
        pSeq3.unpickle(PicklerState.fromElem(inputTriple)))
  }
  
  def pPermute2: Pickler[String ~ String] =
    elem("p", URI, "set2",
      permute(elem("p", URI, "a", text),
              elem("p", URI, "b", text)))
      
  @Test def testPermute2Unpickle {
    val perm1 = 
      <p:set2 xmlns:p="testing-uri">
        <p:a>alfa</p:a>
        <p:b>omega</p:b>
      </p:set2>
    assertSucceedsWith("Permutation of 2 failed unpickling",
        pair,
        pPermute2.unpickle(PicklerState.fromElem(perm1)))
  }
  
  @Test def testPermute2Unpickle1 {
    val perm1 = 
      <p:set2 xmlns:p="testing-uri">
        <p:b>omega</p:b>
        <p:a>alfa</p:a>
      </p:set2>
    assertSucceedsWith("Permutation of 2 failed unpickling",
        pair,
        pPermute2.unpickle(PicklerState.fromElem(perm1)))
  }

  def pPermute3: Pickler[String ~ String ~ String] =
    elem("p", URI, "set3",
        elem("p", URI, "a", text)
      * elem("p", URI, "b", text)
      * elem("p", URI, "c", text))
            
  @Test def testPermute3Unpickle {
    val perm1 = 
      <p:set3 xmlns:p="testing-uri">
        <p:a>alfa</p:a>
        <p:b>beta</p:b>
        <p:c>gamma</p:c>
      </p:set3>
    assertSucceedsWith("Permutation of 2 failed unpickling",
        triple,
        pPermute3.unpickle(PicklerState.fromElem(perm1)))
  }

  @Test def testPermute3Unpickle1 {
    val perm1 = 
      <p:set3 xmlns:p="testing-uri">
        <p:b>beta</p:b>
        <p:a>alfa</p:a>
        <p:c>gamma</p:c>
      </p:set3>
    assertSucceedsWith("Permutation of 2 failed unpickling",
        triple,
        pPermute3.unpickle(PicklerState.fromElem(perm1)))
  }

  @Test def testPermute3Unpickle2 {
    val perm1 = 
      <p:set3 xmlns:p="testing-uri">
        <p:c>gamma</p:c>
        <p:b>beta</p:b>
        <p:a>alfa</p:a>
      </p:set3>
    assertSucceedsWith("Permutation of 2 failed unpickling",
        triple,
        pPermute3.unpickle(PicklerState.fromElem(perm1)))
  }

  @Test def testPermute3Unpickle3 {
    val perm1 = 
      <p:set3 xmlns:p="testing-uri">
        <p:c>gamma</p:c>
        <p:a>alfa</p:a>
        <p:b>beta</p:b>
      </p:set3>
    assertSucceedsWith("Permutation of 2 failed unpickling",
        triple,
        pPermute3.unpickle(PicklerState.fromElem(perm1)))
  }

  @Ignore // Permutation parsing not yet fully working 
  @Test def testPermute3Unpickle4 {
    val perm1 = 
      <p:set3 xmlns:p="testing-uri">
        <p:a>alfa</p:a>
        <p:c>gamma</p:c>
        <p:b>beta</p:b>
      </p:set3>
    assertSucceedsWith("Permutation of 3 failed unpickling",
        triple,
        pPermute3.unpickle(PicklerState.fromElem(perm1)))
  }

  def pStrings = elem("p", URI, "strings", rep(elem("p", URI, "str", text)))
  
  @Test def testRepetition0Unpickle {
    val inputRep = 
      <p:strings xmlns:p="testing-uri">
      </p:strings>
      
    val strings = List()
    assertSucceedsWith("Repetition with empty sequence failed",
        strings,
        pStrings.unpickle(PicklerState.fromElem(inputRep)))
  }

  @Test def testRepetition1Unpickle {
    val inputRep = 
      <p:strings xmlns:p="testing-uri">
        <p:str>one</p:str>
      </p:strings>
      
    val strings = List("one")
    assertSucceedsWith("Repetition with one element failed",
        strings,
        pStrings.unpickle(PicklerState.fromElem(inputRep)))
  }
  
  @Test def testRepetition3Unpickle {
    val inputRep = 
      <p:strings xmlns:p="testing-uri">
        <p:str>one</p:str>
        <p:str>two</p:str>
        <p:str>three</p:str>
      </p:strings>
      
    val strings = List("one", "two", "three")
    assertSucceedsWith("Repetition failed",
        strings,
        pStrings.unpickle(PicklerState.fromElem(inputRep)))
  }
  
  @Test def testRepetition0Pickle {
    val inputRep = 
      <p:strings xmlns:p="testing-uri">
      </p:strings>
      
    val strings = List()
    val pickled = pStrings.pickle(strings, PicklerState.empty)
    Assert.assertEquals("Empty repetition pickling", normalize(inputRep), normalize(pickled.rootNode))
  }

  @Test def testRepetition1Pickle {
    val inputRep = 
      <p:strings xmlns:p="testing-uri">
        <p:str>one</p:str>
      </p:strings>
      
    val strings = List("one")
    val pickled = pStrings.pickle(strings, PicklerState.empty)
    Assert.assertEquals("Repetition of 1 element, pickling", normalize(inputRep), normalize(pickled.rootNode))
  }

  @Test def testRepetition3Pickle {
    val inputRep = 
      <p:strings xmlns:p="testing-uri">
        <p:str>one</p:str>
        <p:str>two</p:str>
        <p:str>three</p:str>
      </p:strings>
      
    val strings = List("one", "two", "three")
    val pickled = pStrings.pickle(strings, PicklerState.empty)
    Assert.assertEquals("Repetition of 3 elements, pickling", normalize(inputRep), normalize(pickled.rootNode))
  }
}

