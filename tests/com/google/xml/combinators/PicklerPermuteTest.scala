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

import scala.xml.{NamespaceBinding, TopScope}

/**
 * Test permutation parsers.
 *
 * @author Iulian Dragos (iuliandragos@google.com)
 */
class PicklerPermuteTest extends PicklerAsserts {
  import Picklers._
  
  final val URI = "testing-uri"
  implicit val namespace = new NamespaceBinding("p", URI, TopScope)
  
  def pPermute2: Pickler[String ~ String] =
    interleaved("set2", elem("a", text) ~ elem("b", text))
      
  val pair = new ~("alfa", "omega")
  val triple = new ~(new ~("alfa", "beta"), "gamma") 

  @Test def testPermute2Unpickle {
    val perm1 = 
      <p:set2 xmlns:p="testing-uri">
        <p:a>alfa</p:a>
        <p:b>omega</p:b>
      </p:set2>
    assertSucceedsWith("Permutation of 2 failed unpickling",
        pair,
        pPermute2.unpickle(LinearStore.fromElem(perm1)))
  }
  
  @Test def testPermute2Unpickle1 {
    val perm1 = 
      <p:set2 xmlns:p="testing-uri">
        <p:b>omega</p:b>
        <p:a>alfa</p:a>
      </p:set2>
    assertSucceedsWith("Permutation of 2 failed unpickling",
        pair,
        pPermute2.unpickle(LinearStore.fromElem(perm1)))
  }

  def pPermute3: Pickler[String ~ String ~ String] =
    interleaved("set3", 
        elem("a", text)
      ~ elem("b", text)
      ~ elem("c", text))
            
  @Test def testPermute3UnpickleAbc {
    val perm1 = 
      <p:set3 xmlns:p="testing-uri">
        <p:a>alfa</p:a>
        <p:b>beta</p:b>
        <p:c>gamma</p:c>
      </p:set3>
    assertSucceedsWith("Permutation of 2 failed unpickling",
        triple,
        pPermute3.unpickle(LinearStore.fromElem(perm1)))
  }

  @Test def testPermute3UnpickleAcb {
    val perm1 = 
      <p:set3 xmlns:p="testing-uri">
        <p:a>alfa</p:a>
        <p:c>gamma</p:c>
        <p:b>beta</p:b>
      </p:set3>
    assertSucceedsWith("Permutation of 3 failed unpickling",
        triple,
        pPermute3.unpickle(LinearStore.fromElem(perm1)))
  }

  @Test def testPermute3UnpickleBac {
    val perm1 = 
      <p:set3 xmlns:p="testing-uri">
        <p:b>beta</p:b>
        <p:a>alfa</p:a>
        <p:c>gamma</p:c>
      </p:set3>
    assertSucceedsWith("Permutation of 2 failed unpickling",
        triple,
        pPermute3.unpickle(LinearStore.fromElem(perm1)))
  }
  
  @Test def testPermute3UnpickleBca {
    val perm1 = 
      <p:set3 xmlns:p="testing-uri">
        <p:b>beta</p:b>
        <p:c>gamma</p:c>
        <p:a>alfa</p:a>
      </p:set3>
    assertSucceedsWith("Permutation of 2 failed unpickling",
        triple,
        pPermute3.unpickle(LinearStore.fromElem(perm1)))
  }
  
  @Test def testPermute3UnpickleCab {
    val perm1 = 
      <p:set3 xmlns:p="testing-uri">
        <p:c>gamma</p:c>
        <p:a>alfa</p:a>
        <p:b>beta</p:b>
      </p:set3>
    assertSucceedsWith("Permutation of 2 failed unpickling",
        triple,
        pPermute3.unpickle(LinearStore.fromElem(perm1)))
  }

  @Test def testPermute3UnpickleCba {
    val perm1 = 
      <p:set3 xmlns:p="testing-uri">
        <p:c>gamma</p:c>
        <p:b>beta</p:b>
        <p:a>alfa</p:a>
      </p:set3>
    assertSucceedsWith("Permutation of 2 failed unpickling",
        triple,
        pPermute3.unpickle(LinearStore.fromElem(perm1)))
  }

  def pa: Pickler[String] = elem("p", URI, "a", text)
  def pb: Pickler[String] = elem("p", URI, "b", text)
  def pc: Pickler[String] = elem("p", URI, "c", text)
  def pd: Pickler[String] = elem("p", URI, "d", text)
  def pe: Pickler[String] = elem("p", URI, "e", text)
  
  def pDPermuteE = elem("p", URI, "elems", pd ~ interleaved("inner", pa ~ pb ~ pc) ~ pe)
  val objAbc = new ~(new ~("a", "b"), "c")
  val obj = new ~(new ~("d", objAbc), "e") 
  
  @Test def testSeqPermutePickleUnpickle {
    val input = 
      <p:elems xmlns:p="testing-uri">
        <p:d>d</p:d>
        <p:inner>
          <p:a>a</p:a>
          <p:b>b</p:b>
          <p:c>c</p:c>
        </p:inner>
        <p:e>e</p:e>
      </p:elems>
      
    val pickled = pDPermuteE.pickle(obj, LinearStore.empty).rootNode
    Assert.assertEquals("Sequence and permutation failed pickling.", normalize(input), normalize(pickled))
    assertSucceedsWith("Sequence and permutation failed unpickling",
        obj,
        pDPermuteE.unpickle(LinearStore.fromElem(input)))
  }

  @Test def testSeqPermuteUnpickleCab {
    val input = 
      <p:elems xmlns:p="testing-uri">
        <p:d>d</p:d>
        <p:inner>
          <p:c>c</p:c>
          <p:a>a</p:a>
          <p:b>b</p:b>
        </p:inner>
        <p:e>e</p:e>
      </p:elems>
      
    assertSucceedsWith("Sequence and permutation failed unpickling",
        obj,
        pDPermuteE.unpickle(LinearStore.fromElem(input)))
  }

  @Test def testSeqPermuteUnpickleCabExtra {
    val input = 
      <p:elems xmlns:p="testing-uri">
        <p:d>d</p:d>
        <p:inner>
          <p:c>c</p:c>
          <p:a>a</p:a>
          <p:b>b</p:b>
        </p:inner>
        <p:e>e</p:e>
        <extra>This tag should be ignored</extra>
      </p:elems>
      
    assertSucceedsWith("Sequence and permutation with unknown elements failed unpickling",
        obj,
        pDPermuteE.unpickle(LinearStore.fromElem(input)))
  }
}
