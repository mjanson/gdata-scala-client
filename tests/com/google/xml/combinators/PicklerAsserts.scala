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

import org.junit.Assert
import scala.xml.{Node, Utility}

/**
 * This trait defines specialized asserts for testing picklers.
 *
 * @author Iulian Dragos (iuliandragos@google.com)
 */
trait PicklerAsserts {
  import Picklers._
   
  def assertSucceedsWith[A](name: String, expected: A, result: PicklerResult[A]) {
    result match {
      case Success(v, _) => Assert.assertEquals(name, expected, v)
      case f: NoSuccess  => Assert.fail(f.toString)
    }
  }
  
  /**
   * Return a string representation without unnecessary white-space.
   * Useful when comparing XML documents.
   */
  def normalize(n: Node) = Utility.trim(n).toString
}
