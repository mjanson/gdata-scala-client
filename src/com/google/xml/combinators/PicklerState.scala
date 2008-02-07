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

/** PicklerState encapsulate the state carried around
 *  when pickling or unpickling XML. This is an immutable data structure.
 *  Speaking from the point of view of unpickling, the state consists of a
 *  set of attributes not yet consumed, a set of nodes not yet consumed and
 *  a set of namespace bindings encountered so far.
 *
 * @author Iulian Dragos (iuliandragos@google.com)
 */
class PicklerState(
  val attrs: MetaData,
  val nodes: List[Node],
  val ns:    NamespaceBinding) {

  /** Return a new PicklerState with a new attribute prepended to the list of attrs */
  def addAttribute(pre: String, key: String, value: String): PicklerState =
    PicklerState(new PrefixedAttribute(pre, key, value, attrs), nodes, ns)

  /** Return a new PicklerState with a new namespace binding. If the 
   *  prefix is already defined to the given URI, it returns the 
   *  current object.
   */
  def addNamespace(pre: String, uri: String): PicklerState = 
    if (ns.getURI(pre) == uri) 
      this 
    else {
      PicklerState(attrs, nodes, new NamespaceBinding(pre, uri, ns))  
    }
  
  /** Add a text node */
  def addText(s: String): PicklerState =
    addNode(Text(s))

  /** Add a node. */
  def addNode(n: Node): PicklerState = 
    PicklerState(attrs, n :: nodes, ns)

  /** Drop 'n' nodes from the list of nodes. */
  def dropNodes(n: Int): PicklerState = 
    PicklerState(attrs, nodes.drop(n), ns)

  /** Return the root element of the constructed XML fragment. 
   *  It always returns the first node in the list of nodes. It
   *  throws an error if there are top-level attributes.
   */
  def rootNode: Elem = {
    val root = nodes.head
    if (attrs.isEmpty && root.isInstanceOf[Elem]) {
      root.asInstanceOf[Elem]
    } else
      throw new MalformedPicklerState("Top-level attributes found hanging: " + attrs, this)
  }

  override def toString = "(" + attrs + ", " + nodes.mkString("", ",", "") + ", " + ns + ")"
}

case class MalformedPicklerState(msg: String, state: PicklerState) extends RuntimeException(msg)

/** Convenience object for creating PicklerStates
 *
 * @author Iulian Dragos
 */
object PicklerState {
  /** Return an empty pickler state. */
  def empty: PicklerState = empty(TopScope)
  
  /** Return an empty pickler state with a given namespace scope. */
  def empty(ns: NamespaceBinding) = PicklerState(Null, Nil, ns)

  /** Create a new PicklerState with the given state.*/
  def apply(attrs: MetaData, nodes: List[Node], ns: NamespaceBinding) = 
    new PicklerState(attrs, nodes, ns)
    
  /** Create a PicklerState from an XML element. */
  def fromElem(e: Elem) = 
    PicklerState(Null, Utility.trimProper(e).toList, TopScope)
}
