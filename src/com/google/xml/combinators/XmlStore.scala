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
import scala.collection.mutable.ListBuffer

trait XmlStore {
   protected[combinators] def attrs: MetaData
   protected[combinators] def nodes: List[Node]
   def ns:    NamespaceBinding
   
  /**
   * Accept the given element, or fail. Succeeds when the given element is the head of the node
   * list. Comments, processing instructions and entity references count (they are not skipped). 
   */
  def acceptElem(Label: String, uri: String): (Option[Node], XmlStore)

  /**
   * Accept the given attribute, or fail. Succeeds when the given attribute exists (order does
   * not matter). Returns a Seq[Node], since attributes may contain text nodes interspersed 
   * with entity references.
   */
  def acceptAttr(label: String, uri: String): (Option[Seq[Node]], XmlStore)
  
  /** Accept a text node. Fails if the head of the node list is not a text node. */
  def acceptText: (Option[Text], XmlStore)

  /** Return a new LinearStore with a new attribute prepended to the list of attrs */
  def addAttribute(pre: String, key: String, value: String): XmlStore
   
  /**
   * Return a new LinearStore with a new namespace binding. If the 
   * prefix is already defined to the given URI, it returns the 
   * current object.
   */
  def addNamespace(pre: String, uri: String): XmlStore

  /** Add a text node */
  def addText(s: String): XmlStore =
    addNode(Text(s))

  /** Add a node. */
  def addNode(n: Node): XmlStore = 
    mkState(attrs, n :: nodes, ns)

  /**
   * Return the root element of the constructed XML fragment. 
   * It always returns the first node in the list of nodes. It
   * throws an error if there are top-level attributes.
   */
  def rootNode: Elem = {
    val root = nodes.head
    if (attrs.isEmpty && root.isInstanceOf[Elem]) {
      root.asInstanceOf[Elem]
    } else
      throw new MalformedXmlStore("Top-level attributes found hanging: " + attrs, this)
  }
   
  protected[combinators] def toLinear: XmlStore
   
  protected def mkState(attrs: MetaData, nodes: List[Node], ns: NamespaceBinding): XmlStore 
}

/**
 * This class encapsulate the state carried around
 * when pickling or unpickling XML. This is an immutable data structure.
 * Speaking from the point of view of unpickling, the store consists of a
 * set of attributes not yet consumed, a set of nodes not yet consumed and
 * a set of namespace bindings encountered so far.
 *
 * @author Iulian Dragos (iuliandragos@google.com)
 */
class LinearStore(ats: MetaData, nods: List[Node], bindings: NamespaceBinding) extends XmlStore {
  
  def attrs = ats
  def nodes = nods
  def ns    = bindings
  
  /**
   * Accept the given element, or fail. Succeeds when the given element is the head of the node
   * list. Comments, processing instructions and entity references count (they are not skipped). 
   */
  def acceptElem(Label: String, uri: String): (Option[Node], LinearStore) = {
    if (nodes.isEmpty) (None, this)
    else nodes.head match {
      case e @ Elem(_, Label, _, scope, _*) if (e.namespace ==  uri) => 
        (Some(e), mkState(attrs, nodes.tail, ns))
      case _ => (None, this)
    }
  }
  
  /**
   * Accept the given attribute, or fail. Succeeds when the given attribute exists (order does
   * not matter). Returns a Seq[Node], since attributes may contain text nodes interspersed 
   * with entity references.
   */
  def acceptAttr(label: String, uri: String): (Option[Seq[Node]], LinearStore) = {
    if (attrs.isEmpty) (None, this)
    else attrs(uri, ns, label) match {
      case null  => (None, this)
      case contents =>
        (Some(contents), mkState(attrs.remove(uri, ns, label), nodes, ns))
    }
  }
  
  /** Accept a text node. Fails if the head of the node list is not a text node. */
  def acceptText: (Option[Text], LinearStore) = {
    if (nodes.isEmpty) (None, this)
    else nodes.head match {
      case t: Text => (Some(t), mkState(attrs, nodes.tail, ns))
      case _       => (None, this)
    }
  }

  /** Return a new LinearStore with a new attribute prepended to the list of attrs */
  def addAttribute(pre: String, key: String, value: String): LinearStore =
    mkState(new PrefixedAttribute(pre, key, value, attrs), nodes, ns)

  /**
   * Return a new LinearStore with a new namespace binding. If the 
   * prefix is already defined to the given URI, it returns the 
   * current object.
   */
  def addNamespace(pre: String, uri: String): LinearStore = 
    if (ns.getURI(pre) == uri) 
      this 
    else {
      mkState(attrs, nodes, new NamespaceBinding(pre, uri, ns))  
    }

  protected[combinators] def toLinear: LinearStore = this
  
  protected def mkState(attrs: MetaData, nodes: List[Node], ns: NamespaceBinding) = 
    LinearStore(attrs, nodes, ns)
  
  override def toString = "LinearStore(" + attrs + ", " + nodes.mkString("", ",", "") + ", " + ns + ")"
}

case class MalformedXmlStore(msg: String, state: XmlStore) extends RuntimeException(msg)

/**
 * This class matches elements at any position in the sequence of nodes. This allows
 * unpicklers to accept any permutation of a defined sequence. For efficiency 
 * reasons, this class uses a mutable representation for elements.
 */
class RandomAccessStore(myAttrs: MetaData, myNodes: List[Node], myNs: NamespaceBinding) extends 
           LinearStore(myAttrs, myNodes, myNs) {
  import collection.mutable.{Set, Map, HashMap, MultiMap}
  
  private val nodeMap = 
    new HashMap[String, Set[Node]] with MultiMap[String, Node]
    
  for (val n <- myNodes) nodeMap.add(n.label, n)
  
  def this(underlying: XmlStore) = 
    this(underlying.attrs, underlying.nodes, underlying.ns)
  
  /**
   * Lookup the given element, based on label and URI. It uses the node map to efficiently 
   * perform lookups and removal.
   */
  override def acceptElem(label: String, uri: String): (Option[Node], RandomAccessStore) = {
    for (val elems <- nodeMap.get(label);
         val n     <- elems)
      n match {
        case e: Elem if (e.namespace == uri) => 
          nodeMap.remove(label, e)
          return (Some(e), this)
      case _ => ()
    }
    println("Could not find " + label + " in " + nodeMap)
    (None, this)
  }
  
/*  override def acceptElem(label: String, uri: String) = {
    val xs: ListBuffer[Node] = new ListBuffer
    var res: Option[Node] = None
    
    for (val n <- nodes) n match {
      case e: Elem if (e.label == label) && (e.namespace == uri) =>
        res = Some(e)
        e +: xs
      case n => n +: xs
    }
    (res, new RandomAccessStore(LinearStore(attrs, xs.toList, ns)))
  }
*/ 
  
  override def nodes: List[Node] = 
    nodeMap.values.toList.flatMap(_.toList)
    
  override protected[combinators] def toLinear = 
    LinearStore(attrs, nodes, ns)
  override protected def mkState(attrs: MetaData, nodes: List[Node], ns: NamespaceBinding) =
    new RandomAccessStore(attrs, nodes, ns)
    
  override def toString = "RandomAccessStore(" + attrs + ", " + 
    nodes.mkString("", ",", "") + ", " + ns + ")"
}


/**
 * Convenience object for creating LinearStores
 *
 * @author Iulian Dragos
 */
object LinearStore {
  /** Return an empty pickler state. */
  def empty: LinearStore = empty(TopScope)
  
  /** Return an empty pickler state with a given namespace scope. */
  def empty(ns: NamespaceBinding) = LinearStore(Null, Nil, ns)

  /** Create a LinearStore with the given state.*/
  def apply(attrs: MetaData, nodes: List[Node], ns: NamespaceBinding) = 
    new LinearStore(attrs, nodes, ns)
    
  /** Create a LinearStore from an element. */
  def fromElem(e: Elem) = 
    LinearStore(e.attributes, Utility.trimProper(e).toList, TopScope)
    
  /** Create a LinearStore for the contents of the given element. */ 
  def enterElem(e: Elem) = 
    LinearStore(e.attributes, e.child.toList, e.scope)
}