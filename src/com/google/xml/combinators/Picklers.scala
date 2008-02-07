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

/** A class for XML Pickling combinators. Influenced by the <a href="http://www.fh-wedel.de/~si/HXmlToolbox/">
 *  Haskell XML Toolbox</a>, Andrew Kennedy's Pickling Combinators, and Scala combinator parsers
 *  <p>
 *  A pickler for some type A is a class that can save objects of type A to XML (pickle)
 *  and read XML back to objects of type A (unpickle). This class provides some basic 
 *  building blocks (like text), and several combinators (like elem, attr, 
 *  seq) to build more complex picklers.
 *  <p>
 *  Example:
 *  <xmp>
 *    def picklePair: Pickler[String ~ String] = 
 *       elem("p", URI, "pair", 
 *          elem("p", URI, "a", text) ~ elem("p", URI, "b", text))
 *        
 *    val input =
 *      <p:pair xmlns:p="testing-uri">
 *        <p:a>alfa</p:a>
 *        <p:b>omega</p:b>
 *      </p:pair>
 *  </xmp>
 *  picklePair will be able to pickle and unpickle pairs of Strings that look like the
 *  input.
 *
 *  @author Iulian Dragos (iuliandragos@google.com) 
 */
abstract class Picklers {

  /** The state of the pickler is a collection of attributes, a list of 
   *  nodes (which might be Text nodes), and namespace bindings.
   */
  type St = PicklerState

  val emptySt: St = PicklerState.empty

  /** A class representing pickling results. It encapsulate the result and the state of 
   *  the pickler. It can be either @see Success or @see Failure.
   */
  sealed abstract class PicklerResult[+A] {
    /** Apply 'f' when this result is successful. */
    def andThen[B](f: (A, St) => PicklerResult[B]): PicklerResult[B]
    
    /** Apply 'f' when this result is failure. */
    def orElse[B >: A](f: => PicklerResult[B]): PicklerResult[B]
  }
  
  case class Success[+A](v: A, in: St) extends PicklerResult[A] {
    def andThen[B](f: (A, St) => PicklerResult[B]): PicklerResult[B] = f(v, in)
    def orElse[B >: A](f: => PicklerResult[B]): PicklerResult[B] = this
  }
  
  case class Failure(msg: String) extends PicklerResult[Nothing] {
    def andThen[B](f: (Nothing, St) => PicklerResult[B]) = this
    def orElse[B >: Nothing](f: => PicklerResult[B]): PicklerResult[B] = f
  }

  /** Pickler for type A */
  abstract class Pickler[A] extends ((A, St) => St) with (St => PicklerResult[A]) {
    def apply(v: A, in: St) = pickle(v, in)
    def apply(in: St): PicklerResult[A] = unpickle(in)
    
    def pickle(v: A, in: St): St
    def unpickle(in: St): PicklerResult[A]
    
    /** Sequential composition. This pickler will accept an A and then a B. */
    def ~[B](pb: => Pickler[B]): Pickler[~[A, B]] = 
      seq(this, pb)
      
    /** Permutation operator (left associative). Returns a pickler that parses A and B in
     *  any order.
     */
    def *[B](pb: => Pickler[B]): Pickler[A ~ B] = permute(this, pb)
  }

  /** A basic pickler that serializes a value to a string and back. 
   *  It can be later wrapped into an element or attribute.
   */
  def text: Pickler[String] = new Pickler[String] {
    def pickle(v: String, in: St): St = 
      in.addText(v)

    def unpickle(in: St): PicklerResult[String] = {
      in.nodes.head match {
        case Text(content) => Success(content, in.dropNodes(1))
        case n             => Failure("Text node expected, but " + n + " found")
      }
    }
  }
  
  /** A constant pickler: it always pickles 'v', and always unpickles 'v'. */
  def const[A](v: A, pickler: (A, St) => St): Pickler[A] = new Pickler[A] {
    def pickle(ignored: A, in: St) = pickler(v, in)
    def unpickle(in: St): PicklerResult[A] = Success(v, in)
  }

  /** Wrap a parser into an attribute. The attribute will contain all the 
   *  content produced by 'pa' in the 'nodes' field.
   */
  def attr[A](pre: String, uri: String, key: String, pa: => Pickler[A]) = new Pickler[A] {
    def pickle(v: A, in: St) = {
      in.addNamespace(pre, uri).addAttribute(pre, key, pa.pickle(v, emptySt).nodes.text)
    }

    def unpickle(in: St): PicklerResult[A] = {
      in.attrs(uri, in.ns, key) match {
        case null  => Failure("Expected attribute " + pre + ":" + key + " in " + uri + " but none found in " + in.attrs)
        case nodes => pa.unpickle(PicklerState(null, nodes.toList, in.ns))
      }
    }
  }

  /** Wrap a pickler into an element. */
  def elem[A](pre: String, uri: String, label: String, pa: => Pickler[A]) = new Pickler[A] {
    def pickle(v: A, in: St): St = {
      val ns1 = if (in.ns.getURI(pre) == uri) in.ns else new NamespaceBinding(pre, uri, in.ns)
      val in1 = pa.pickle(v, PicklerState.empty(ns1))
      in.addNode(Elem(pre, label, in1.attrs, in1.ns, in1.nodes.reverse:_*))
    }

    def unpickle(in: St): PicklerResult[A] = {
      in.nodes match {
        case (e: Elem) :: _ if (e.label == label) && (e.namespace == uri) => 
          pa.unpickle(toPicklerState(e)) match {
            case Success(v, in1) => Success(v, in.dropNodes(1))
            case f @ Failure(_)  => f
          }
          
        case _ => 
          Failure("Expected a <" + pre + ":" + label + "> in " + uri + " in " + in.nodes)
      }
    }

    private def toPicklerState(e: Elem): PicklerState = 
      PicklerState(e.attributes, e.child.toList, e.scope)
  }

  /** Sequential composition of two picklers */
  def seq[A, B](pa: => Pickler[A], pb: => Pickler[B]): Pickler[~[A, B]] =  new Pickler[~[A, B]] {
    def pickle(v: ~[A, B], in: St): St = 
      pb.pickle(v._2, pa.pickle(v._1, in))
    
    def unpickle(in: St): PicklerResult[~[A, B]] = {
      pa.unpickle(in) match {
        case Success(va, in1) =>
          pb.unpickle(in1) match {
            case Success(vb, in2) => Success(new ~(va, vb), in2)
            case f @ Failure(_)   => f
          }
        case f @ Failure(_) => f
      }
    }
  }

  /** Accept values of A and B in any order.
   *  TODO (iuliandragos): Fix the permutations like 'a c b' for 'a * b * c'
   */
  def permute[A, B](pa: => Pickler[A], pb: => Pickler[B]): Pickler[~[A, B]] = new Pickler[A ~ B] {
    def pickle(v: A ~ B, in: St): St = 
      pb.pickle(v._2, pa.pickle(v._1, in))
      
    def unpickle(in: St): PicklerResult[A ~ B] = {
      pa.unpickle(in) match { 
        case Success(va, in1) => 
          pb.unpickle(in1) andThen { (vb, in2) => Success(new ~(va, vb), in2) }
          
        case Failure(_) =>
          pb.unpickle(in) andThen { (vb, in1) =>
              pa.unpickle(in1) andThen { (va, in2) => Success(new ~(va, vb), in2) }
          }
      }
    }
  }
  
  /** Return a pickler that always pickles the first value, but unpickles using the second when the
   *  first one fails.
   */
  def or[A](pa: => Pickler[A], paa: Pickler[A]): Pickler[A] = new Pickler[A] {
    def pickle(v: A, in: St): St = 
      pa.pickle(v, in)
      
    def unpickle(in: St): PicklerResult[A] = 
      pa.unpickle(in) orElse paa.unpickle(in)
  }
  
  def rep[A](pa: => Pickler[A]): Pickler[List[A]] = new Pickler[List[A]] {
    def pickle(vs: List[A], in: St): St = vs match {
      case v :: vs => pickle(vs, pa.pickle(v, in))
      case Nil     => in
    }
    
    def unpickle(in: St): PicklerResult[List[A]] = 
      pa.unpickle(in).andThen { (v: A, in1: St) => 
         val Success(vs, in2) = unpickle(in1)
         Success(v :: vs, in2)
      } orElse Success(Nil, in)
  }

  /** Wrap a pair of functions around a given pickler */
  def wrap[A, B](f: A => B)(g: B => A)(pb: => Pickler[B]): Pickler[A] = new Pickler[A] {
    def pickle(v: A, in: St): St = 
      pb.pickle(f(v), in)

    def unpickle(in: St): PicklerResult[A] = 
      pb.unpickle(in) match {
        case Success(vb, in1) => Success(g(vb), in1)
        case f @ Failure(_)   => f
      }
  }

  /** A logging combinator */
  def log[A](pa: => Pickler[A])(name: String): Pickler[A] = new Pickler[A] {
    def pickle(v: A, in: St): St = {
      println("pickling " + name + " at: " + in)
      val res = pa.pickle(v, in)
      println("got back: " + res)
      res
    }

    def unpickle(in: St) = {
      println("unpickling " + name + " at: " + in)
      val res = pa.unpickle(in)
      println("got back: " + res)
      res
    }
  }
}

/** Convenience class to hold two values (it has lighter syntax than pairs). */
case class ~[+A, +B](_1: A, _2: B) {
  override def toString = "~(" + _1 + ", " + _2 + ")"
}

object Picklers {
  
  /** Convert a binary function to a function of a pair. */
  implicit def fun2ToPair[A, B, C](f: (A, B) => C): (~[A, B]) => C = { 
    case a ~ b => f(a, b)
  }
  
  /** Convert a function of 3 arguments to one that takes a pair of a pair. */
  implicit def fun3ToPpairL[A, B, C, D](f: (A, B, C) => D): (~[~[A, B], C]) => D = { 
    case a ~ b ~ c =>  f(a, b, c)
  }
  
  /** Convert a function of 3 arguments to one that takes a pair of a pair, 
   *  right associative. */
  implicit def fun3ToPpairR[A, B, C, D](f: (A, B, C) => D): (~[A, ~[B, C]]) => D = { 
    case a ~ (b ~ c) =>  f(a, b, c)
  }
  
}
