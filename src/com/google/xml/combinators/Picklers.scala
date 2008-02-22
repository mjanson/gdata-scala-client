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
import com.google.gdata.data.util.DateTime

import scala.xml.{Node, Elem, NamespaceBinding, NodeSeq, Null, Text}
import java.text.ParseException

/** 
 * A class for XML Pickling combinators. Influenced by the <a href="http://www.fh-wedel.de/~si/HXmlToolbox/">
 * Haskell XML Toolbox</a>, Andrew Kennedy's Pickling Combinators, and Scala combinator parsers
 * <p>
 * A pickler for some type A is a class that can save objects of type A to XML (pickle)
 * and read XML back to objects of type A (unpickle). This class provides some basic 
 * building blocks (like text), and several combinators (like elem, attr, 
 * seq) to build more complex picklers.
 * <p>
 * Example:
 * <xmp>
 *   def picklePair: Pickler[String ~ String] = 
 *      elem("p", URI, "pair", 
 *         elem("p", URI, "a", text) ~ elem("p", URI, "b", text))
 *       
 *   val input =
 *     <p:pair xmlns:p="testing-uri">
 *       <p:a>alfa</p:a>
 *       <p:b>omega</p:b>
 *     </p:pair>
 * </xmp>
 * picklePair will be able to pickle and unpickle pairs of Strings that look like the
 * input.
 *
 * @author Iulian Dragos (iuliandragos@google.com) 
 */
object Picklers extends AnyRef with ImplicitConversions {

  /**
   * The state of the pickler is a collection of attributes, a list of 
   * nodes (which might be Text nodes), and namespace bindings.
   */
  type St = XmlStore

  val emptySt: St = LinearStore.empty

  /**
   * A class representing pickling results. It encapsulate the result and the state of 
   * the pickler. It can be either @see Success or @see Failure.
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

  abstract class NoSuccess(msg: String, in: St) extends PicklerResult[Nothing] {
    def andThen[B](f: (Nothing, St) => PicklerResult[B]) = this
    def orElse[B >: Nothing](f: => PicklerResult[B]): PicklerResult[B] = f

    val prefix: String

    override def toString = prefix + msg + " with input: " + in
  }
  
  /** A Failure means the parsing has failed, but alternatives can still be tried. */
  case class Failure(m: String, i: St) extends NoSuccess(m, i) {
    override val prefix = "Failure: " 
  }

  /** An Error is a failure which causes the entire parsing to fail (no alternatives are tried). */
  case class Error(m: String, i: St) extends NoSuccess(m, i) {
    override val prefix = "Error: "
  }
  
  /** Pickler for type A */
  abstract class Pickler[+A] {
    type In = T forSome {type T <: A}
    
    def pickle(v: In, in: St): St
    def unpickle(in: St): PicklerResult[A]
    
    /** Sequential composition. This pickler will accept an A and then a B. */
    def ~[B](pb: => Pickler[B]): Pickler[~[A, B]] = 
      seq(this, pb)
  }

  /**
   * A basic pickler that serializes a value to a string and back. 
   * It can be later wrapped into an element or attribute.
   */
  def text: Pickler[String] = new Pickler[String] {
    def pickle(v: In, in: St): St = 
      in.addText(v)

    def unpickle(in: St): PicklerResult[String] = {
      in.acceptText match {
        case (Some(Text(content)), in1) => Success(content, in1)
        case (None, in1)                => Failure("Text node expected", in1)
      }
    }
  }
  
  def dateTime: Pickler[DateTime] = new Pickler[DateTime] {
    def pickle(v: In, in: St): St = 
      in.addText(v.toString)
      
    def unpickle(in:St): PicklerResult[DateTime] = 
      in.acceptText match {
        case (Some(Text(str)), in1) =>
          try {
            Success(DateTime.parse(str), in1)
          } catch {
            case e: ParseException => Failure("Invalid date: " + e.getMessage, in1)
          }
        case (None, in1) => 
          Failure("Expected date in textual format", in1) 
      }
  }

  
  /** A constant pickler: it always pickles 'v', and always unpickles 'v'. */
  def const[A](v: A, pickler: (A, St) => St): Pickler[A] = new Pickler[A] {
    def pickle(ignored: In, in: St) = pickler(v, in)
    def unpickle(in: St): PicklerResult[A] = Success(v, in)
  }
  
  /** A pickler for default values. If 'pa' fails, returns 'v' instead. */
  def default[A](pa: => Pickler[A], v: A): Pickler[A] =  
    wrap (opt(pa)) ({ 
      case Some(v1) => v1
      case None => v
  }) (v => Some(v))

  /** Convenience method for creating an attribute within a namepace. */
  def attr[A](label: String, pa: => Pickler[A], ns: NamespaceBinding): Pickler[A] =
    attr(ns.prefix, ns.uri, label, pa)

  /**
   * Wrap a parser into an attribute. The attribute will contain all the 
   * content produced by 'pa' in the 'nodes' field.
   */
  def attr[A](pre: String, uri: String, key: String, pa: => Pickler[A]) = new Pickler[A] {
    def pickle(v: In, in: St) = {
      in.addNamespace(pre, uri).addAttribute(pre, key, pa.pickle(v, emptySt).nodes.text)
    }

    def unpickle(in: St): PicklerResult[A] = {
      in.acceptAttr(key, uri) match {
        case (Some(nodes), in1) =>
          pa.unpickle(LinearStore(Null, nodes.toList, in.ns)) andThen { (v, in2) => Success(v, in1) }
        case (None, in1) => 
          Failure("Expected attribute " + pre + ":" + key + " in " + uri, in)
      }
    }
  }
  
  def attr[A](label: String, pa: => Pickler[A]): Pickler[A] = new Pickler[A] {
    def pickle(v: In, in: St) = 
      in.addAttribute(label, pa.pickle(v, emptySt).nodes.text)
      
    def unpickle(in: St): PicklerResult[A] = {
      in.acceptAttr(label) match {
        case (Some(nodes), in1) =>
          pa.unpickle(LinearStore(Null, nodes.toList, in.ns)) andThen { (v, in2) => Success(v, in1) }
        case (None, in1) => 
          Failure("Expected unprefixed attribute " + label, in)
      }
    }
  }
  
  /** 
   * Convenience method for creating an element with an implicit namepace. Contents of
   * this element are committed (this parser is not allowed to recover from failures in
   * parsing its content.
   */
  def elem[A](label: String, pa: => Pickler[A])(implicit ns: NamespaceBinding): Pickler[A] =
    elem(ns.prefix, ns.uri, label, commit(pa))
  
  /** Wrap a pickler into an element. */
  def elem[A](pre: String, uri: String, label: String, pa: => Pickler[A]) = new Pickler[A] {
    def pickle(v: In, in: St): St = {
      val ns1 = if (in.ns.getURI(pre) == uri) in.ns else new NamespaceBinding(pre, uri, in.ns)
      val in1 = pa.pickle(v, LinearStore.empty(ns1))
      in.addNode(Elem(pre, label, in1.attrs, in1.ns, in1.nodes.reverse:_*))
    }

    def unpickle(in: St): PicklerResult[A] = {
      in.acceptElem(label, uri) match {
        case (Some(e: Elem), in1) => 
          pa.unpickle(LinearStore.enterElem(e)) andThen { (v, in2) =>
            Success(v, in1)
          }
          
        case _ => 
          Failure("Expected a <" + pre + ":" + label + "> in " + uri, in)
      }
    }
  }

  /** Sequential composition of two picklers */
  def seq[A, B](pa: => Pickler[A], pb: => Pickler[B]): Pickler[~[A, B]] =  new Pickler[~[A, B]] {
    def pickle(v: In, in: St): St = 
      pb.pickle(v._2, pa.pickle(v._1, in))
    
    def unpickle(in: St): PicklerResult[~[A, B]] = {
      pa.unpickle(in) match {
        case Success(va, in1) =>
          pb.unpickle(in1) match {
            case Success(vb, in2) => Success(new ~(va, vb), in2)
            case f: NoSuccess     => f
          }
        case f: NoSuccess => f
      }
    }
  }

  /** 
   * Convenience method for creating an element with interleaved elements. Elements enclosed
   * by the given element label can be parsed in any order. Any unknown elements are ignored.
   * <p/>
   * Example: 
   *   <code>interleaved("entry", elem("link", text) ~ elem("author", text))</code> will
   * will parse an element entry with two subelements, link and author, in any order, with
   * possibly other elements between them.
   */
  def interleaved[A](label: String, pa: => Pickler[A])(implicit ns: NamespaceBinding): Pickler[A] =
    elem(label, interleaved(pa))(ns)

  /**
   * Transform the given parser into a parser that accepts permutations of its containing 
   * sequences. That is, interleaved(a ~ b ~ c) will parse a, b, c in any order (with possibly 
   * other elements in between. It should not be called directly, instead use the
   * interleaved which wraps an element around the interleaved elements.  
   */
  def interleaved[A](pa: => Pickler[A]): Pickler[A] = new Pickler[A] {
    def pickle(v: In, in: St): St = pa.pickle(v, in)
    
    def unpickle(in: St): PicklerResult[A] = in match {
      case _: RandomAccessStore => pa.unpickle(in)
      case _ => pa.unpickle(new RandomAccessStore(in)) andThen { (v, in1) => 
        Success(v, in1.toLinear) 
      }
    }
  }
   
  /** 
   * A commit parser. Failures are transformed to errors, so alternatives (when combined with 
   * other parsers) are not tried. 
   */
  def commit[A](pa: => Pickler[A]): Pickler[A] = new Pickler[A] {
    def pickle(v: In, in: St): St = pa.pickle(v, in)
    def unpickle(in: St): PicklerResult[A] = pa.unpickle(in) match {
      case s: Success[_] => s
      case Failure(msg, in1) => Error(msg, in1)
      case e: Error => e
    }
  }

  /**
   * Return a pickler that always pickles the first value, but unpickles using the second when the
   * first one fails.
   */
  def or[A](pa: => Pickler[A], paa: Pickler[A]): Pickler[A] = new Pickler[A] {
    def pickle(v: In, in: St): St = 
      pa.pickle(v, in)
      
    def unpickle(in: St): PicklerResult[A] = 
      pa.unpickle(in) match {
        case s: Success[_] => s
        case f: Failure => paa.unpickle(in)
        case e: Error => e
      }
  }
  
  /**
   * An optional pickler. It pickles v when it is there, and leaves the input unchanged when empty.
   * It unpickles the value when the underlying parser succeeds, and returns None otherwise.
   */
  def opt[A](pa: => Pickler[A]) = new Pickler[Option[A]] {
    def pickle(v: In, in: St) = v match {
      case Some(v) => pa.pickle(v, in)
      case None    => in
    }
    
    def unpickle(in: St): PicklerResult[Option[A]] = 
      pa.unpickle(in) andThen {(v, in1) => Success(Some(v), in1) } orElse Success(None, in)
  }
  
  def rep[A](pa: => Pickler[A]): Pickler[List[A]] = new Pickler[List[A]] {
    def pickle(vs: In, in: St): St = vs match {
      case v :: vs => pickle(vs, pa.pickle(v, in))
      case Nil     => in
    }
    
    def unpickle(in: St): PicklerResult[List[A]] = { 
      val res1 = pa.unpickle(in).andThen { (v: A, in1: St) => 
         val Success(vs, in2) = unpickle(in1)
         Success(v :: vs, in2)
      } 
      res1 match {
        case s: Success[_] => s
        case f: Failure => Success(Nil, in)
        case e: Error => e
      }
    }
  }

  /** Wrap a pair of functions around a given pickler */
  def wrap[A, B](pb: => Pickler[B])(g: B => A)(f: A => B): Pickler[A] = new Pickler[A] {
    def pickle(v: In, in: St): St = 
      pb.pickle(f(v), in)

    def unpickle(in: St): PicklerResult[A] = 
      pb.unpickle(in) match {
        case Success(vb, in1) => Success(g(vb), in1)
        case f: NoSuccess     => f
      }
  }

/* // Waiting for bug fix in 2.7.0
  def wrapCaseClass[A, B](pa: => Pickler[A])(f: A => B)(g: B => Some[A]): Pickler[B] =
    wrap(pa) (f) { x => g(x).get }
*/
  
  /** Collect all elements of the input into a NodeSeq. */
  def collect: Pickler[NodeSeq] = new Pickler[NodeSeq] {
    def pickle(v: In, in: St) = 
      v.foldLeft(in) (_.addNode(_))
    def unpickle(in: St) =
      Success(NodeSeq.fromSeq(in.nodes), LinearStore(in.attrs, Nil, in.ns))
  }
  
  def extend[A <: Extensible, B](pa: => Pickler[A], pb: => Pickler[B]) = new Pickler[A ~ B] {
    def pickle(v: In, in: St): St = {
      val in1 = pb.pickle(v._2, LinearStore.empty)
      v._1.extension = NodeSeq.fromSeq(in1.nodes.reverse)
      pa.pickle(v._1, in)
    }
 
    def unpickle(in: St): PicklerResult[A ~ B] = {
      pa.unpickle(in) andThen { (a, in1) => 
        pb.unpickle(LinearStore(Null, a.extension.toList, in1.ns)) andThen { (b, in2) =>
          Success(new ~(a, b), in1)
        }
      }
    }
  }
  
  /** Make a given element handle raw XML elements.  */
  def extensible[A <: Extensible](pa: => Pickler[A]): Pickler[A] = 
    wrap (pa ~ collect) { case a ~ ext => a.extension = ext; a } { a => new ~ (a, a.extension) }
  
  /** A logging combinator */
  def log[A](name: String, pa: => Pickler[A]): Pickler[A] = new Pickler[A] {
    def pickle(v: In, in: St): St = {
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

  def withNamespace[A](pre: String, uri: String, parent: NamespaceBinding)
    (body: NamespaceBinding => Pickler[A]): Pickler[A] =
      if (parent.getURI(pre) == uri) 
        body(parent) 
      else 
        body(new NamespaceBinding(pre, uri, parent))
}

/** Convenience class to hold two values (it has lighter syntax than pairs). */
final case class ~[+A, +B](_1: A, _2: B) {
  override def toString = "~(" + _1 + ", " + _2 + ")"
  
  /** Append another value to this pair. */
  def ~[C](c: C) = new ~(this, c)
}

trait ImplicitConversions {
  /** Convert a binary function to a function of a pair. */
    implicit def fun2ToPair[A, B, C](fun: (A, B) => C): (~[A, B]) => C = { 
      case a ~ b => fun(a, b)
    }
    
    /** Convert a function of 3 arguments to one that takes a pair of a pair. */
    implicit def fun3ToPpairL[A, B, C, D]
        (fun: (A, B, C) => D): (~[~[A, B], C]) => D = { 
      case a ~ b ~ c =>  fun(a, b, c)
    }
    
    /** Convert a function of 4 arguments to one that takes a pair of a pair. */
    implicit def fun4ToPpairL[A, B, C, D, E]
        (fun: (A, B, C, D) => E): (~[~[~[A, B], C], D]) => E = { 
      case a ~ b ~ c ~ d =>  fun(a, b, c, d)
    }

    /** Convert a function of 4 arguments to one that takes a pair of a pair. */
    implicit def fun5ToPpairL[A, B, C, D, E, F]
        (fun: (A, B, C, D, E) => F): (A ~ B ~ C ~ D ~ E) => F = { 
      case a ~ b ~ c ~ d ~ e =>  fun(a, b, c, d, e)
    }

    /** Convert a function of 4 arguments to one that takes a pair of a pair. */
    implicit def fun6ToPpairL[A, B, C, D, E, F, G]
        (fun: (A, B, C, D, E, F) => G): (A ~ B ~ C ~ D ~ E ~ F) => G = { 
      case a ~ b ~ c ~ d ~ e ~ f =>  fun(a, b, c, d, e, f)
    }

    /** Convert a function of 4 arguments to one that takes a pair of a pair. */
    implicit def fun7ToPpairL[A, B, C, D, E, F, G, H]
        (fun: (A, B, C, D, E, F, G) => H): (A ~ B ~ C ~ D ~ E ~ F ~ G) => H = { 
      case a ~ b ~ c ~ d ~ e ~ f ~ g =>  fun(a, b, c, d, e, f, g)
    }

    /** Convert a function of 3 arguments to one that takes a pair of a pair, 
     *  right associative. */
    implicit def fun3ToPpairR[A, B, C, D](f: (A, B, C) => D): (~[A, ~[B, C]]) => D = { 
      case a ~ (b ~ c) =>  f(a, b, c)
    } 
    
    implicit def funTuple2ToPair[A, B, C](f: A => (B, C)) = { x: A => tuple2Pair(f(x)) }
    implicit def funTuple3ToPair[A, B, C, D](f: A => (B, C, D)) = { x: A => tuple3Pair(f(x)) }
    
    def tuple2Pair[A, B](p: (A, B)) = new ~(p._1, p._2)
    def tuple3Pair[A, B, C](p: (A, B, C)) = new ~(new ~(p._1, p._2), p._3)
    def tuple4Pair[A, B, C, D](p: (A, B, C, D)) = new ~(new ~(new ~(p._1, p._2), p._3), p._4)
}

/** A trait for extensible data. Unknown elements will be collected in 'extension'. */
trait Extensible {
  var extension: NodeSeq = NodeSeq.Empty
}
