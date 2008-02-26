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


package com.google.gdata.data;

import com.google.xml.combinators.{Picklers, ~}
import Picklers._
import scala.xml.{NamespaceBinding, TopScope}

/**
 * An Atom category, as defined by the Atom spec.
 * 
 * @see http://atomenabled.org/developers/syndication/atom-format-spec.php
 */
case class Category(term: String, scheme: Option[String], label: Option[String])

trait Isomorphic[A, B] {
  def toB(v: A): B
  def fromB(v: B): A
}

/** 
 * TODO: remove the isomorphism class, use plain wrappers.
 */
object Category {
  implicit val atomNs = new NamespaceBinding("atom", Uris.ATOM, TopScope)

  def wrap[A, B](p: => Pickler[A], iso: Isomorphic[A, B]): Pickler[B] =
    Picklers.wrap (p) (iso.toB) (iso.fromB)
  
  lazy val pickler: Pickler[Category] =
    wrap(elem("category",
        attr("term", text) ~ opt(attr("scheme", text)) ~ opt(attr("label", text))),
        isoCategory)
}

/** Define the isomorphism between categories and the parseable strings. */
object isoCategory extends Isomorphic[String ~ Option[String] ~ Option[String], Category] {
  def toB(v: String ~ Option[String] ~ Option[String]) = v match {
    case term ~ scheme ~ label => Category(term, scheme, label) 
  }
  def fromB(v: Category) = new ~(v.term, v.scheme) ~ v.label
}
