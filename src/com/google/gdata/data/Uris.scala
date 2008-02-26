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

import scala.xml.{NamespaceBinding, TopScope}

/**
 * This object holds together common namespace definitions.
 */
object Uris {
  /** Atom namespace. Usually aliased to 'atom'. */
  final val ATOM = "http://www.w3.org/2005/Atom"

  /** An atom namespace binding. */
  lazy val atomNs = new NamespaceBinding("atom", ATOM, TopScope)
  
  /** GData namespace. Usually aliased to 'gd'. */
  final val GDATA = "http://schemas.google.com/g/2005"

  /** A gdata namespace binding. */
  lazy val gdNs = new NamespaceBinding("gd", GDATA, TopScope)
  
  /** XHTML namespace. Usually aliased to 'xhtml'. */
  final val XHTML = "http://www.w3.org/1999/xhtml"
  
  /** An xhtml namespace binding. */
  lazy val xhtmlNs = new NamespaceBinding("xhtml", XHTML, TopScope)

  /** XML namespace. <b>Always</b> aliased to 'xml'. */
  final val XML = "http://www.w3.org/XML/1998/namespace"
  
  /** An xml namespace binding. */
  lazy val xmlNs = new NamespaceBinding("xml", XML, TopScope)

  /** The Media RSS namespace. */
  final val MEDIA = "http://search.yahoo.com/mrss/"
  
  /** A media namespace binding. */
  lazy val mediaNs = new NamespaceBinding("media", MEDIA, TopScope)
}
