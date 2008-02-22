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

/**
 * This object holds together common namespace definitions.
 */
object Uris {
  /** Atom namespace. Usually aliased to 'atom'. */
  final val ATOM = "http://www.w3.org/2005/Atom"
  
  /** GData namespace. Usually aliased to 'gd'. */
  final val GDATA = "http://schemas.google.com/g/2005"
  
  /** XHTML namespace. Usually aliased to 'xhtml'. */
  final val XHTML = "http://www.w3.org/1999/xhtml"
  
  /** XML namespace. <b>Always</b> aliased to 'xml'. */
  final val XML = "http://www.w3.org/XML/1998/namespace"
  
  /** The Media RSS namespace. */
  final val MEDIA = "http://search.yahoo.com/mrss/"
}
