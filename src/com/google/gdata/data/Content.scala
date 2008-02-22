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

import scala.xml.{NamespaceBinding, TopScope, NodeSeq}

import Atom._
import Picklers._

case class MalformedEntry(str: String) extends RuntimeException

abstract class Content(tpe: String)

case class TextContent(var text: String) extends Content("text")
case class HtmlContent(var text: String) extends Content("html")
case class XhtmlContent(var div: NodeSeq) extends Content("xhtml")
case class OutOfLineContent(var src: String, var tpe: String) extends Content(tpe)

object Content {
  implicit val atomNs = new NamespaceBinding("atom", Uris.ATOM, TopScope)
  
  lazy val pickler: Pickler[Content] = {
    val content = elem("content", 
        opt(attr("type", text)) ~ opt(attr("src", text)) ~ collect)
        
    def toContent(parsed: Option[String] ~ Option[String] ~ NodeSeq): Content = 
      parsed match {
        case Some("text") ~ None ~ ctent => TextContent(ctent.toString)
        case Some("html") ~ None ~ ctent => HtmlContent(ctent.toString)
        case Some("xhtml") ~ None ~ ctent => XhtmlContent(ctent)
        case Some(tpe) ~ Some(src) ~ NodeSeq.Empty => OutOfLineContent(src, tpe)
        case _ ~ _ ~ ctent => TextContent(ctent.toString) // this forgives some malformed entries!
//        case _ => throw new MalformedEntry("Invalid atom:content entry: " + parsed)
      }
    
    def fromContent(c: Content) = c match {
      case TextContent(plainText)     => new ~(new ~(Some("text"), None), scala.xml.Text(plainText))
      case HtmlContent(htmlText)      => new ~(new ~(Some("html"), None), scala.xml.Text(htmlText))
      case XhtmlContent(xhtml)        => new ~(new ~(Some("xhtml"), None), xhtml)
      case OutOfLineContent(src, tpe) => new ~(new ~(Some(tpe), Some(src)), NodeSeq.Empty)
    }
    wrap (content) (toContent) (fromContent)
  }
}
