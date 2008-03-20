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


package com.google.gdata

import com.google.xml.combinators.Picklers.{Pickler, Success, NoSuccess}
import com.google.gdata.data.kinds.FeedLink

import java.net.URL

abstract class Service(appName: String) {
  private val userAgent = appName + "-" + Service.SERVICE_NAME + "-" + Service.SERVICE_VERSION

  /**
   * Make the given query and parse the XML result using the given pickler.
   * 
   * @throws UnknownDocumentException of the pickler is unsuccessful.
   */
  def query[A](base: URL, q: Query, p: Pickler[A]): A = {
    query(q.mkUrl(base.toString), p)
  }
  
  /**
   * Make the given query and parse the XML result using the given pickler.
   * 
   * @throws UnknownDocumentException of the pickler is unsuccessful.
   */
  def query[A](url: String, p: Pickler[A]): A = {
    val request = mkRequest(RequestMethod.GET, url)
    request.unpickle(p) match {
      case Success(res, _) => 
        res
      case e: NoSuccess => 
        throw new UnknownDocumentException("The XML response could not be parsed.", e)
    }
  }
  
  /**
   * Put 'a' at the given URL, serialized using the given pickler. Returns the
   * object as given back by the server.
   */
/*  def put[A](url: URL, a: A, pa: Pickler[A]): A = {

  }*/
  
  /** 
   * Return the feed embedded in the given feed link, or make a query to retrieve
   * it from the given URL. If the url is not given, it assumes the feed is embedded.
   */
  def fromFeedLink[A](f: FeedLink[A], pa: Pickler[A]): A = {
    f.href match {
      case Some(href) => query(href, pa)
      case None => f.feed.get
    }
  }
  
  protected def mkRequest(method: RequestMethod.Value, url: String) = {
    val request = new GDataRequest(RequestMethod.GET, url)
    request += ("User-Agent", userAgent)
    
    request
  }
}

object Service {
  final val SERVICE_NAME = "GData/Scala"
  final val SERVICE_VERSION = "0.1"
}