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

import com.google.xml.combinators.Picklers.{Pickler, PicklerResult}
import com.google.xml.combinators.LinearStore

import java.net.{URL, HttpURLConnection, URLEncoder}
import java.io.{InputStream, OutputStream}
import java.util.logging.Logger

import scala.collection._

/**
 * This class encapsulates an HTTP request to a GData service. It sets up
 * the HTTP connection and handles authentication. This class throws 
 * IllegalStateException in exactly the same cases as HttpURLConnection. 
 * 
 * @see java.net.HttpURLConnection for details regarding state errors.
 * @author Iulian Dragos
 */
class GDataRequest(method: RequestMethod.Value, url: String) {
  /** The underlying HTTP connection. */
  private var connection: HttpURLConnection = 
    new URL(url).openConnection.asInstanceOf[HttpURLConnection]
  
  /** Is this connection connected? */
  private var connected: Boolean = false
  
  /** Add a request property. */
  def +=(field: String, value: String) = {
    connection.addRequestProperty(field, value)
    this
  }
  
  /** The slug header  */
  var slug: String = ""
  
  /** 
   * Connect to the given URL. If already connected, this call is ignored.
   * The server response code is turned into an exception if it is an error response.
   */
  def connect {
    import RequestMethod._
    
    if (connected) return
    
    GDataRequest.logger.fine("Connecting to " + url)
    connection.setRequestMethod(method.toString)
    
    method match {
      case GET => 
        connection.setDoInput(true); connection.setDoOutput(false)
      case PUT => 
        connection.setDoInput(true); connection.setDoOutput(true)
      case POST => 
        if (!slug.isEmpty)
          connection.addRequestProperty("Slug", URLEncoder.encode(slug, "UTF-8"))
        connection.setDoInput(true); connection.setDoOutput(true)
      case DELETE => 
        connection.setDoInput(false); connection.setDoOutput(false)
    }
    connection.connect()
    connected = true
    
    GDataRequest.logger.fine("Got back: " + responseCode + " " + responseMessage)
    
    if (responseCode > 300)
      handleErrorCode(responseCode)
  }
  
  /** 
   * Turn some error codes into exceptions.
   */
  private def handleErrorCode(code: Int) = {
    import HttpURLConnection._
    
    code match {
      case HTTP_NOT_MODIFIED => throw NotModifiedException()
      case HTTP_BAD_REQUEST => throw BadRequestException()
      case HTTP_UNAUTHORIZED => throw UnauthorizedException()
      case HTTP_FORBIDDEN => throw ForbiddenException()
      case HTTP_NOT_FOUND => throw NotFoundException()
      case HTTP_CONFLICT => throw ConflictException()
      case HTTP_INTERNAL_ERROR => throw InternalServerErrorException()
      case _ => ()
    }
  } 
  
  /** Return an output stream for writing in this connection. */
  def outputStream: OutputStream = connection.getOutputStream
  
  /** Get the content of this request (what the server returned). */
  def content: InputStream = connection.getContent.asInstanceOf[InputStream]
  
  /** Unpickle the result of this request. */
  def unpickle[A](pa: Pickler[A]): PicklerResult[A] = {
    connect
    try {
      val source = io.Source.fromInputStream(content)
      // TODO: fix the 'reset' method in the scala library, before we can use logging
      //GDataRequest.logger.fine("Got back content: " + source.getLines.mkString("", "\n", ""))
      //source.reset
      pa.unpickle(LinearStore.fromSource(source))
    } finally {
      content.close
    }
  }
  
  /** Return a map of all headers in the server's respones. */
  lazy val headers: Map[String, List[String]] = {
    def toScalaList[A](l: java.util.List[A]): List[A] = {
      val ab = new mutable.ListBuffer[A]
      val iter = l.iterator
      while (iter.hasNext) ab += iter.next
      ab.toList
    }
    (for ((keys, headers) <- new mutable.JavaMapAdaptor(connection.getHeaderFields))
      yield (keys, toScalaList(headers))).asInstanceOf[Map[String, List[String]]]
  }
  
  /** Get the response code of this request. */
  def responseCode: Int = 
    connection.getResponseCode
  
  /** Get the response message, if it was provided. */
  def responseMessage: Option[String] = {
    val tmp = connection.getResponseMessage
    if (tmp eq null) None else Some(tmp)
  }
}

/** The HTTP method used by the GData Request */
object RequestMethod extends Enumeration {
  val GET = Value("GET")
  val PUT = Value("PUT")
  val POST = Value("POST")
  val DELETE = Value("DELETE")
}

object GDataRequest {
  private val logger = Logger.getLogger("com.google.gdata.GDataRequest") 
}