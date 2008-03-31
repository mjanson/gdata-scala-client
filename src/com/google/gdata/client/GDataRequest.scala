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


package com.google.gdata.client

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
class GDataRequest(method: RequestMethod.Value, url: URL) {
  /** The underlying HTTP connection. */
  private val connection: HttpConnection = 
    new HttpConnection(url.openConnection.asInstanceOf[HttpURLConnection])
  
  /** Is this connection connected? */
  private var connected: Boolean = false
  
  private var response: connection.HttpResponse = _
  
  /** Create a new request using a string URL. */
  def this(method: RequestMethod.Value, url: String) = {
    this(method, new URL(url))
  }
  
  /** Add a request property. */
  def +=(field: String, value: String) = {
    connection(field) = value
    GDataRequest.logger.fine("Added request property: " + field + ": " + value)
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
    connection.requestMethod = method.toString
    
    method match {
      case GET => 
        connection.doInput = true; connection.doOutput = false
      case PUT => 
        connection.doInput =true; connection.doOutput = true
      case POST => 
        if (!slug.isEmpty)
          connection("Slug") = URLEncoder.encode(slug, "UTF-8")
        connection.doInput = true; connection.doOutput = true
      case DELETE => 
        connection.doInput = false; connection.doOutput = false
    }
    response = connection.connect
    connected = true
    
    GDataRequest.logger.fine("Got back: " + response.responseCode + " " + response.responseMessage)
    
    if (response.responseCode >= 300)
      handleErrorCode(response.responseCode)
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
  def outputStream: OutputStream = connection.outputStream
  
  /** Get the content of this request (what the server returned). */
  def content: InputStream = {
    if (!connected)
      connect
    response.inputStream
  } 
  
  /** Unpickle the result of this request. */
  def unpickle[A](pa: Pickler[A]): PicklerResult[A] = {
    connect
    try {
      pa.unpickle(LinearStore.fromInputStream(content))
    } finally {
      content.close
    }
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
  
  /**
   * A factory for GDataRequests.
   */
  class Factory extends RequestFactory {
    /** Additional request parameters. */
    private var params: List[(String, String)] = Nil
    
    /** An optional authentication token. */
    private var authToken: Option[AuthToken] = None
    
    /** 
     * Create a new GData request. Additional request parameters, and authentication token are
     * set before returning the request.
     */
    def mkRequest(method: RequestMethod.Value, url: URL): GDataRequest = {
      val request = new GDataRequest(method, url)
      for ((n, v) <- params)
        request += (n, v)
      if (authToken.isDefined)
        request += ("Authorization", token.get.getAuthHeader)
      request
    }
    
    /** Get the current authentication token. */
    def token = authToken
      
    /** Set the current authentication token. */
    def token_=(tok: AuthToken) = {
      authToken = Some(tok)
    }
    
    /** Add a request parameter that will be added to all created requests. */
    def +=(name: String, value: String): this.type = {
      params = (name, value) :: params
      this
    }
  }
}
