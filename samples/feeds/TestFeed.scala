package feeds;

import com.google.xml.combinators._
import com.google.gdata.youtube.VideoEntries
import com.google.gdata.data.{AtomFeeds, AtomEntries, Uris}
import com.google.gdata.data.media.{MediaRss}

import java.net.{URL, MalformedURLException, HttpURLConnection}
import java.io.{InputStream, InputStreamReader}

import scala.xml.{XML, NamespaceBinding, TopScope}

import Picklers._


object atomFeeds extends Object with AtomFeeds with AtomEntries {
  type Entry = AtomEntry
  type Feed = AtomFeed
  def entryPickler = elem("entry", atomEntryPickler)(Uris.atomNs)
  def feedPickler = elem("feed", atomFeedPickler)(Uris.atomNs)
}

object mediaFeeds extends AtomFeeds with MediaRss with VideoEntries {
  type Entry = VideoEntry
  type Feed = AtomFeed
  type Content = YouTubeContent
  type Group = BaseGroup
  
  def groupContentsPickler = baseGroupPickler
  def contentContentsPickler = ytContentContentsPickler
  
  def entryPickler = elem("entry", videoEntryPickler)(Uris.atomNs)
  def feedPickler = elem("feed", atomFeedPickler)(Uris.atomNs)
}

object TestFeed {
  var url = "http://gdata.youtube.com"
  
  var query = ""
  
  var verbose = false
  
  private def log(str: String) = println(str)
    
  def main(args: Array[String]) {
    parseCommandLine(args.toList)
    
    log("Connecting to " + url + "/" + query)
    doQuery
  }
  
  private def doQuery {
    try {
      val url = new URL(this.url + "/" + query)
      val connection = url.openConnection.asInstanceOf[HttpURLConnection]

      connection.setDoOutput(true)
      connection.setRequestMethod("GET")
      connection.connect
      val reader = new InputStreamReader(connection.getContent.asInstanceOf[InputStream])
      //while (reader.ready()) print(reader.read().asInstanceOf[Char])
      log("Parsing XML..")
        val elem = XML.load(reader)
      
      if (verbose) 
        println(new scala.xml.PrettyPrinter(80, 2).format(elem))
      
      log("Unpickling..")
      mediaFeeds.feedPickler.unpickle(LinearStore.fromElem(elem)) match {
        case Success(feed, rest) => 
          println("Success")
          println(feed)
        case f: NoSuccess => println(f)
      }
        
    } catch {
      case e: MalformedURLException =>
        error(e.getMessage)
    }

  }
  
  private def parseCommandLine(as: List[String]) {
    var args = as
    
    while (!args.isEmpty) args match {
      case "-url" :: url :: rest =>
        this.url = url
        args = rest
      case "-q" :: q :: rest =>
        query = q
        args = rest
      case "-v" :: rest =>
        verbose = true
        args = rest
      case "-help" :: rest =>
        printUsage()
        System.exit(0)
        
      case a :: _ =>
        error("Don't know what to do with " + a)
        
      case Nil => ()
    }
  }
  
  private def printUsage() {
    println("""TestFeed [-url <url>] [-q <query>] [-v]

    -url The service url (default: "http://gdata.youtube.com")
    -q   Query string
    -v   Verbose output (prints XML response before unpickling) 
""")
  }
}
