package com.google.gdata

import com.google.xml.combinators.{LinearStore, PlainOutputStore, Picklers}
import com.google.xml.combinators.PicklerAsserts
import com.google.gdata.data.util.{SpecificTime, Now}
import com.google.xmldiff.{Comparison, SimplePath, NoDiff}

import scala.xml.{Utility, XML}

import org.junit._

trait FeedFileTest {
  val comparison = new Comparison

  /** 
   * Test the pickler on the given input file. Pickling back the data should match the input
   * file.
   * 
   * @param resource The path to an XML file on the classpath, on which to run the pickler
   * @param pickler  The pickler to test.
   * @param ignores  Elements to ignore in the comparison.
   */
  def testRoundtrip[A](resource: String, pickler: Picklers.Pickler[A], ignores: String*) {
    val inStream = getClass.getClassLoader.getResourceAsStream(resource)
    if (inStream eq null) Assert.fail("Could not find '" + resource + "'")

    val elem = XML.load(inStream)
    val feed = pickler.unpickle(LinearStore.fromElem(elem))
    comparison.ignorePaths = ignores.toList map (new SimplePath(_))
    Assert.assertTrue("Unpickling failed: " + feed, feed.isSuccessful)
    Assert.assertEquals("Pickling failed", NoDiff, 
        comparison(elem, pickler.pickle(feed.get, PlainOutputStore.empty).rootNode))
  }
}
