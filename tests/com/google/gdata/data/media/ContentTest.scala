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


package com.google.gdata.data.media

import com.google.xml.combinators.{LinearStore, Picklers}
import com.google.xml.combinators.PicklerAsserts
import com.google.gdata.data.util.{SpecificTime, Now}

import org.junit._


/**
 * Test media content elements.
 */
class ContentTest extends PicklerAsserts {
  
  @Test def testRating {
    val input = (<media:rating xmlns:media="http://search.yahoo.com/mrss/"
                 scheme="urn:icra">r (cz 1 lz 1 nz 1 oz 1 vz 1)</media:rating>)
    val exp = Rating("urn:icra", "r (cz 1 lz 1 nz 1 oz 1 vz 1)")
    assertSucceedsWith("rating", exp, input, Rating.pickler)
    
    assertPicklesTo("rating pickling", input, exp, Rating.pickler)
  }

  @Test def testDefaultSchemeRating {
    val input = (<media:rating xmlns:media="http://search.yahoo.com/mrss/">adult</media:rating>)
    val expectedInput = (<media:rating xmlns:media="http://search.yahoo.com/mrss/"
                         scheme="urn:simple">adult</media:rating>)
    val expected = Rating("urn:simple", "adult")
    assertSucceedsWith("rating", expected, input, Rating.pickler)
    assertPicklesTo("rating pickling", expectedInput, expected, Rating.pickler) 
  }
  
  @Test def testTitle {
    val input = (<media:title xmlns:media="http://search.yahoo.com/mrss/"
        type="plain">The Judy's - The Moo Song</media:title>)
    val exp = SimpleText("plain", "The Judy's - The Moo Song")
    assertSucceedsWith("title", exp, input, SimpleText.pickler("title"))
    assertPicklesTo("title pickling", input, exp, SimpleText.pickler("title"))
  }
  
  @Test def testKeywords {
    val input = (<media:keywords xmlns:media="http://search.yahoo.com/mrss/">
                   kitty, cat, big dog, yarn, fluffy</media:keywords>)
    val exp = List("kitty", "cat", "big dog", "yarn", "fluffy")
    assertSucceedsWith("keywords", exp, input, Keywords.keywordsPickler)
    assertPicklesTo("keywords pickling", input, exp, Keywords.keywordsPickler)
  }
  
  @Test def testThumbs {
    val input = (<media:thumbnail xmlns:media="http://search.yahoo.com/mrss/"
                 url="http://www.foo.com/keyframe.jpg" width="75" height="50" time="12:05:01.123"/>)
    val exp = Thumbnail("http://www.foo.com/keyframe.jpg", Some(75), Some(50), Some(new SpecificTime(12, 5, 1, 123)))
    assertSucceedsWith("thumbnail", exp, input, Thumbnail.pickler)
    assertPicklesTo("thumbnail", input, exp, Thumbnail.pickler)
  }

  @Test def testThumbsNow {
    val input = (<media:thumbnail xmlns:media="http://search.yahoo.com/mrss/"
                 url="http://www.foo.com/keyframe.jpg" width="75" height="50" time="now"/>)
    val exp = Thumbnail("http://www.foo.com/keyframe.jpg", Some(75), Some(50), Some(Now))
    assertSucceedsWith("thumbnail", exp, input, Thumbnail.pickler)
    assertPicklesTo("thumbsNow", input, exp, Thumbnail.pickler)
  }

  @Test def testCategory {
    val input1 = (<media:category xmlns:media="http://search.yahoo.com/mrss/">music/artist/album/song</media:category>)
    val input2 = (<media:category xmlns:media="http://search.yahoo.com/mrss/"
        scheme="http://dmoz.org" label="Ace Ventura - PetDetective">
        Arts/Movies/Titles/A/Ace_Ventura_Series/Ace_Ventura_-_Pet_Detective</media:category>)
    val exp1 = Category("http://search.yahoo.com/mrss/category_schema", None, "music/artist/album/song")
    val exp2 = Category("http://dmoz.org", Some("Ace Ventura - PetDetective"),"""
        Arts/Movies/Titles/A/Ace_Ventura_Series/Ace_Ventura_-_Pet_Detective""")
    
    assertSucceedsWith("category-default", exp1, input1, Category.pickler)
    //assertPicklesTo("category pickling", input1, exp1, Category.pickler)
    
    assertSucceedsWith("category", exp2, input2, Category.pickler)
    assertPicklesTo("category pickling", input2, exp2, Category.pickler)
  }
  
  @Test def testPlayer {
    val input = (<media:player xmlns:media="http://search.yahoo.com/mrss/"
        url="http://www.foo.com/player?id=1111" height="200" width="400" />)
    val exp = Player("http://www.foo.com/player?id=1111", Some(200), Some(400))
    assertSucceedsWith("player", exp, input, Player.pickler)
    assertPicklesTo("player pickling", input, exp, Player.pickler)    
  }

  @Test def testHash {
    val input = (<media:hash xmlns:media="http://search.yahoo.com/mrss/"
        algo="md5">dfdec888b72151965a34b4b59031290a</media:hash>)
    val exp = Hash("md5", "dfdec888b72151965a34b4b59031290a")
    assertSucceedsWith("hash", exp, input, Hash.pickler)
    assertPicklesTo("hash pickling", input, exp, Hash.pickler)    
  }

  @Test def testCredit {
    val input = (<media:credit xmlns:media="http://search.yahoo.com/mrss/"
        role="producer" scheme="urn:ebu">entity name</media:credit>)
    val exp = Credit("urn:ebu", Some("producer"), "entity name")
    assertSucceedsWith("credit", exp, input, Credit.pickler)
    assertPicklesTo("credit pickling", input, exp, Credit.pickler)    
  }
  
  @Test def testCopyright {
    val input = (<media:copyright xmlns:media="http://search.yahoo.com/mrss/"
        url="http://blah.com/additional-info.html">2005 FooBar Media</media:copyright>)
    val exp = Copyright(Some("http://blah.com/additional-info.html"), "2005 FooBar Media")
    assertSucceedsWith("credit", exp, input, Copyright.pickler)
    assertPicklesTo("credit pickling", input, exp, Copyright.pickler)    
  }
  
  @Test def testText {
    val input = (<media:text  xmlns:media="http://search.yahoo.com/mrss/"
        type="plain" lang="en" start="00:00:03" end="00:00:10">Oh, say, can you see</media:text>)
    val exp = Text("plain", "Oh, say, can you see", Some("en"), 
        Some(SpecificTime(0, 0, 3, 0)), Some(SpecificTime(0, 0, 10, 0)))
    assertSucceedsWith("text", exp, input, Text.pickler)
    assertPicklesTo("text pickling", input, exp, Text.pickler)        
  }
  
  @Test def testRestriction {
    val input = (<media:restriction xmlns:media="http://search.yahoo.com/mrss/"
        relationship="allow" type="country">au us</media:restriction>)
    val exp = Restriction("allow", Some("country"), List("au", "us"))
    assertSucceedsWith("restriction", exp, input, Restriction.pickler)
    assertPicklesTo("restriction pickling", input, exp, Restriction.pickler)        
  }
  
  @Test def testGroup {
    val input = (
      <media:group xmlns:media="http://search.yahoo.com/mrss/">
        <media:content url="http://www.foo.com/song64kbps.mp3" 
        fileSize="1000" bitrate="64" type="audio/mpeg" 
        isDefault="true" expression="full"/>
        <media:content url="http://www.foo.com/song128kbps.mp3" 
        fileSize="2000" bitrate="128" type="audio/mpeg" 
        expression="full"/>
        <media:content url="http://www.foo.com/song256kbps.mp3" 
        fileSize="4000" bitrate="256" type="audio/mpeg" 
        expression="full"/>
        <media:content url="http://www.foo.com/song512kbps.mp3.torrent" 
        fileSize="8000" type="application/x-bittorrent;enclosed=audio/mpeg" 
        expression="full"/>
        <media:content url="http://www.foo.com/song.wav" 
        fileSize="16000" type="audio/x-wav" expression="full"/>
        <media:credit role="musician">band member 1</media:credit>
        <media:credit role="musician">band member 2</media:credit>
        <media:category>music/artist name/album/song</media:category>
        <media:rating>nonadult</media:rating>
        <media:copyright url="http://blah.com/additional-info.html">FooBar</media:copyright>
      </media:group>
    )
    
    /** A media group. */
    object mediaGroup extends MediaRss {
      import Picklers._
      
      type Group = BaseGroup
      type Content = BaseContent
      
      def groupContentsPickler = baseGroupPickler
      def contentContentsPickler = baseContentPickler
    }
    import mediaGroup._
    
    val Picklers.Success(group, left) = mediaGroup.groupPickler.unpickle(LinearStore.fromElem(input))
    Assert.assertEquals("content entries length", 5, group.contentEntries.length)
    Assert.assertEquals("credits length", 2, group.credits.length)
    Assert.assertEquals("rating", Some(Rating("urn:simple", "nonadult")), group.rating)
    Assert.assertEquals("category", 
        Some(Category("http://search.yahoo.com/mrss/category_schema", None, "music/artist name/album/song")),
        group.category)
    Assert.assertEquals("copyright", Some(Copyright(Some("http://blah.com/additional-info.html"), "FooBar")), 
        group.copyright)
  }
}

object ContentTestApp extends ContentTest with App {
  testGroup
}
