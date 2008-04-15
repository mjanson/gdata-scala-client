Google data API for Scala
==========================================================

April  15, 2008

The Google Data package contains Scala bindings for Google Data APIs. It
consists of a generic XML pickling library, generic data classes and
picklers for Atom and Google common elements, authentication and
HTTP connection handling. For up to date information about what
services are supported, see the project webpage:

http://code.google.com/p/gdata-scala-client/

Building
========

To build the project you need ant, and a Scala compiler. It is known
that it does not work with the latesc stable release (2.7.0). It works
with 2.7.1 RC1. Remember to have the environment variable SCALA_HOME
pointing to the Scala distribution (it should point to the directory
containing 'lib\scala-compiler.jar'. For 2.7.0 and later it is the
directory where you installed Scala, for earlier releases you need to
append 'share/scala'). To build, type:

ant build

For more information, check the Developer's Guide:

http://code.google.com/p/gdata-scala-client/wiki/DevelopersGuide

Tests
=====

To run the tests, type

ant test

If you have installed emma (http://emma.sourceforge.net/) you can get
a report of code coverage during testing. Change the 'emma.dir' ant
property to point to your the lib directory of your installation. You
can do that by adding the following line in your 'build.properties'
file:

emma.dir=/path/to/emma-2.0.5312/lib

(of course, you might have a different version).

Then run 

ant coverage

This will build the tests with instrumentation and run the tests
again. You'll find an html report in the 'coverage/'
directory. Results are not very precise, since some synthetic methods
like $tag, are taken into accound and bring down the coverage
percentage.

For more information, see the Developer's Guide:

http://code.google.com/p/gdata-scala-client/wiki/DevelopersGuide
