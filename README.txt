Google data API for Scala
==========================================================

April  7, 2008

The GData package contains Scala bindings for Google Data APIs. It
consists of a generic XML pickling library, generic data classes and
picklers for Atom and Google common elements, authentication and
HTTP connection handling. It has specific bindings for the YouTube and
Calendar API.

Building
========

To build the project you need ant, and a Scala compiler. It has been
tested with version 2.7.0 (RC), but should work with the latest stable
release as well (2.6.1). Remember to have the environment variable
SCALA_HOME pointing to the Scala distribution (it should point to the
directory containing 'lib\scala-compiler.jar'. For 2.7.0 it is the directory
where you installed Scala, for earlier releases you need to append
'share/scala'). To build, type:

ant build

Tests
=====

To run the tests, type

ant test

If you have installed emma (http://emma.sourceforge.net/) you can get
a report of code coverage during testing. Change the 'emma.dir' ant
property to point to your the lib directory of your installation. Then
run 

ant coverage

This will build the tests with instrumentation and run the tests
again. You'll find an html report in the 'coverage/'
directory. Results are not very precise, since some synthetic methods
like $tag, are taken into accound and bring down the coverage
percentage.
