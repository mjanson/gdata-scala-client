Google data API for Scala
==========================================================

February  5, 2008

The GData package contains Scala bindings for Google Data APIs. For
the moment, it consists of a generic XML pickling library. It will
contain specific instantiations for several Google services, and a
communication library to tie everything together.

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

To run the tests, type

ant test
