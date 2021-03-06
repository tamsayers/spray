.. _-getFromFile-:

getFromFile
===========

Completes GET requests with the content of the given file.

Signature
---------

.. includecode:: /../spray-routing/src/main/scala/spray/routing/directives/FileAndResourceDirectives.scala
   :snippet: getFromFile

Description
-----------

The actual I/O operation is running detached in a `Future`, so it doesn't block the current thread (but potentially
some other thread !). If the file cannot be found or read the request is rejected.

To serve files from a directory use ``getFromDirectory``, instead. To serve a file from a classpath resource
use ``getFromResource`` instead.
