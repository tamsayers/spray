.. _-getFromResource-:

getFromResource
===============

Completes GET requests with the content of the given classpath resource.

Signature
---------

.. includecode:: /../spray-routing/src/main/scala/spray/routing/directives/FileAndResourceDirectives.scala
   :snippet: getFromResource

Description
-----------

The actual I/O operation is running detached in a `Future`, so it doesn't block the current thread (but potentially
some other thread !). If the file cannot be found or read the request is rejected.

To serve files from a classpath directory use ``getFromResourceDirectory`` instead. To serve files from a filesystem
directory use ``getFromDirectory``, instead.
