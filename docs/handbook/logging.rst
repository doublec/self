Logging
=======

``log`` is a useful system-wide logging mechanism. You can find it in the ``system`` category of ``globals``.

How to log 
----------

There are a number of useful messages in the ``logging`` category of ``log`` which allow you to simply and cleanly log messages. For example::

  log warn: 'This is a warning.'
  
You can log with one of five levels found at ``log levels``. These are, in order of severity, ``debug``, ``info``, ``warn``, ``error``, ``fatal``.

You can also tag log entries, for example::

  log fatal: 'The server has caught fire' For: 'webserver'
  
By default, entries of either error or fatal severity which aren't tagged are logged to stderr in the form::

  [Thu Oct 23 16:25:07 2014] error -- Something went wrong!
  
How logging works
-----------------

The helper methods shown above constuct a ``log entry`` and hand it to the ``log dispatcher``. The dispatcher has a number of handlers, each is given a chance to handle the log entry. The handlers can choose which entries to act on. Example handlers are in ``log prototypeHandlers``.

When making a handler, please keep in mind that the log entry's ``message`` is expected to be something which understands ``value``, returning an object (or itself) which understands ``asString``. If you do not need to resolve the message by sending it ``value`` please don't; that way logs can be sent blocks which are only resolved if necessary; eg::

  log debug: ['We have reached: ', somethingComplicatedToCalculate]
  
will not slow down your code if no log handler is interested in handling debuggers.

If your handler breaks the logging process you can restart it by::

  log dispatcher hup

