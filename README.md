# A CLI for lpaste.net.

Pastepipe reads from stdin, posting to hpaste, and prints out the resulting url (the last line of output). Parameters control various hpaste form fields:

```
$ pastepipe --help
PastePipe v1.4, (C) Rogan Creswick 2009--2012

pastepipe [FLAG]

  -? --help[=FORMAT]      Show usage information (optional format)
  -V --version            Show version information
  -v --verbose            Higher verbosity
  -q --quiet              Lower verbosity
  --user=USER             Your user name (default is taken from $USER)
  -l --language=LANGUAGE  The language used for syntax highlighting (default=haskell)
  -t --title=TITLE        The title of the snippet
  -u --uri=URL            The URI of the lpaste instance to post to (default=http://lpaste.net/)
     --test               Prevents PastePipe from actually posting content, just echos the configuration and input
```

It will auto-detect your local username, but --user overrides this detection.

Parameters can come in any order, but only the first of duplicate entries will be used. So, if you have an alias to send to a local hpaste uri, then that alias should effectively disable the --uri switch. It is not possible to "disable" the --test or --help switches in this way, so you can always add --test to a command line to disable the actual sending of content.

# Installation

PastePipe is available on hackage (http://hackage.haskell.org/package/PastePipe) , so you can cabal install it, if you have a working cabal-install.

# Authors / contributors

 - Rogan Creswick
 - Brian Victor
 - Mateusz Kowalczyk
