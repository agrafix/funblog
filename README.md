funblog
=====

[![Build Status](https://travis-ci.org/agrafix/funblog.svg)](https://travis-ci.org/agrafix/funblog)


## Intro


Blog written in Haskell

## Cli Usage: funblog

```sh
$ funblog --help
Usage: funblog

Configure using the blog.cfg file

```

## Install

* From Source (cabal): `git clone https://github.com/agrafix/funblog.git && cd funblog && cabal install`
* From Source (stack): `git clone https://github.com/agrafix/funblog.git && cd funblog && stack build`

# About funblog

This is a simple blog software written in Haskell to demonstrate how to
use the web framework [Spock](http://github.com/agrafix/Spock). It
(will) provide(s) examples for:

* Routing
* Using template engines
* Writing a REST API (JSON)
* Working with Forms
* Using a database
* Session management
* Background-Workers
* Authentication
* Spock Contexts

This repository also provided the material for my talk at
[BobKonf 2015 Berlin](http://bobkonf.de/2015/thiemann.html) (German).

# Quickstart

1. Clone the github repository
2. Run `stack install`
3. Adjust `blog.cfg`
4. Run `funblog`

Please not that this software is NOT intendet to be used in production (yet?).

## Misc

### Supported GHC Versions

* 7.10.2

### License

Released under the Apache-2.0 license.
(c) 2014 - 2015 Alexander Thiemann
