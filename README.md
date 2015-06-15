funblog
=======

[![Build Status](https://travis-ci.org/agrafix/funblog.svg)](https://travis-ci.org/agrafix/funblog)

# Intro

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

This repository also provides the material for my upcoming talk at
[BobKonf 2015 Berlin](http://bobkonf.de/2015/thiemann.html) (German).

# Setup

1. Clone the github repository
2. Run `cabal install --only-dependencies`
3. Run `cabal build`
4. Adjust `blog.cfg`
5. Run `dist/build/funblog/funblog`

# License

Copyright 2014 - 2015 Alexander Thiemann

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
