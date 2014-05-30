funblog
=======

[![Build Status](https://travis-ci.org/agrafix/funblog.svg)](https://travis-ci.org/agrafix/funblog)

# Intro

This is a simple blog software written in Haskell to demonstrate how to use the webframework [Spock](http://github.com/agrafix/Spock). The project has two goals:

* Provide an example of how to write haskell web applications
* Provide a simple and robust blogging software written in Haskell

It will take some time to arrive at the second goal, but I hope we'll get there someday.

# Setup

1. Clone the github repository
2. Run `cabal install --only-dependencies`
3. Run `cabal build`
4. Adjust `blog.cfg`
5. Run `dist/build/funblog/funblog`

# License

Copyright 2014 Alexander Thiemann

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
