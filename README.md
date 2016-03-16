# jump

[![Build Status](https://travis-ci.org/commercialhaskell/jump.svg?branch=master)](https://travis-ci.org/commercialhaskell/jump)

_Jump start your Haskell development_

__NOTE__: This package is currently just a proof of concept. If you're
interested in it, please discuss on the [Commercial Haskell mailing
list](http://commercialhaskell.com).

## What this is

This package is meant to give a concrete answer to the question "how do I start
writing real world Haskell code?" In particular, this package aims to provide
the following:

* A great set of recommended packages for many common problem domains
* Additional functionality of its own not provided by those other packages
* Comprehensive documentation on how to use this all together
* Example code demonstrating how to do things idiomatically
* A powerful prelude replacement to make it easy and fast to leverage all of
  this

If you've got a solid grasp of the Haskell language itself, and are looking to
dive into a real application, this package is intended for you. If you're
looking for resources on learning the language itself, please consider:

* Books
    * [Haskell Programming from first principles](http://haskellbook.com/)
    * [Learn You a Haskell for Great Good](http://learnyouahaskell.com/)
    * [Real World Haskell](http://book.realworldhaskell.org/)
* [School of Haskell](https://www.schoolofhaskell.com/)

## Opinionated

This package is opinionated. Instead of offering you lots of different ways of
doing something, it will give you a recommended approach. In the case of
multiple choices being relevant (e.g., time vs space complexity trade-offs), it
will describe both solutions and why each one is available.

The opinions used to fuel these decisions are based on lots of real world
Haskell development experience, but they are subjective nonetheless. If you
disagree with the choices used here, you'll probably be happier going for a
different set of libraries or another prelude replacement (such as
[base-prelude](https://www.stackage.org/package/base-prelude),
[basic-prelude](https://www.stackage.org/package/basic-prelude),
[lens](https://www.stackage.org/package/lens), or
[classy-prelude](https://www.stackage.org/package/classy-prelude)).

To give you an idea, here are some of the opinions that guide this library:

* Partial functions are bad and should be avoided
* High-performance data structures like `Vector`, `HashMap`, and `Text` should
  be preferred
* Functions generalized over time-tested typeclasses like `Foldable`,
  `Traversable`, `Monoid`, `Applicative`, and `MonadIO` should be preferred
* Lazy I/O should be avoided
* Lens-inspired getters and setters should be easily available
* mtl-style classes should be readily available

## How to use it

To build a new project with Jump, first [download
Stack](http://haskellstack.com/). __FIXME__ Once Jump is officially released,
update instructions to either use a preexisting Jump template or something
similar.

You can [view the generated module documentation on
stackage.org](https://www.stackage.org/package/jump). Start off with the `Jump`
module and move on from there.

## Officially endorsed packages

We break down our endorsed packages into two groups. The first group is
"fundamental packages," which provide basic functionality that we consider
common to most programs. These packages not only are recommended for use, but
may also be used when developing this package as dependencies, and may have
their functionality exported here.  These are:

* async
* bytestring
* containers
* exceptions
* mtl
* stm
* text
* transformers
* unordered-containers
* vector

The second group are the "companion packages," which provide less commonly used
functionality, and therefore should not be a dependency of this package, but
are still best-in-class package. These are:

* [cryptonite](https://www.stackage.org/package/cryptonite)
* [wai](https://www.stackage.org/package/wai)
* [warp](https://www.stackage.org/package/warp)

### TODOs

* Flesh out these lists more fully
* Should we include a short description of functionality next to each
  package?
* Prior art.  Let's collect here prior art on such "recommended package"
  lists and see which ones can be promoted here:
    * https://haskelliseasy.readthedocs.org/en/latest/
    * https://www.schoolofhaskell.com/user/snoyberg/ide-documentation/recommended-libraries
    * https://github.com/Gabriel439/post-rfc/blob/master/sotu.md

## Desired functionality

The following is a list of functionality we wish to implement in this library
(or in another library we would depend on), but do not believe any current
package available in the ecosystem properly provides.

* A `Display` typeclass, so we stop abusing `Show` for user-friendly output

## Interesting differences

Not everything in Jump behaves the way the standard Prelude module or base
package behave. Some notable differences:

* We prefer the more efficient `Text` type instead of `String`
* Instead of using `type FilePath = [Char]`, we have `FilePath` as an abstract
  type. (See
  [motivation](https://ghc.haskell.org/trac/ghc/wiki/Proposal/AbstractFilePath).)
* Whenever possible, we provide lifted functions (based on `MonadIO`,
  `MonadThrow`, `MonadBaseControl`, etc)
* We avoid exposing partial functions and functions using lazy I/O, and when we
  do expose such functions, make sure the names make these issues clear.
* We prefer binary I/O instead of textual I/O in most cases

## Difference from the Haskell Platform

There is a distinct overlap in goals between this project and the Haskell
Platform. Let's establish why we think this approach is different:

* The Haskell Platform combines a recommended set of libraries with an
  installer. Instead, Jump is meant as a companion to whichever method you use
  for getting your Haskell toolchain up-and-running (though we recommend
  [Stack](http://haskellstack.com)). This allows people interested in the
  toolchain but not the opinionated choices made here, or vice-versa, to
  pick-and-choose what they want.
* The Haskell Platform has a committee based process for admitting new
  packages, and getting consensus through that process has been an aruduous
  task. By being opinionated here and not looking for universal consensus, we
  can move faster. (And we strongly encourage those with different opinions to
  start projects similar to this with those other biases instead!)
* The Haskell Platform grandfathered in a few preexisting packages - like cgi
  and xhtml - which are widely accepted as not being recommended Haskell coding
  practice. If Jump uses something, it will be because we think it's the best
  way to do it.
* Instead of just curating existing packages, Jump's goal is to create new
  content. The largest focus for us here will actually be on creating better
  documentation for the already available and excellent packages out there. We
  will also be creating new functionality as necessary.
