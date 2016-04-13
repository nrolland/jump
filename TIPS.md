Some tips on reading (and writing!) documentation.

## Prerequisites

This section needs to be filled out. The idea is to give a set of minimum
expected knowledge to follow a tutorial. If there is specific additional set of
knowledge needed to follow the tutorial, it should be listed at the top (and
ideally that topic should be covered by another Jump tutorial).

## Running code snippets

Code snippets that begin with `#!/usr/bin/env stack` can be run using [Stack's
script interpreter
mode](http://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter). To do
so:

* Make sure you have Stack installed
* Copy the entire snippet into a file ending with `.hs` (e.g., `foo.hs`)
* Run `stack foo.hs`
* Alternatively: `chmod +x foo.hs && ./foo.hs`

When writing tutorials, you should almost always write examples as fully
runnable snippets, together with a Stack script interpreter header. An example
is:

```
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package http-client-tls
```

Note how we include:

* `--install-ghc` to make sure GHC is automatically installed for the user if
  needed
* `--resolver` so that the user is using the same GHC version and package set
  as you
* An explicit list of `--package`s that must be installed. You need only
  provide "leaf" packages, anything that those packages depend on will be
  automatically installed

## Format

Each tutorial should follow this format:

*   Preamble: the following boilerplate text:

        *If this is your first time reading a Jump tutorial, consider reading the [Jump
        tutorial tips](https://github.com/commercialhaskell/jump/blob/master/TIPS.md).*

*   Intro: short explanation of what we're covering

*   Prerequisites: if any

*   Concepts: the minimal amount of prose necessary to orient the reader to what he/she will be learning

*   Content: lots of examples
