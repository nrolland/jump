Command-line argument parsing
=============================

For command-line argument parsing, we recommend the
[optparse-simple](https://www.stackage.org/package/optparse-simple) package.
This is actually a wrapper the around the powerful
[optparse-applicative](https://www.stackage.org/package/optparse-applicative),
and exposes most of its functionality but sets friendlier defaults and avoids
some boilerplate, which is especially desirable if you are building a basic
script-type program.

With this, you declare your program's options in one place, and get
nicely formatted `--help` text and bash auto-completion for free.

Basic example
-------------

Here's an example:

```haskell
import Options.Applicative.Simple

data Opts = Opts
    { flag :: Bool
    , val :: String }

main = do
    (opts,()) <- simpleOptions
        "0.0" -- version number
        "optparse example" -- header
        "A small example program for optparse-simple" -- description
        ((,) <$> switch (long "some-flag" <> help "Set the flag")
             <*> strOption
                     (long "some-value"
                      <> metavar "VALUE"
                      <> value "default"
                      <> help "Override default name"))
        empty
    putStrLn (concat ["Hello, ", val opts, "! The flag is ", show (flag opts)])
```

Without arguments, this program outputs `Hello, default! The flag is False`. If
you run this program with the `--help` argument, you get this output:

```
optparse example

Usage: optparse-example.hs [--version] [--help] [--some-flag]
                           [--some-value VALUE]
  A small example program for optparse-simple

Available options:
  --version                Show version
  --help                   Show this help text
  --some-flag              Set the flag
  --some-value VALUE       Override default name
```

Now use the arguments `--some-flag --some-value world`, and the output changes
to: `Hello, world! The flag is True`.

Sub-commands example
--------------------

It's also easy to add multiple sub-commands:

```haskell
import Options.Applicative.Simple

main = do
    (flag, runCmd) <- simpleOptions
        "0.0" -- version number
        "optparse subcommands example" -- header
        "A small example program for optparse-simple with subcommands" -- description
        (switch (long "global-flag" <> help "Set the global flag"))
        (do addCommand
                "create" -- subcommand name
                "Create a thing" -- subcommand help
                createThing -- function to call
                (strArgument (metavar "NAME")) -- subcommand argument(s)
            addCommand "delete" "Delete the thing" (const deleteThing) (pure ()))
    putStrLn $ concat ["The global flag is ", show flag]
    runCmd

deleteThing = putStrLn "Deleted the thing!"

createThing name = putStrLn (concat ["Created the thing named ", name])
```

Now you can pass the arguments `create foo`, and the output will be `The global
flag is False\nCreated the thing named foo`. In this case the subcommand only
has a single option, but as with global options you can put together multiple
option parsers using the Applicative operators (`Constructor <$> arg1 <*> arg2
<*> ...`).

Require one option out of a group of possibilities
---------------------------------------------------

The parser's Alternative instance lets you require one argument out of a group
of possibilities by separating them with `<|>`. For example, `(strOption (long
"foo") <|> strOption (long "bar"))` requires either the `--foo` _or_ `--bar`
option.

Parsing a other option types
----------------------------

So far, we have used `strOption` to parse a simple String option, but this will
not work if your option has a different datatype such as `Int`. For this case,
you can usually use `option auto` instead, which will use the Read instance to
parse the argument, and fail gracefully if it cannot be parsed.

It's also possible to define your own reader and pass it to `option` for custom
parsing. For example, this will parse a date in the "Jun 12, 1977" format:

```haskell
import Data.Time

day = eitherReader $ \arg ->
    case parseTimeM True defaultTimeLocale "%b %d, %Y" arg of
        Nothing -> Left ("Cannot parse date: " ++ arg)
        Just day -> Right day
```

Then use `option day (long "some-date")` for the option parser.

For options of type `Text`, you have to do things a bit differently. Using
`option auto` would require quotes around the argument value (which, in a shell,
means escaping the quotes like `\"text\"`) since that's what the Read
instance expects. Instead, use the parser's Functor instance to pack a regular
String to Text. For example, `fmap T.pack (strOption (long "some-text"))`.
Better yet, define your own `textOption`:

```haskell
import qualified Data.Text as T

textOption = fmap T.pack . strOption
```

Other parsers and options
-------------------------

These examples have only shown a few of the types of option parsers (`switch`
`strOption`, etc.) and their options (`long`, `help`, etc.), but you can read
the
[haddocks for Options.Applicative.Builder](http://hackage.haskell.org/package/optparse-applicative/docs/Options-Applicative-Builder.html)
(which is re-exported by Options.Applicative.Simple) for the full list. It's
worth browsing through the whole thing to get a sense of all the possibilities.

Package version number and Git commit ID
----------------------------------------

It's often nice take the version number from your Cabal package rather than have
to repeat it in the source code. To do that, `import Paths_your_module
(version)` (adjust "your_package" to match your package's name) and use
`version`. It's also nice to include the Git commit ID of your project in the
`--version` output traceability, which
[simpleVersion](http://hackage.haskell.org/package/optparse-simple-0.0.3/docs/Options-Applicative-Simple.html#v:simpleVersion)
makes easy.

Here's an example that puts them together. Notice that you must enable the
TemplateHaskell extension for `$(simpleVersion)`.

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Options.Applicative.Simple
import Paths_optparse_example (version)

main = do
    (str,()) <- simpleOptions $(simpleVersion version) "head" "desc"
                    (strArgument mempty) empty
    print str
```

Now the `--version` output looks like this: `Version 0.1.0.0, Git revision
9cc1dbd020c2a42e2bd93204d517470c5781bbf2`.

Using Auto-completion
---------------------

Every program using optparse-applicative gets hidden arguments to support bash
auto-completion. For your own personal use, you run this or add it this to your
`.bashrc`:

```sh
eval "$(myprog --bash-completion-script myprog)"
```

To install the bash completion system-wide, put the output of
--bash-completion-script where your system looks for it. For example, on Ubuntu:

```sh
myprog --bash-completion-script myprog >/etc/bash_completion.d/myprog
```

Using optparse-applicative directly
-----------------------------------

While optparse-simple covers most programs' needs, certain customizations may
require using optparse-applicative directly without the simple wrapper. It has
good
[getting started documentation](https://github.com/pcapriotti/optparse-applicative/blob/master/README.md)
already, so no sense repeating that here.
