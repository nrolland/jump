http-client
===========

*If this is your first time reading a Jump tutorial, consider reading the [Jump
tutorial tips](https://github.com/commercialhaskell/jump/blob/master/TIPS.md).*

We'll be using the [http-client](https://www.stackage.org/package/http-client)
library as the base for making HTTP requests.  http-client itself is a
minimalistic package with a relatively low-level API and no support for TLS
(HTTPS). There are add-on packages which provide additional functionality,
e.g.:

* [http-client-tls](https://www.stackage.org/package/http-client-tls) provides TLS support via the Haskell-native tls package
* [http-conduit](https://www.stackage.org/package/http-conduit) allows for streaming request and responses using conduit

In this tutorial, we'll be using these packages together as one library for
making HTTP client requests.

## Concepts

All HTTP requests are made via a `Manager`. A `Manager` handles the details of
creating connections to servers. It handles things like reusing connections (to
avoid high TCP overhead when making multiple requests to the same host). It
also allows you to configure various settings, most important how to make
secure connections (HTTPS). For our purposes, you should use
`tlsManagerSettings` to ensure you have full HTTP and HTTPS support (as all
examples below do).

## Caveats

There are a few important caveats to mention about this library:

* By default, any non-2XX status code response results in a runtime exception.
  See the examples of `checkStatus` below for more information
* By default, http-client will respect the `http_proxy` and `https_proxy`
  environment variables. See the proxy examples below for information on how to
  bypass this.

## Kick the tires

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package http-client-tls
import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseUrl "http://httpbin.org/get"
    response <- httpLbs request manager

    putStrLn $ "The status code was: " ++
               show (statusCode $ responseStatus response)
    print $ responseBody response
```

We're using `newManager tlsManagerSettings` to get a new `Manager`, `parseUrl`
to parse a textual URL into a `Request`, and then making the request with
`httpLbs`. Once we have our `Response`, we can use standard accessors to
inspect its fields. 

## Streaming

`httpLbs` means "HTTP + lazy ByteString." Despite that implication, the
response body is _not_ lazily consumed; the lazy ByteString is just used for a
better memory representation. If you want more reliable memory usage, you can
stream the response:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package http-client-tls
import qualified Data.ByteString           as S
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import           System.IO                 (stdout)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseUrl "http://httpbin.org/get"

    withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

        let loop = do
                bs <- brRead $ responseBody response
                if S.null bs
                    then putStrLn "\nFinished response body"
                    else do
                        S.hPut stdout bs
                        loop
        loop
```

Streaming at this kind of low-level can be difficult, so using http-conduit can
be useful for these situations:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package http-conduit
import qualified Data.ByteString             as S
import           Data.Conduit                (($$))
import qualified Data.Conduit.List           as CL
import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.HTTP.Types.Status   (statusCode)
import           System.IO                   (stdout)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseUrl "http://httpbin.org/get"

    withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

        bodyReaderSource (responseBody response)
            $$ CL.mapM_ (S.hPut stdout)
```

## Receiving JSON

It's also straightforward to compose this streaming with aeson to parse JSON:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package http-conduit --package aeson
import           Data.Aeson.Parser           (json)
import           Data.Conduit                (($$))
import           Data.Conduit.Attoparsec     (sinkParser)
import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.HTTP.Types.Status   (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseUrl "http://httpbin.org/get"

    withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

        value <- bodyReaderSource (responseBody response)
              $$ sinkParser json
        print value
```

NOTE: A rich avenue for future development would be a convenience package for
doing this kind of parsing automatically. Perhaps it could even be added to
http-conduit.

## Sending JSON

Sending JSON can be done with modifying the request method and body:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package http-client-tls --package aeson
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson                 (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    -- Create the request
    let requestObject = object
            [ "name" .= ("Alice" :: String)
            , "age"  .= (35 :: Int)
            ]
    initialRequest <- parseUrl "http://httpbin.org/post"
    let request = initialRequest
            { method = "POST"
            , requestBody = RequestBodyLBS $ encode requestObject
            , requestHeaders =
                [ ("Content-Type", "application/json; charset=utf-8")
                ]
            }

    response <- httpLbs request manager
    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

NOTE: This is another case where convenience functions could be added.

Another common request body format is URL encoded bodies. The `urlEncodedBody`
function is a convenient function for doing this. Note that it automatically
sets the request method to `POST`, which we can override if desired:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package http-client-tls
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    initialRequest <- parseUrl "http://httpbin.org/put"
    let pairs =
            [ ("name", "Alice")
            , ("age", "35")
            ]
        request = (urlEncodedBody pairs initialRequest)
            { method = "PUT"
            }

    response <- httpLbs request manager
    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

## IsString instance for Request

As an extra convenience, instead of using `parseUrl`, you can turn on
`OverloadedStrings` and treat a string literal as a `Request`. Let's try
rewriting the previous example:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package http-client-tls
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    let pairs =
            [ ("name", "Alice")
            , ("age", "35")
            ]
        request = (urlEncodedBody pairs "http://httpbin.org/put")
            { method = "PUT"
            }

    response <- httpLbs request manager
    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

CAUTION: If you provide an invalid URL as a string literal, it will manifest as
a runtime exception when forcing the pure `Request` value, e.g.:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package http-client-tls
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    response <- httpLbs "BAD URL" manager
    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

generates:

```
foo.hs: InvalidUrlException "BAD URL" "Invalid URL"
```

## Non-2XX responses

By default, a non-2XX response (such as a 404 not found) will generate a
runtime exception. You can change this behavior with the `checkStatus` setting.
The code below will never throw exceptions based on the status code.

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package http-client-tls
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    -- Make a GET request to a POST-expecting endpoint, which will generate a
    -- 405 status code
    let request = "http://httpbin.org/post"
            { checkStatus = \_ _ _ -> Nothing
            }

    response <- httpLbs request manager
    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

NOTE: The decision to turn non-2XX responses into exceptions is one of the most
controversial decisions in this library, with strong arguments on each side. At
this point, the behavior is well established and won't be changing in the
future.

## Exceptions

There are other potential exceptions that may be thrown by this library, such
as due to failed connections. To catch these, you should catch the
`HttpException` exception type.

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package http-client-tls
{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception          (try)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    eresponse <- try $ httpLbs "http://does-not-exist" manager

    case eresponse of
        Left e -> print (e :: HttpException)
        Right response -> do
            putStrLn $ "The status code was: "
                    ++ show (statusCode $ responseStatus response)
            L8.putStrLn $ responseBody response
```

## Proxy settings

By default, http-client will respect the `http_proxy` and `https_proxy`
environment variables. You can modify this when creating your `Manager`:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package http-client-tls
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager $ managerSetProxy noProxy tlsManagerSettings

    response <- httpLbs "http://httpbin.org/get" manager

    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

You can also modify the proxy settings per-request:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package http-client-tls
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    let request = "http://httpbin.org/get"
            { proxy = Just $ Proxy "127.0.0.1" 3128
            }
    response <- httpLbs request manager

    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

If you set both the manager and request proxy overrides, the manager setting
will win out.

## Sharing the Manager

There is a small cost to initializing a `Manager`. More importantly, each
`Manager` maintains its own pool of connections. It is highly advisable to
share your `Manager` value throughout your application. This will decrease TCP
handshake overhead, and make it less likely that you will make too many
connections to a single server at once.

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package http-client-tls --package async
{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent.Async  (Concurrently (..))
import qualified Data.ByteString.Char8     as S8
import qualified Data.ByteString.Lazy      as L
import           Data.Foldable             (sequenceA_)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    runConcurrently $ sequenceA_ $ replicate 16
                    $ Concurrently $ doSomething manager

doSomething :: Manager -> IO ()
doSomething manager = do
    let request = "http://httpbin.org/get"

    response <- httpLbs request manager

    let msg = encodeUtf8 $ T.pack $ concat
            [ "Got a message with status code "
            , show $ statusCode $ responseStatus response
            , " with response body length "
            , show $ L.length $ responseBody response
            , "\n"
            ]

    -- Using bytestring-based output to avoid interleaving of string-based
    -- output
    S8.putStr msg
```
