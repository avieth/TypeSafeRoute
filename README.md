# TypeSafeRoute

This experiment shows how the programmer can inform GHC of HTTP resources
(routes with named bindings, query parameters, request and response body types)
and profit from compile-time checks which guarantee that

  1. Parameter names are not shadowed.
  2. Routes of a server do not overlap.
  3. Handlers for a given route attempt to use only the data provided.

## An example

The file [Seddit](Examples/Seddit.hs) contains an example use of this library.
It implements, using Wai and the Warp web server, an HTTP server for posting
and retrieving titled text messages. Let's begin.

```Haskell
type Say = Root -/ "say"
type SaySingleTitle = Root -/ "say" =/ '("title", T.Text)
type SayManyTitles = Root -/ "say" =/ '("titles", [T.Text])
type SayListQuery = Q -? '("count", Maybe Integer) -? '("offset", Maybe Integer)
```

These are route and query definitions at the type level. The type `-/`
chains together symbolic route pieces (those which do not bind data to a
name), and `=/` introduces a binding. The route `SaySingleTitle` will bind
a single string found after `/say/` to the name `title`, and this will be
retrievable in any handler for this route.

`SayListQuery` describes a query string, consisting of optional `count` and
`offset` numbers. Any handler which uses this query type will have access
to these data.

```Haskell
type SedditRouter = '[
      -- Say something.
      '(POST, SaySingleTitle, Q, T.Text, ())
      -- Get something that was said.
    , '(GET,  SayManyTitles, Q, (), [Maybe T.Text])
      -- List the titles of things said.
    , '(GET,  Say, SayListQuery, (), [T.Text])
    ]
```

This type describes an HTTP server's routes. The third and fourth types in
each tuple are, respectively, the request body type, and response body type.
GETting the `SayManyTitles` route, for example, will return a list of optional
texts in the response body.

Now let's move on to define some handlers for these routes. First, we'll
need some notion of server state. We'll just store the data in a regular
Haskell map.

```Haskell
type SedditState = M.Map T.Text T.Text
```

To handle the first route, POSTs to `SaySingleTitle`, we need only to update
the map held in some `TVar`. The type of our `HTTPHandler` indicates that the
request is handled in `Kleisli IO`, which is just `IO` as an `Arrow`. It
indicates as well that no query parameters are expected, and that an request
body of type `Text` is expected.

```Haskell
handlePostSay
    :: TVar SedditState
    -> HTTPHandler (Kleisli IO) POST SaySingleTitle '[] T.Text ()
handlePostSay state = Kleisli $ \httpData ->
    let title = routeValue httpData (Proxy :: Proxy "title")
        text = requestBody httpData
    in  do atomically $ do
               seddits <- readTVar state
               writeTVar state (M.alter (const (Just text)) title seddits)
           return (response200 ())
```

As you can see, we use type literals to obtain route parameters, in this
case the title of the post.
The type parameters shape the input `httpData`'s type. As part of type
checking, GHC ensures that `title` will indeed be provided, and of type
`T.Text`. Similarly for `text`, that it *will* be present and of type `T.Text`.

```Haskell
handleGetSay
    :: TVar SedditState
    -> HTTPHandler (Kleisli IO) GET SayManyTitles '[] () [Maybe T.Text]
handleGetSay state = Kleisli $ \httpData ->
    let titles = routeValue httpData (Proxy :: Proxy "titles")
    in  do texts <- atomically $ do
               seddits <- readTVar state
               return (foldr (\title ts -> M.lookup title seddits : ts) [] titles)
           return (response200 texts)
```

There's nothing new here. To get posts, we grab the titles, which is guaranteed
to be a list of `T.Text`, and we respond with the associated messages. GHC
checks that the response we give really is of type `[Maybe T.Text]`.

The next handler will list message titles subjects to an offset and count.

```Haskell
handleListSay
    :: TVar SedditState
    -> HTTPHandler (Kleisli IO) GET Say SayListQuery () [T.Text]
handleListSay state = Kleisli $ \httpData ->
    let count = maybe 10 id (queryValue httpData (Proxy :: Proxy "count"))
        offset = maybe 0 id (queryValue httpData (Proxy :: Proxy "offset"))
    in  do titles <- atomically $ do
               seddits <- readTVar state
               let orderedList = fmap fst (M.toAscList seddits)
               return (listSlice offset count orderedList)
           return (response200 titles)
  where
    listSlice :: Integer -> Integer -> [a] -> [a]
    listSlice offset count xs
        | offset < 0 || count < 0 = []
        | otherwise = case (offset, count, xs) of
            (0, 0, _) -> []
            (0, n, x : rest) -> x : listSlice 0 (n-1) rest
            (n, m, x : rest) -> listSlice (n-1) m rest
            _ -> []
```

To obtain a working server, we start by bundling our handlers into an
`HTTPHandlers` value. This GADT ensures that there's no route overlap, and
that all routes are sane.

```Haskell
handlers
    :: TVar SedditState
    -> HTTPHandlers (Kleisli IO) SedditRouter
handlers tvar = AddHandler (handlePostSay tvar)
              $ AddHandler (handleGetSay tvar)
              $ AddHandler (handleListSay tvar)
              $ NoHandlers
```

The Wai `Application` is now had using `waiApplication` as defined in
[HTTP](TypeSafeRoute/HTTP.hs).

```Haskell
application :: TVar SedditState -> Wai.Application
application tvar = waiApplication id (httpServer id id (handlers tvar))

main = newTVarIO M.empty >>= run 7777 . application
```

Try it out via curl!

```Bash
$ curl -X POST localhost:7777/say/message1 -d "Haskell"
$ curl -X POST localhost:7777/say/message2 -d "Rules"
$ curl -X GET localhost:7777/say?count=2
message1,message2
$ curl -X GET localhost:7777/say/message1,message2
Just Haskell,Just rules
```
