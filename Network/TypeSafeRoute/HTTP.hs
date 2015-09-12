{-|
Module      : TypeSafeRoute.HTTP
Description : Statically checked HTTP handlers.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module TypeSafeRoute.HTTP where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Void
import Data.Proxy
import Data.Profunctor
import Data.Functor.Identity
import Data.String (fromString)
import Data.Char (isAlphaNum)
import Control.Freedom.Construction
import Control.Applicative
import Control.Arrow
import TypeSafeRoute.Server
import qualified Network.HTTP.Types as H
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Map as M
import qualified Data.Attoparsec.Text as PT
import qualified Data.Attoparsec.ByteString as PB
import qualified Data.Attoparsec.ByteString.Char8 as PB8
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai.Internal
import qualified Blaze.ByteString.Builder.ByteString as BSBuilder

type QueryMap = M.Map T.Text (Maybe T.Text)

data HTTPRequest t = HTTPRequest {
      httpRequestMethod :: H.Method
    , httpRequestHeaders :: H.RequestHeaders
    , httpRequestPath :: [T.Text]
    , httpRequestQuery :: QueryMap
    , httpRequestBody :: t
    }

deriving instance Functor HTTPRequest
deriving instance Show t => Show (HTTPRequest t)

data HTTPResponse t = HTTPResponse {
      httpResponseStatus :: H.Status
    , httpResponseHeaders :: H.ResponseHeaders
    , httpResponseBody :: t
    }

deriving instance Functor HTTPResponse
deriving instance Show t => Show (HTTPResponse t)

response200 :: t -> HTTPResponse t
response200 = HTTPResponse H.status200 []

data HTTPMethod where
    GET :: HTTPMethod
    PUT :: HTTPMethod
    POST :: HTTPMethod
    DELETE :: HTTPMethod

data RoutePieceT where
    RoutePieceSymbolT :: RoutePieceT
    RoutePieceSingleT :: RoutePieceT

data RoutePiece t where
    RoutePieceSymbol :: RoutePiece '(RoutePieceSymbolT, sym, Void)
    RoutePieceSingle :: t -> RoutePiece '(RoutePieceSingleT, sym, t)

type family RoutePieceOverlap piece route where
    RoutePieceOverlap piece '[] = False
    RoutePieceOverlap '(RoutePieceSymbolT, sym, s) rs = False
    RoutePieceOverlap '(RoutePieceSingleT, sym, s) ( '(RoutePieceSingleT, sym, t) ': rs ) = True
    RoutePieceOverlap '(RoutePieceSingleT, sym, s) ( '(pieceType, sym', t) ': rs) = RoutePieceOverlap '(RoutePieceSingleT, sym, s) rs


type family InvalidRoute route where
    InvalidRoute '[] = False
    InvalidRoute (r ': rs) = Or (RoutePieceOverlap r rs) (InvalidRoute rs)

data Route route where
    Root :: Route '[]
    Route :: RoutePiece t -> Route ts -> Route (t ': ts)

data QueryPieceT where
    QueryPieceSingleT :: QueryPieceT

data QueryPiece t where
    QueryPieceSingle :: t -> QueryPiece '(QueryPieceSingleT, sym, t)

type family QueryPieceOverlap piece query where
    QueryPieceOverlap piece '[] = False
    QueryPieceOverlap '(any1, sym, s) ( '(any2, sym, t) ': qs ) = True
    QueryPieceOverlap '(any1, sym1, s) ( '(any2, sym2, t) ': qs ) = QueryPieceOverlap '(any1, sym1, s) qs

type family InvalidQuery query where
    InvalidQuery '[] = False
    InvalidQuery (q ': qs) = Or (QueryPieceOverlap q qs) (InvalidQuery qs)

data Query query where
    EndQuery :: Query '[]
    Query :: QueryPiece t -> Query ts -> Query (t ': ts)

data Body body where
    Body :: body -> Body body

data HTTP (method :: HTTPMethod) route query body where
    HTTP :: Route route -> Query query -> Body body -> HTTP method route query body

routeValue
    :: InRoute sym route
    => HTTP method route query body
    -> Proxy sym
    -> InRouteType sym route
routeValue http proxy = case http of
    HTTP route _ _ -> inRouteLookup proxy route

queryValue
    :: InQuery sym query
    => HTTP method route query body
    -> Proxy sym
    -> InQueryType sym query
queryValue http proxy = case http of
    HTTP _ query _ -> inQueryLookup proxy query

requestBody
    :: HTTP method route query body
    -> body
requestBody http = case http of
    HTTP _ _ (Body b) -> b

httpNoData :: HTTP method '[] '[] ()
httpNoData = HTTP Root EndQuery (Body ())

type HTTPHandler a method route query body output = a (HTTP method route query body) (HTTPResponse output)

noRoute :: Arrow a => HTTPHandler a method '[] '[] () ()
noRoute = arr (\_ -> HTTPResponse H.status404 [] ())

noMethod :: Arrow a => HTTPHandler a method '[] '[] () ()
noMethod = arr (\_ -> HTTPResponse H.status405 [] ())

-- TODO would be cool if we could throw in a reason, like which parameter
-- failed to parse.
badRequest :: Arrow a => HTTPHandler a method '[] '[] () ()
badRequest = arr (\_ -> HTTPResponse H.status403 [] ())

data HTTPHandlers a resources where
    NoHandlers :: HTTPHandlers a '[]
    AddHandler
        -- It's here where we check that the type parameters are sane.
        -- You can construct bogus @HTTPHandler@s, but you cannot use them
        -- in an @HTTPHandlers@ value.
        :: ( ResourceOverlap '(method, route) rs ~ False
           , InvalidRoute route ~ False
           , InvalidQuery query ~ False
           , MatchMethod method
           , MatchRoute route
           , MatchQuery query
           , HTTPBody body
           , HTTPBody output
           )
        => HTTPHandler a method route query body output
        -> HTTPHandlers a rs
        -> HTTPHandlers a ( '(method, route, query, body, output) ': rs )

type family Or (a :: Bool) (b :: Bool) :: Bool where
    Or False False = False
    Or a b = True

-- Two routes overlap precisely when they are of equal length and all of their
-- corresponding symbolic components (strings between the slashes) are the same.
type family RouteOverlap q r where
    RouteOverlap '[] '[] = True
    RouteOverlap ( '(RoutePieceSymbolT, sym, t) ': qs ) ( '(RoutePieceSymbolT, sym, s) ': rs ) = RouteOverlap qs rs
    -- Two distinct symbols encountered; we know there's no overlap.
    RouteOverlap ( '(RoutePieceSymbolT, sym1, t) ': qs ) ( '(RoutePieceSymbolT, sym2, s) ': rs) = False
    -- If we encounter any two non-symbolic pieces (RoutePieceSymbolT already
    -- covered above) then the route may still overlap.
    RouteOverlap ( '(piece1, sym1, t) ': qs ) ( '(piece2, sym2, s) ': rs ) = RouteOverlap qs rs
    RouteOverlap qs '[] = False
    RouteOverlap '[] rs = False

-- Identify if some resource overlaps with any of a list of resources.
--
--   1. /foo/<bar>/baz
--   2. /<foo>/bar/baz
--   3. /bar/foo
--
-- <x> means that part of the route is a variable binding.
-- Here, 1 and 2 overlap if they have the same method, because there's no way
-- to distinguish the two; either could be interpreted as the other.
-- But 3 overlaps with neither, since the first route piece differs from that
-- of 1, and the second route piece differs from that of 2.
--
-- The rule: two resources overlap if they have the same method, and their
-- routes are of the same length and do not contain one pair of distinct symbols
-- (non-bindings) at the same index.
type family ResourceOverlap r rs where
    ResourceOverlap r '[] = False
    ResourceOverlap '(method, route1) ( '(method, route2, query, body, output) ': rs ) = Or (RouteOverlap route1 route2) (ResourceOverlap '(method, route1) rs)
    ResourceOverlap '(method1, route1) ( '(method2, route2, query, body, output) ': rs ) = ResourceOverlap '(method1, route1) rs

type family InRouteType (t :: Symbol) (ts :: [(RoutePieceT, Symbol, *)]) :: * where
    InRouteType sym ( '(RoutePieceSingleT, sym, t) ': ts ) = t
    InRouteType sym ( s ': ts ) = InRouteType sym ts
    InRouteType sym '[] = Void

class InRoute (sym :: Symbol) (ts :: [(RoutePieceT, Symbol, *)]) where
    inRouteLookup :: Proxy sym -> Route ts -> InRouteType sym ts

instance {-# OVERLAPPING #-} InRoute sym ( '(RoutePieceSingleT, sym, t) ': ts ) where
    inRouteLookup _ rd = case rd of
        Route (RoutePieceSingle x) _ -> x

instance {-# OVERLAPPABLE #-}
    ( InRoute sym ts
    , InRouteType sym (s ': ts) ~ InRouteType sym ts
    ) => InRoute sym (s ': ts)
  where
    inRouteLookup proxy rd = case rd of
        Route _ rest -> inRouteLookup proxy rest

type family InQueryType (q :: Symbol) (qs :: [(QueryPieceT, Symbol, *)]) :: * where
    InQueryType sym ( '(QueryPieceSingleT, sym, t) ': qs ) = t
    InQueryType sym ( q ': qs ) = InQueryType sym qs
    InQueryType sym '[] = Void

class InQuery (sym :: Symbol) (qs :: [(QueryPieceT, Symbol, *)]) where
    inQueryLookup :: Proxy sym -> Query qs -> InQueryType sym qs

instance {-# OVERLAPPING #-} InQuery sym ( '(QueryPieceSingleT, sym, t) ': qs ) where
    inQueryLookup _ q = case q of
        Query (QueryPieceSingle x) _ -> x

instance {-# OVERLAPPABLE #-}
    ( InQuery sym qs
    , InQueryType sym (q ': qs) ~ InQueryType sym qs
    ) => InQuery sym (q ': qs)
  where
    inQueryLookup proxy q = case q of
        Query _ rest -> inQueryLookup proxy rest

class MatchMethod method where
    matchMethod :: Proxy method -> H.Method -> Bool

instance MatchMethod GET where
    matchMethod _ = (==) H.methodGet

instance MatchMethod PUT where
    matchMethod _ = (==) H.methodPut

instance MatchMethod POST where
    matchMethod _ = (==) H.methodPost

instance MatchMethod DELETE where
    matchMethod _ = (==) H.methodDelete

class MatchRoute route where
    matchRoute :: [T.Text] -> Either () (Route route)

instance MatchRoute '[] where
    matchRoute texts = case texts of
        (_ : _) -> Left ()
        [] -> Right Root

instance
    ( KnownSymbol sym
    , MatchRoute ts
    ) => MatchRoute ( '(RoutePieceSymbolT, sym, Void) ': ts )
  where
    matchRoute texts = case texts of
        (text : rest) -> case text == T.pack (symbolVal (Proxy :: Proxy sym)) of
            True -> Route <$> Right RoutePieceSymbol <*> matchRoute rest
            False -> Left ()
        [] -> Left ()

instance
    ( HTTPParameter t
    , MatchRoute ts
    ) => MatchRoute ( '(RoutePieceSingleT, sym, t) ': ts )
  where
    matchRoute texts = case texts of
        (text : rest) -> (\x y -> Route (RoutePieceSingle x) y) <$> parsed <*> matchRoute rest
          where
            parsed = case PT.parseOnly parseHttpParameter text of
                Right x -> Right x
                Left _ -> Left ()
        [] -> Left ()

class MatchQuery query where
    matchQuery :: QueryMap -> Either () (Query query)

instance MatchQuery '[] where
    matchQuery queryMap = Right EndQuery

instance
    ( QueryParameter t
    , MatchQuery ts
    , KnownSymbol sym
    ) => MatchQuery ( '(QueryPieceSingleT, sym, t) ': ts )
  where
    matchQuery queryMap = (\x y -> Query (QueryPieceSingle x) y) <$> parsed <*> matchQuery restOfMap
      where
        key = T.pack (symbolVal (Proxy :: Proxy sym))
        restOfMap = M.delete key queryMap
        value = M.lookup key queryMap
        parsed = parseQueryParameter value

class QueryParameter t where
    parseQueryParameter :: Maybe (Maybe T.Text) -> Either () t
    printQueryParameter :: t -> Maybe T.Text

instance QueryParameter Bool where
    parseQueryParameter bs = case bs of
        Just (Just bs') -> Right True
        _ -> Right False
    printQueryParameter = Just . T.pack . show

instance HTTPParameter t => QueryParameter (Identity t) where
    parseQueryParameter ts = case ts of
        Just (Just ts) -> case PT.parseOnly parseHttpParameter ts of
            Right x -> Right (Identity x)
            Left _ -> Left ()
        _ -> Left ()
    printQueryParameter = Just . printHttpParameter . runIdentity

instance HTTPParameter t => QueryParameter (Maybe t) where
    parseQueryParameter ts = case ts of
        Nothing -> Right Nothing
        Just Nothing -> Right Nothing
        Just (Just ts') -> case PT.parseOnly parseHttpParameter ts' of
            Right x -> Right (Just x)
            Left _ -> Left ()
    printQueryParameter = fmap printHttpParameter

instance HTTPParameter t => QueryParameter [t] where
    parseQueryParameter ts = case ts of
        Just (Just tss) -> case PT.parseOnly (PT.sepBy1 (parseHttpParameter) (PT.char ',')) tss of
            Right xs -> Right xs
            Left _ -> Left ()
        _ -> Left ()
    printQueryParameter = Just . T.intercalate (T.pack ",") . fmap printHttpParameter

class HTTPParameter t where
    parseHttpParameter :: PT.Parser t
    printHttpParameter :: t -> T.Text

instance HTTPParameter Integer where
    parseHttpParameter = do
        x <- PT.number
        case x of
            PT.I int -> pure int
            _ -> empty
    printHttpParameter = T.pack . show

instance HTTPParameter T.Text where
    parseHttpParameter = PT.takeWhile isAlphaNum
    printHttpParameter = id

instance HTTPParameter t => HTTPParameter [t] where
    parseHttpParameter = PT.sepBy parseHttpParameter (PT.char ',')
    printHttpParameter = T.intercalate (T.pack ",") . fmap printHttpParameter

-- Any HTTPBody must have an injection into ByteString, which we express as
-- a parser rather than a straight up function.
class HTTPBody t where
    parseHttpBody :: PB.Parser t
    printHttpBody :: t -> BS.ByteString

instance HTTPBody () where
    parseHttpBody = pure ()
    printHttpBody = const BS.empty

instance HTTPBody BS.ByteString where
    parseHttpBody = PB.takeByteString
    printHttpBody = id

instance HTTPBody T.Text where
    parseHttpBody = do
        bs <- PB.takeByteString
        case decodeUtf8' bs of
            Left _ -> empty
            Right x -> pure x
    printHttpBody = encodeUtf8

instance HTTPBody t => HTTPBody [t] where
    parseHttpBody = PB.sepBy1 parseHttpBody (PB8.char ',')
    printHttpBody = BS.intercalate (B8.pack ",") . fmap printHttpBody

instance HTTPBody t => HTTPBody (Maybe t) where
    parseHttpBody = (PB8.string "Just " *> (Just <$> parseHttpBody)) <|> (PB8.string "Nothing" *> pure Nothing)
    printHttpBody x = case x of
        Nothing -> "Nothing"
        Just x' -> BS.append (B8.pack "Just ") (printHttpBody x')

type family TermSymbol t where
    TermSymbol '(sym, t) = sym

type family TermType t where
    TermType '(sym, t) = t

type family Snoc t ts where
    Snoc t '[] = '[t]
    Snoc t (u ': ts) = u ': (Snoc t ts)

infixl 8 -/
type route -/ symbol = Snoc '(RoutePieceSymbolT, symbol, Void) route

infixl 8 =/
type route =/ (term :: (Symbol, *)) = Snoc '(RoutePieceSingleT, TermSymbol term, TermType term) route

type Root = '[]

infixl 8 -?
type query -? term = Snoc '(QueryPieceSingleT, TermSymbol term, TermType term) query

type Q = '[]

-- Take the HTTPRequest, match a handler, and run it.
handle :: ArrowApply a => HTTPHandlers a routes -> a (HTTPRequest BS.ByteString) (HTTPResponse BS.ByteString)
handle httpHandlers = proc httpRequest -> do
    httpResponse <- app -< handleSomeHandler (matchHandler httpRequest httpHandlers)
    returnA -< httpResponse

data SomeHandler a where
    SomeHandler :: (a (HTTP method route query body) (HTTPResponse BS.ByteString), HTTP method route query body) -> SomeHandler a

handleSomeHandler :: (Arrow a) => SomeHandler a -> (a () (HTTPResponse BS.ByteString), ())
handleSomeHandler someHandler = case someHandler of
    SomeHandler (arrow, input) -> (arr (const input) >>> arrow, ())

matchHandler
    :: forall routes method route query body a .
       ( Arrow a
       )
    => (HTTPRequest BS.ByteString)
    -> HTTPHandlers a routes
    -> SomeHandler a
matchHandler httpRequest handlers = case handlers of
    NoHandlers -> SomeHandler (noRoute >>> arr (fmap printHttpBody), httpNoData)
    AddHandler (handler :: HTTPHandler a thisMethod thisRoute thisQuery thisBody thisOutput) rest ->
        case matchMethod (Proxy :: Proxy thisMethod) (httpRequestMethod httpRequest) :: Bool of
            False -> matchHandler httpRequest rest
            True -> case matchRoute (httpRequestPath httpRequest) :: Either () (Route thisRoute) of
                Left () -> matchHandler httpRequest rest
                Right route -> case matchQuery (httpRequestQuery httpRequest) :: Either () (Query thisQuery) of
                    -- Do not recurse! The route matches, but the query parameters
                    -- don't; that counts as a bad request.
                    Left () -> SomeHandler (badRequest >>> arr (fmap printHttpBody), httpNoData)
                    Right query -> case PB.parseOnly parseHttpBody (httpRequestBody httpRequest) :: Either String thisBody of
                        Left _ -> SomeHandler (badRequest >>> arr (fmap printHttpBody), httpNoData)
                        Right body -> SomeHandler (handler >>> arr (fmap printHttpBody), HTTP route query (Body body))

httpServer
    :: ArrowApply a
    => (req -> HTTPRequest BS.ByteString)
    -> (HTTPResponse BS.ByteString -> res)
    -> HTTPHandlers a router
    -> Server a req res
httpServer makeRequest makeResponse handlers = Server $ proc req -> do
    httpResponse <- handle handlers -< makeRequest req
    returnA -< makeResponse httpResponse

waiApplication
    :: (forall s t . a s t -> Kleisli IO s t)
    -> Server a (HTTPRequest BS.ByteString) (HTTPResponse BS.ByteString)
    -> Wai.Application
waiApplication makeIO server req response = do
    httpRequest <- waiRequestToHTTPRequest req
    out <- runKleisli (makeIO (runServer server)) httpRequest
    let waiResponse = httpResponseToWaiResponse out
    response waiResponse

waiRequestToHTTPRequest :: Wai.Internal.Request -> IO (HTTPRequest BS.ByteString)
waiRequestToHTTPRequest req = do
    let method = Wai.Internal.requestMethod req
    let path = Wai.Internal.pathInfo req
    let headers = Wai.Internal.requestHeaders req
    let query = Wai.Internal.queryString req
    let queryMap = makeQueryMap (H.queryToQueryText query)
    body <- waiForceRequestBody req
    return (HTTPRequest method headers path queryMap body)
  where
    makeQueryMap = M.fromList
    waiForceRequestBody req = do
        chunk <- Wai.Internal.requestBody req
        if chunk == BS.empty
        then return chunk
        else BS.append chunk <$> waiForceRequestBody req

httpResponseToWaiResponse :: HTTPResponse BS.ByteString -> Wai.Internal.Response
httpResponseToWaiResponse httpResponse =
    let status = httpResponseStatus httpResponse
        headers = httpResponseHeaders httpResponse
        body = httpResponseBody httpResponse
    in  Wai.Internal.ResponseBuilder status headers (BSBuilder.fromByteString body)

type family RouteTypes (r :: [(RoutePieceT, Symbol, *)]) :: [*] where
    RouteTypes '[] = '[]
    RouteTypes ( '(RoutePieceSymbolT, sym, Void) ': rest ) = RouteTypes rest
    RouteTypes ( '(RoutePieceSingleT, sym, t) ': rest ) = t ': RouteTypes rest

type family QueryTypes (q :: [(QueryPieceT, Symbol, *)]) :: [*] where
    QueryTypes '[] = '[]
    QueryTypes ( '(QueryPieceSingleT, sym, t) ': rest ) = t ': QueryTypes rest

data HList (ts :: [*]) where
    HNil :: HList '[]
    HCons :: t -> HList ts -> HList (t ': ts)

-- We can use a route type to make a path suitable for use in a URL.
-- The texts will not be escaped; use encodePathSegments from
-- Network.HTTP.Types.URI to obtain a Builder for the path.
class MakePath (r :: [(RoutePieceT, Symbol, *)]) where
    makePath :: Proxy r -> HList (RouteTypes r) -> [T.Text]

instance MakePath '[] where
    makePath _ _ = []

instance
    ( KnownSymbol sym
    , MakePath ts
    ) => MakePath ( '(RoutePieceSymbolT, sym, Void) ': ts )
  where
    -- It's right to put this portion in the LHS of the list cons, because
    -- the type-list representation of routes is in the "higher-levels to the
    -- right style" just like the way we usually write domain-names: google.com
    -- rather than com.google; subresource/resource/ rather than
    -- /resource/subresource.
    -- Note that the type synonyms -/, =/, and +/ allow us to write the
    -- routes in the familiar web-browsing style, by reverse-consing the
    -- RHS onto the LHS.
    makePath _ list = T.pack (symbolVal (Proxy :: Proxy sym)) : makePath (Proxy :: Proxy ts) list

instance
    ( MakePath ts
    , HTTPParameter t
    ) => MakePath ( '(RoutePieceSingleT, sym, t) ': ts )
  where
    makePath _ hlist = case hlist of
        HCons x rest -> printHttpParameter x : makePath (Proxy :: Proxy ts) rest

-- Use renderQueryText from Network.HTTP.Types.URI to get a Builder for the
-- actual text.
class MakeQuery (q :: [(QueryPieceT, Symbol, *)]) where
    makeQuery :: Proxy q -> HList (QueryTypes q) -> [(T.Text, Maybe T.Text)]

instance MakeQuery '[] where
    makeQuery _ _ = []

instance
    ( KnownSymbol sym
    , MakeQuery qs
    , QueryParameter t
    ) => MakeQuery ( '(QueryPieceSingleT, sym, t) ': qs )
  where
    makeQuery _ hlist = case hlist of
        HCons x rest -> (T.pack (symbolVal (Proxy :: Proxy sym)), printQueryParameter x) : makeQuery (Proxy :: Proxy qs) rest
