{-|
Module      : 
Description : 
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

module TypeSafeRoute.HTTP where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Void
import Data.Proxy
import Data.Profunctor
import Control.Freedom.Construction
import Control.Arrow
import TypeSafeRoute.Server
import qualified Network.HTTP.Types as H
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Map as M

type QueryMap = M.Map BS.ByteString BS.ByteString

data HTTPRequest = HTTPRequest {
      httpRequestMethod :: H.Method
    , httpRequestHeaders :: H.RequestHeaders
    , httpRequestPath :: [T.Text]
    , httpRequestQuery :: QueryMap
    , httpRequestBody :: BS.ByteString
    }

data HTTPResponse = HTTPResponse {
      httpResponseStatus :: H.Status
    , httpResponeHeaders :: H.ResponseHeaders
    , httpResponseBody :: BS.ByteString
    }

data HTTPMethod where
    GET :: HTTPMethod
    PUT :: HTTPMethod
    POST :: HTTPMethod
    DELETE :: HTTPMethod

data RoutePieceT where
    RoutePieceSymbolT :: RoutePieceT
    RoutePieceSingleT :: RoutePieceT
    RoutePieceManyT :: RoutePieceT

data RoutePiece t where
    RoutePieceSymbol :: RoutePiece '(RoutePieceSymbolT, sym, Void)
    RoutePieceSingle :: t -> RoutePiece '(RoutePieceSingleT, sym, t)
    RoutePieceMany :: [t] -> RoutePiece '(RoutePieceManyT, sym, t)

type family RoutePieceOverlap piece route where
    RoutePieceOverlap piece '[] = False
    RoutePieceOverlap '(RoutePieceSymbolT, sym, s) rs = False
    RoutePieceOverlap '(RoutePieceSingleT, sym, s) ( '(RoutePieceSingleT, sym, t) ': rs ) = True
    RoutePieceOverlap '(RoutePieceSingleT, sym, s) ( '(RoutePieceManyT, sym, t) ': rs) = True
    RoutePieceOverlap '(RoutePieceSingleT, sym, s) ( '(pieceType, sym', t) ': rs) = RoutePieceOverlap '(RoutePieceSingleT, sym, s) rs

    RoutePieceOverlap '(RoutePieceManyT, sym, s) ( '(RoutePieceSingleT, sym, t) ': rs) = True
    RoutePieceOverlap '(RoutePieceManyT, sym, s) ( '(RoutePieceManyT, sym, t) ': rs) = True
    RoutePieceOverlap '(RoutePieceManyT, sym, s) ( '(pieceType, sym', t) ': rs) = RoutePieceOverlap '(RoutePieceManyT, sym, s) rs

type family InvalidRoute route where
    InvalidRoute '[] = False
    InvalidRoute (r ': rs) = Or (RoutePieceOverlap r rs) (InvalidRoute rs)

data Route route where
    Root :: Route '[]
    Route :: RoutePiece t -> Route ts -> Route (t ': ts)

data QueryPieceT where
    QueryPieceSingleT :: QueryPieceT
    QueryPieceManyT :: QueryPieceT

data QueryPiece t where
    QueryPieceSingle :: t -> QueryPiece '(QueryPieceSingleT, sym, t)
    QueryPieceMany :: [t] -> QueryPiece '(QueryPieceManyT, sym, t)

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

httpNoData :: HTTP method '[] '[] ()
httpNoData = HTTP Root EndQuery (Body ())

type HTTPHandler a method route query body = a (HTTP method route query body) HTTPResponse

noRoute :: Arrow a => HTTPHandler a method '[] '[] ()
noRoute = arr (\_ -> HTTPResponse H.status404 [] BS.empty)

noMethod :: Arrow a => HTTPHandler a method '[] '[] ()
noMethod = arr (\_ -> HTTPResponse H.status405 [] BS.empty)

-- TODO would be cool if we could throw in a reason, like which parameter
-- failed to parse.
badRequest :: Arrow a => HTTPHandler a method '[] '[] ()
badRequest = arr (\_ -> HTTPResponse H.status403 [] BS.empty)

data HTTPHandlers a resources where
    NoHandlers :: HTTPHandlers a '[]
    AddHandler
        -- It's here where we check that the type parameters are sane.
        -- You can construct bogus @HTTPHandler@s, but you cannot use them
        -- in an @HTTPHandlers@ value.
        :: ( ResourceOverlap '(method, route) rs ~ False
           , InvalidRoute route ~ False
           , InvalidQuery query ~ False
           , MatchRoute route
           , MatchQuery query
           )
        => HTTPHandler a method route query body
        -> HTTPHandlers a rs
        -> HTTPHandlers a ( '(method, route, query, body) ': rs )

type family Or (a :: Bool) (b :: Bool) :: Bool where
    Or False False = False
    Or a b = True

-- Two routes overlap precisely when all of their symbolic components (strings
-- between the slashes) are the same.
type family RouteOverlap q r where
    RouteOverlap '[] '[] = True
    RouteOverlap ( '(RoutePieceSymbolT, sym, t) ': qs ) ( '(RoutePieceSymbolT, sym, s) ': rs ) = RouteOverlap qs rs
    -- Two distinct symbols encountered; we know there's no overlap.
    RouteOverlap ( '(RoutePieceSymbolT, sym1, t) ': qs ) ( '(RoutePieceSymbolT, sym2, s) ': rs) = False
    -- If we encounter any two non-symbolic pieces (RoutePieceSymbolT already
    -- covered above) then the route may still overlap.
    RouteOverlap ( '(piece, sym1, t) ': qs ) ( '(piece, sym2, s) ': rs ) = RouteOverlap qs rs
    RouteOverlap qs '[] = False
    RouteOverlap '[] rs = False

-- Identify if some resource overlaps with any of a list of resources.
type family ResourceOverlap r rs where
    ResourceOverlap r '[] = False
    ResourceOverlap '(method, route1) ( '(method, route2, query, body) ': rs ) = Or (RouteOverlap route1 route2) (ResourceOverlap '(method, route1) rs)
    ResourceOverlap '(method1, route1) ( '(method2, route2, query, body) ': rs ) = ResourceOverlap '(method1, route1) rs

type family InRouteType (t :: Symbol) (ts :: [(RoutePieceT, Symbol, *)]) :: * where
    InRouteType sym ( '(RoutePieceSingleT, sym, t) ': ts ) = t
    InRouteType sym ( '(RoutePieceManyT, sym, t) ': ts ) = [t]
    InRouteType sym ( s ': ts ) = InRouteType sym ts
    InRouteType sym '[] = Void

class InRoute (sym :: Symbol) (ts :: [(RoutePieceT, Symbol, *)]) where
    inRouteLookup :: Proxy sym -> Route ts -> InRouteType sym ts

instance InRoute sym ( '(RoutePieceSingleT, sym, t) ': ts ) where
    inRouteLookup _ rd = case rd of
        Route (RoutePieceSingle x) _ -> x

instance InRoute sym ( '(RoutePieceManyT, sym, t) ': ts ) where
    inRouteLookup _ rd = case rd of
        Route (RoutePieceMany xs) _ -> xs

instance
    ( InRoute sym ts
    , InRouteType sym (s ': ts) ~ InRouteType sym ts
    ) => InRoute sym (s ': ts)
  where
    inRouteLookup proxy rd = case rd of
        Route _ rest -> inRouteLookup proxy rest

type family InQueryType (q :: Symbol) (qs :: [(QueryPieceT, Symbol, *)]) :: * where
    InQueryType sym ( '(QueryPieceSingleT, sym, t) ': qs ) = t
    InQueryType sym ( '(QueryPieceManyT, sym, t) ': qs ) = [t]
    InQueryType sym ( q ': qs ) = InQueryType sym qs
    InQueryType sym '[] = Void

class InQuery (sym :: Symbol) (qs :: [(QueryPieceT, Symbol, *)]) where
    inQueryLookup :: Proxy sym -> Query qs -> InQueryType sym qs

instance InQuery sym ( '(QueryPieceSingleT, sym, t) ': qs ) where
    inQueryLookup _ q = case q of
        Query (QueryPieceSingle x) _ -> x

instance InQuery sym ( '(QueryPieceManyT, sym, t) ': qs ) where
    inQueryLookup _ q = case q of
        Query (QueryPieceMany xs) _ -> xs

instance
    ( InQuery sym qs
    , InQueryType sym (q ': qs) ~ InQueryType sym qs
    ) => InQuery sym (q ': qs)
  where
    inQueryLookup proxy q = case q of
        Query _ rest -> inQueryLookup proxy rest

class MatchRoute route where
    matchRoute :: [T.Text] -> Either () (Route route)

instance MatchRoute '[] where
    matchRoute texts = case texts of
        (_ : _) -> Left ()
        [] -> Right Root

instance
    ( ParseRouteParameter '(ty, sym, t)
    , MatchRoute ts
    ) => MatchRoute ( '(ty, sym, t) ': ts )
  where
    matchRoute texts = case texts of
        (text : rest) -> Route <$> parseRouteParameter text <*> (matchRoute rest)
        [] -> Left ()

class MatchQuery query where
    matchQuery :: QueryMap -> Either () (Query query)

instance MatchQuery '[] where
    matchQuery queryMap = Right EndQuery

instance
    ( ParseQueryParameter '(ty, sym, t)
    , MatchQuery ts
    , KnownSymbol sym
    ) => MatchQuery ( '(ty, sym, t) ': ts )
  where
    matchQuery queryMap = Query <$> parseQueryParameter (key, value) <*> matchQuery restOfMap
      where
        key = B8.pack (symbolVal (Proxy :: Proxy sym))
        restOfMap = M.delete key queryMap
        value = M.lookup key queryMap

class ParseRouteParameter t where
    parseRouteParameter :: T.Text -> Either () (RoutePiece t)

instance HTTPParameter t => ParseRouteParameter '(RoutePieceSingleT, sym, t) where
    parseRouteParameter text = undefined

instance HTTPParameter t => ParseRouteParameter '(RoutePieceManyT, sym, t) where
    parseRouteParameter text = undefined

class ParseQueryParameter t where
    parseQueryParameter :: H.QueryItem -> Either () (QueryPiece t)

instance ParseQueryParameter '(QueryPieceSingleT, sym, t) where
    parseQueryParameter queryItem = undefined

instance ParseQueryParameter '(QueryPieceManyT, sym, t) where
    parseQueryParameter queryItem = undefined

-- TODO use attoparsec instead.
class HTTPParameter t where
    parseHttpParameter :: T.Text -> Either () t



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

infixl 8 +/
type route +/ (term :: (Symbol, *)) = Snoc '(RoutePieceManyT, TermSymbol term, TermType term) route

type Root = '[]

{-
type TestRoute1 = Root -/ "foo" =/ '("bar", Int)
type TestRoute2 = Root -/ "bar" +/ '("bars", String)

h1 :: HTTPHandler (->) GET TestRoute1 '[] ()
h1 = undefined

h2 :: HTTPHandler (->) GET TestRoute2 '[] ()
h2 = undefined

type MyRouter = '[
      '(GET, TestRoute1, '[], ())
    , '(GET, TestRoute2, '[], ())
    ]

hndlrs :: HTTPHandlers (->) MyRouter
hndlrs = AddHandler h1 (AddHandler h2 NoHandlers)
-}

-- Take the HTTPRequest, match a handler, and run it.
handle :: ArrowApply a => HTTPHandlers a routes -> a HTTPRequest HTTPResponse
handle httpHandlers = proc httpRequest -> do
    httpResponse <- app -< handleSomeHandler (matchHandler httpRequest httpHandlers)
    returnA -< httpResponse

data SomeHandler a where
    SomeHandler :: (a (HTTP method route query body) HTTPResponse, HTTP method route query body) -> SomeHandler a

handleSomeHandler :: (Arrow a) => SomeHandler a -> (a () HTTPResponse, ())
handleSomeHandler someHandler = case someHandler of
    SomeHandler (arrow, input) -> (arr (const input) >>> arrow, ())

matchHandler
    :: forall routes method route query body a .
       ( Arrow a
       )
    => HTTPRequest
    -> HTTPHandlers a routes
    -> SomeHandler a
matchHandler httpRequest handlers = case handlers of
    NoHandlers -> SomeHandler (noRoute, httpNoData)
    AddHandler (handler :: HTTPHandler a thisMethod thisRoute thisQuery thisBody) rest ->
        case matchRoute (httpRequestPath httpRequest) :: Either () (Route thisRoute) of
            Left () -> matchHandler httpRequest rest
            Right route -> case matchQuery (httpRequestQuery httpRequest) :: Either () (Query thisQuery) of
                -- Do not recurse! The route matches, but the query parameters
                -- don't; that counts as a bad request.
                Left () -> SomeHandler (badRequest, httpNoData)
                -- TODO parse the body.
                Right query -> SomeHandler (handler, HTTP route query (Body undefined))

httpServer
    :: ArrowApply a
    => (req -> HTTPRequest)
    -> (HTTPResponse -> res)
    -> HTTPHandlers a router
    -> Server a req res
httpServer makeRequest makeResponse handlers = Server $ proc req -> do
    httpResponse <- handle handlers -< makeRequest req
    returnA -< makeResponse httpResponse
