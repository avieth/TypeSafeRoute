{-|
Module      : Examples.Seddit
Description : Example HTTP server program.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

import Control.Arrow
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Functor.Identity
import Data.Proxy
import TypeSafeRoute.Server
import TypeSafeRoute.HTTP
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp

type SedditState = M.Map T.Text T.Text

type Say = Root -/ "say"
type SaySingleTitle = Root -/ "say" =/ '("title", T.Text)
type SayManyTitles = Root -/ "say" =/ '("titles", [T.Text])

type SayListQuery = Q -? '("count", Maybe Integer) -? '("offset", Maybe Integer)

type SedditRouter = '[
      -- Say something.
      '(POST, SaySingleTitle, Q, T.Text, ())
      -- Get something that was said.
    , '(GET,  SayManyTitles, Q, (), [Maybe T.Text])
      -- List the titles things said.
    , '(GET,  Say, SayListQuery, (), [T.Text])
    ]

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

handleGetSay
    :: TVar SedditState
    -> HTTPHandler (Kleisli IO) GET SayManyTitles '[] () [Maybe T.Text]
handleGetSay state = Kleisli $ \httpData ->
    let titles = routeValue httpData (Proxy :: Proxy "titles")
    in  do texts <- atomically $ do
               seddits <- readTVar state
               return (foldr (\title ts -> M.lookup title seddits : ts) [] titles)
           return (response200 texts)

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

handlers
    :: TVar SedditState
    -> HTTPHandlers (Kleisli IO) SedditRouter
handlers tvar = AddHandler (handlePostSay tvar)
              $ AddHandler (handleGetSay tvar)
              $ AddHandler (handleListSay tvar)
              $ NoHandlers

application :: TVar SedditState -> Wai.Application
application tvar = waiApplication id (httpServer id id (handlers tvar))

main = newTVarIO M.empty >>= run 7777 . application
