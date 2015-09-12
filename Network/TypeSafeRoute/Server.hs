{-|
Module      : TypeSafeRoute.Server
Description : The Server type.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module TypeSafeRoute.Server (

      Server(..)

    ) where

-- | A very general notion of a Server. It's a function from some request
--   type @req@ to some response type @res@, via some datatype @a@ which will
--   probably be an Arrow.
newtype Server a req res = Server {
      runServer :: a req res
    }
