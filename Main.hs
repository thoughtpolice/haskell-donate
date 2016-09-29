{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Module      : Main
-- Copyright   : (c) Austin Seipp 2013-2016
-- License     : MIT
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : portable
--
-- Donation server for Haskell.org, built on Stripe. This application
-- runs a web server that exposes a REST endpoint, which, when
-- submitted to, charges a credit/debit card to a Stripe account.
--
-- Additionally, this server also exposes an endpoint containing a
-- Javascript file, which lets you access the public key for a stripe
-- account -- this allows the frontend to decouple the stripe key and
-- fetch it from somewhere else, for use on your donation page with
-- stripe.js or other donation forms.
--
module Main
  ( main -- :: IO ()
  ) where
import           Data.List                     ( isPrefixOf )
import           Data.List.Split               ( splitOn )
import           Data.Maybe                    ( fromMaybe )
import           System.Exit                   ( die )
import           System.Environment            ( lookupEnv )

import qualified Data.Text               as T  ( pack )
import qualified Data.Text.Encoding      as T  ( encodeUtf8 )
import qualified Data.Text.Lazy          as TL ( pack )
import qualified Data.Text.Lazy.Encoding as TL ( encodeUtf8 )

import           Network.HTTP.Media            ( (//), (/:) )
import           Network.Wai.Handler.Warp      ( run )
import           Servant

import           Web.Stripe                    ()
import           Web.Stripe.Client             ( StripeConfig(..)
                                               , StripeKey(..) )
import           Web.Stripe.Charge             ()
import           Web.Stripe.Token              ()

--------------------------------------------------------------------------------
-- Health check interface

-- | Health check endpoint API. This is queried by some monitor or
-- manager to ensure the service is still responding properly.
type HealthAPI = "health" :> Get '[PlainText] NoContent

-- | Health check endpoint API implementation. When queried, responds
-- based on the health of the application. A 200 response indicates
-- the application is healthy.
health :: Server HealthAPI
health = return NoContent

--------------------------------------------------------------------------------
-- Stripe public key interface

-- | Javascript content type.
data Javascript

-- | Uses @application/javascript; charset=utf-8@.
instance Accept Javascript where
  contentType _ = "application" // "javascript" /: ("charset", "utf-8")

instance MimeRender Javascript String where
  mimeRender _ = TL.encodeUtf8 . TL.pack

--
-- Servant API
--

-- | Public key endpoint API. This API endpoint allows you to fetch a
-- javascript file containing a stripe public key, which you can use
-- to submit charge requests. The file roughly looks like:
--
-- @var stripe_pubkey = "pk_test...";@
type PubkeyAPI = "pubkey.js" :> Get '[Javascript] String

-- | Public key endpoint API implementation. The file roughly looks
-- like:
--
-- @var stripe_pubkey = "pk_test...";@
pubkey :: Maybe String
       -- ^ Name of the public key variable in the javascript file.
       -- Defaults to @stripe_pubkey@.
       -> String
       -- ^ Stripe public key. Can be either a test or live public
       -- key.
       -> Server PubkeyAPI
       -- ^ Resulting server endpoint.
pubkey var key = pure (unwords [ "var", v, "=", quoted, ";" ]) where
  quoted = "\"" ++ key ++ "\""
  v      = fromMaybe "stripe_pubkey" var

--------------------------------------------------------------------------------
-- Stripe charge API

-- | Charge endpoint API. Used to charge some bank account or
-- credit/debit card a fixed amount of money via Stripe.
type ChargeAPI = "charge" :> Post '[PlainText] NoContent

-- | Charge endpoint API implementation.
charge :: String
       -- ^ Stripe secret key.
       -> Server ChargeAPI
charge sk = return NoContent
  where
    _config = StripeConfig (StripeKey $ T.encodeUtf8 (T.pack sk))

--------------------------------------------------------------------------------
-- Miscellaneous utilities

-- | Check that a stripe key has a valid format.
checkKey :: String -> String -> Bool
checkKey pref x = or [ (pref ++ "_test") `isPrefixOf` x
                     , (pref ++ "_live") `isPrefixOf` x
                     ]

-- | Get the configured stripe keys. This looks in the @STRIPE_KEYS@
-- environment variable for a value of the form
-- @\"public_key:secret_key\"@, where @public_key@ and @secret_key@
-- are your Stripe API credentials.
--
-- If this fails, the program terminates.
getKeys :: IO (String, String)
getKeys = lookupEnv "STRIPE_KEYS" >>= \case
  Nothing -> die "ERROR: Must configure STRIPE_KEYS environment variable!"
  Just xs -> case (splitOn ":" xs) of
    -- two entries, with a public and secret key
    [p, s] | checkKey "pk" p
           , checkKey "sk" s
             -> return (p, s)
    -- no bueno
    _        -> die "ERROR: Invalid STRIPE_KEYS setting!"

--------------------------------------------------------------------------------
-- Entry point for the server, and Servant application

-- | The full set of Servant API endpoints, combined into one full endpoint.
type FullAPI = HealthAPI
          :<|> PubkeyAPI
          :<|> ChargeAPI

-- | Entry point, which does basic startup jobs and then sits in a
-- loop, running the Warp server.
main :: IO ()
main = getKeys >>= uncurry runServer where
  runServer pk sk = run 8080 app where
    app = serve (Proxy :: Proxy FullAPI)
        $ health
     :<|> pubkey Nothing pk
     :<|> charge sk
