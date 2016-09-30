{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

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
import           GHC.Generics                  ( Generic(..) )

import           Control.Monad.Trans           ( MonadIO(..) )
import           Data.List                     ( isPrefixOf )
import           Data.List.Split               ( splitOn )
import           Data.Maybe                    ( fromMaybe )
import           Data.Time                     ( getCurrentTime )
import           System.IO                     ( hSetBuffering, BufferMode(..)
                                               , stdout, stderr
                                               , hPutStrLn )
import           System.Exit                   ( die )
import           System.Environment            ( lookupEnv )

import           Data.Aeson
import qualified Data.Text               as T  ( Text, pack )
import qualified Data.Text.Encoding      as T  ( encodeUtf8 )
import qualified Data.Text.Lazy          as TL ( pack )
import qualified Data.Text.Lazy.Encoding as TL ( encodeUtf8 )

import           Network.HTTP.Media            ( (//), (/:) )
import           Network.Wai.Handler.Warp      ( run )
import           Servant

import           Web.Stripe
import           Web.Stripe.Charge
import           Web.Stripe.Customer

--------------------------------------------------------------------------------
-- Types and utilities

-- | Entry for @Access-Control-Allow-Origin@ header.
type CorsDomain = String

-- | HTTP @OPTIONS@ @'Verb'@, with a 200 response.
type Options = Verb 'OPTIONS 200

-- | CORS headers, sent in response to usage of the pubkey or charge endpoints.
type CorsHeader v = Headers '[ Header "Access-Control-Allow-Origin" String
                             , Header "Access-Control-Allow-Methods" String
                             ] v

-- | CORS headers sent as part of the preflight check, using an HTTP @OPTIONS@
-- request. The browser requires an empty response with these headers filled out
-- before it will comply and send a cross domain request from another site.
type CorsOptHeaders v = Headers '[ Header "Access-Control-Allow-Origin" String
                                 , Header "Access-Control-Allow-Methods" String
                                 , Header "Access-Control-Allow-Headers" String
                                 , Header "Access-Control-Max-Age" Int
                                 ] v

-- | Add the @'CorsHeaders'@ headers to a response.
addCors :: String
        -- ^ Origin domain
        -> String
        -- ^ Allowed method
        -> v
        -- ^ Return value
        -> CorsHeader v
addCors origin method
  = addHeader origin
  . addHeader method

-- | Add the @'CorsOptHeaders'@ to a response.
corsOptHeaders :: String
               -> String
               -> String
               -> Int
               -> v
               -> CorsOptHeaders v
corsOptHeaders origin method headers age
  = addHeader origin
  . addHeader method
  . addHeader headers
  . addHeader age

-- | A logging message; sent to either @'stdout'@ or @'stderr'@.
data LogMsg = Stdout String | Stderr String

-- | Log a message to stdout, along with the current time.
logM :: MonadIO m => LogMsg -> m ()
logM msg = liftIO $ do
  t <- getCurrentTime
  case msg of
    Stdout m -> hPutStrLn stdout $ "[" ++ show t ++ "] " ++ m
    Stderr m -> hPutStrLn stderr $ "[" ++ show t ++ "] " ++ m

--------------------------------------------------------------------------------
-- Health check interface

-- | Health check endpoint API. This is queried by some monitor or manager to
-- ensure the service is still responding properly.
type HealthAPI = "health" :> GetNoContent '[PlainText] NoContent

-- | Health check endpoint API implementation. When queried, responds based on
-- the health of the application. A 200 response indicates the application is
-- healthy.
health :: Server HealthAPI
health = logM (Stdout "INFO: health check ping received") >> return NoContent

--------------------------------------------------------------------------------
-- Stripe public key interface

-- | Javascript content type, used to render the proper @Content-Type@ for a
-- response.
data Javascript

-- | Uses @application/javascript; charset=utf-8@.
instance Accept Javascript where
  contentType _ = "application" // "javascript" /: ("charset", "utf-8")

instance MimeRender Javascript String where
  mimeRender _ = TL.encodeUtf8 . TL.pack

--
-- Servant API
--

-- | Public key endpoint API. This API endpoint allows you to fetch a javascript
-- file containing a stripe public key, which you can use to submit charge
-- requests. The file roughly looks like:
--
-- @var stripe_pubkey = "pk_test...";@
type PubkeyAPI = "pubkey.js" :> Get '[Javascript] (CorsHeader String)

-- | Public key endpoint API implementation. The file roughly looks
-- like:
--
-- @var stripe_pubkey = "pk_test...";@
pubkey :: CorsDomain
       -- ^ CORS domain.
       -> Maybe String
       -- ^ Name of the public key variable in the javascript file.
       -- Defaults to @stripe_pubkey@.
       -> String
       -- ^ Stripe public key. Can be either a test or live public
       -- key.
       -> Server PubkeyAPI
       -- ^ Resulting server endpoint.
pubkey corsDom var key = pure (addCors corsDom "GET" lst) where
  lst    = unwords [ "var", v, "=", quoted, ";" ]
  quoted = "\"" ++ key ++ "\""
  v      = fromMaybe "stripe_pubkey" var

--------------------------------------------------------------------------------
-- Stripe charge API

-- | Donation object, submitted to the @'ChargeAPI'@ endpoint in JSON.
data Donation = Donation
  { donationToken  :: T.Text
    -- ^ Credit card token, generated by the frontend that submits the request
    -- (such as a browser using @stripe.js@).
  , donationAmount :: Int
    -- ^ The amount to donate, in denominations of cents; i.e. 100 = 1 dollar in
    -- the specified currency.
  , donationEmail  :: T.Text
    -- ^ Email to send a receipt to.
  } deriving (Generic, FromJSON)

-- | Charge endpoint API. Used to charge some bank account or credit/debit card
-- a fixed amount of money via Stripe, via a @'POST'@ request.
type ChargeAPI = "charge"
              :> ReqBody '[JSON] Donation
              :> Post '[PlainText] (CorsHeader NoContent)

-- | Charge endpoint API implementation.
charge :: CorsDomain
       -- ^ CORS Domain.
       -> String
       -- ^ Stripe secret key.
       -> Donation
       -- ^ Donation object.
       -> Handler (CorsHeader NoContent)
charge corsDom sk Donation{..} = do
  currentTime <- liftIO getCurrentTime

  let desc = "Donated " ++ show (conv donationAmount) ++ " USD"
      md   = [ ("amount", T.pack $ show donationAmount)
             , ("date",   T.pack $ show currentTime) ]

      act1 =
        createCustomer
          -&- Email donationEmail
          -&- TokenId donationToken
          -&- Description (T.pack desc)
          -&- MetaData md

      act2 cust =
        createCharge (Amount donationAmount) USD
          -&- ReceiptEmail donationEmail
          -&- (customerId cust)

  -- Do the business
  liftIO (stripe stripeConf act1) >>= \case
    Left e  -> bad e
    Right x -> liftIO (stripe stripeConf (act2 x)) >>= \case
      Left e  -> bad e
      Right v -> good v

  -- Done
  return $ addCors corsDom "POST" NoContent
  where
    stripeConf = StripeConfig (StripeKey $ T.encodeUtf8 (T.pack sk))

    -- utility to convert stripe amount to a decimal
    conv x = (fromIntegral x / 100) :: Double

    -- handlers for failure and success
    bad e = logM (Stderr $ "ERR: when charging -- " ++ show e)

    good Charge{..} = logM (Stdout msg) where
      ChargeId cid = chargeId
      msg = "OK: Submitted a donation of "
         ++ unwords [ show (conv chargeAmount), show chargeCurrency ]
         ++ " (with a charge ID of " ++ show cid ++ "); receipt sent to "
         ++ show (fromMaybe "(none)" chargeReceiptEmail)

-- | Charge options endpoint API. When the browser is instructed to make some
-- @'POST'@ request to the @\/charge@ endpoint, it first does a /preflight/
-- /check/, by sending an @'OPTIONS'@ request to the endpoint, as an attempt to
-- ask whether the user/given domain should be allowed to make a cross site
-- request -- based on the referring domain, headers, and request method.
type ChargeOptionsAPI = "charge"
                      :> Options '[PlainText] (CorsOptHeaders NoContent)

-- | Charge options endpoint API implementation. The response headers only allow
-- a @/charge@ request that is a POST, with a Content-Type header, for the
-- specified domain.
chargeOpts :: String
           -- ^ CORS domain.
           -> Handler (CorsOptHeaders NoContent)
chargeOpts dom = pure (corsOptHeaders dom method headers age NoContent) where
  method  = "POST"
  headers = "Content-Type"
  age     = 86400

--------------------------------------------------------------------------------
-- Miscellaneous utilities

-- | Check that a stripe key has a valid format.
checkKey :: String -> String -> Bool
checkKey pref x = or [ (pref ++ "_test") `isPrefixOf` x
                     , (pref ++ "_live") `isPrefixOf` x
                     ]

-- | Get the configured stripe keys. This looks in the @STRIPE_KEYS@ environment
-- variable for a value of the form @\"public_key:secret_key\"@, where
-- @public_key@ and @secret_key@ are your Stripe API credentials.
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

-- | Get the CORS domain configured for this server. This value is sent in as
-- part of the @Access-Control-Allow-Origin@ header in responses, and is
-- intended to be used to indicate what domains may POST requests to this
-- machine.
getCorsDomain :: IO String
getCorsDomain = lookupEnv "CORS_DOMAIN" >>= \case
  Nothing -> die "ERROR: Must configure CORS_DOMAIN environment variable!"
  Just xs -> return xs

--------------------------------------------------------------------------------
-- Entry point for the server, and Servant application

-- | The full set of Servant API endpoints, combined into one full endpoint.
type FullAPI = HealthAPI
          :<|> PubkeyAPI
          :<|> ChargeAPI
          :<|> ChargeOptionsAPI

-- | Entry point, which does basic startup jobs and then sits in a
-- loop, running the Warp server.
main :: IO ()
main = (,) <$> getCorsDomain <*> getKeys >>= runServer where
  runServer (corsDom,(pk,sk)) = do
    -- turn off buffering for stdout/error, so that tools (like docker) which
    -- read from stdout will have read(2) return immediately.
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    -- run the app
    run 8080 app where
      app = serve (Proxy :: Proxy FullAPI)
          $ health
       :<|> (pubkey corsDom Nothing pk)
       :<|> (charge corsDom sk)
       :<|> (chargeOpts corsDom)
