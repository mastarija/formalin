{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
--
module Formalin.Test where
--
import Formalin.Parser
import Formalin.Model ( caster )
import Formalin.Builder ( builder )
import Formalin.Servant ( Formalin )
--
import GHC.Generics ( Generic )

import Data.Proxy ( Proxy ( .. ) )
import Data.ByteString.Lazy.Char8 ( pack )
import qualified Data.Text as T ( pack )

import Lucid

import Data.Aeson ( Value (..) )
import Servant.API ( Accept (..) , MimeRender (..) , ReqBody , FormUrlEncoded , Get , Post , (:>) )
import Servant.API.Generic ( ToServantApi , genericApi , (:-) )
import Servant.Server.Generic ( AsServer , genericServe )

import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Network.HTTP.Media ( (//) , (/:) )

import Control.Monad ( forM_ )

import Web.FormUrlEncoded ( FromForm (..) , toListStable )

import Data.Attoparsec.Text ( parseOnly )
import Data.Bifunctor ( bimap )
import Data.Aeson.Types ( parseEither , parseJSON , FromJSON )
--

main :: IO ()
main = srv

--

data Wrap = Warp
  { client :: Client
  , shitster :: Shitster
  } deriving ( Eq , Show , Generic , FromJSON )

data Client = Client
  { name    :: String
  , surname :: String
  } deriving ( Eq , Show , Generic , FromJSON )

data Shitster = Shitster
  { shit1 :: [ Int ]
  , shit2 :: [ Int ]
  , shit3 :: Client
  } deriving ( Eq , Show , Generic , FromJSON )

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ( "charset" , "utf-8" )

instance MimeRender HTML String where
  mimeRender _ = pack

instance MimeRender HTML ( Html () ) where
  mimeRender _ = renderBS

data Routes route = Routes
  { get :: route :- Get  '[HTML] ( Html () )
  , put :: route :- ReqBody '[Formalin] Wrap :> Post '[HTML] ( Html () )
  } deriving ( Generic )

record :: Routes AsServer
record = Routes
  { get = pure extraForm
  , put = pure . extraResult
  }

api :: Proxy ( ToServantApi Routes )
api = genericApi ( Proxy :: Proxy Routes )

app :: Application
app = genericServe record

srv :: IO ()
srv = do
  putStrLn "Starting the server on 80"
  run 80 app

initForm :: Html ()
initForm = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [ charset_ "utf-8" ]
      title_ "Home - Formalin"
    body_ $ do
      form_ [ method_ "post" ] $ do
        input_ [ name_ "name" , type_ "text" ]
        input_ [ name_ "surname" , type_ "text" ]
        button_ "Submit"

formResult :: Client -> Html ()
formResult c = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [ charset_ "utf-8" ]
      title_ "Results - Formalin"
    body_ $ do
      h1_ "Your data"
      ol_ $ do
        li_ $ toHtml $ name c
        li_ $ toHtml $ surname c

extraResult :: Wrap -> Html ()
extraResult w = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [ charset_ "utf-8" ]
      title_ "Extra Results - Formalin"
    body_ $ do
      h1_ "Extra data"
      h2_ "Client"
      ol_ $ do
        li_ $ do
          strong_ "name"
          toHtml $ name $ client w
        li_ $ do
          strong_ "surname"
          toHtml $ surname $ client w
      h2_ "Shitster"
      ol_ $ do
        li_ $ do
          h3_ "Shit1"
          ul_ $ do
            forM_ ( shit1 $ shitster w ) $ \ i -> do
              li_ $ toHtml $ show i
            -- li_ $ toHtml $ shit1 $ shitster w
        li_ $ do
          h3_ "Shit2"
          ul_ $ do
            forM_ ( shit2 $ shitster w ) $ \ i -> do
              li_ $ toHtml $ show i
        li_ $ do
          h3_ "Shit3"
          h4_ "Client"
          ul_ $ do
            li_ $ do
              strong_ "name:"
              toHtml $ name $ shit3 $ shitster w
            li_ $ do
              strong_ "surname:"
              toHtml $ surname $ shit3 $ shitster w

extraForm :: Html ()
extraForm = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [ charset_ "utf-8" ]
      title_ "Extra - Formalin"
    body_ $ do
      form_ [ method_ "post" ] $ do
        fieldset_ $ do
          legend_ "Client"
          p_ $ do
            label_ "Name"
            input_ [ name_ "client[name]:Str" , type_ "text" , value_ "DaName" ]
          p_ $do
            label_ "Surname"
            input_ [ name_ "client[surname]:Str" , type_ "text" , value_ "DaSurname" ]

        fieldset_ $ do
          legend_ "Shitster"
          p_ $ do
            label_ "Shit1"
            input_ [ name_ "shitster[shit1]:Vec[Num]" , type_ "text" , value_ "1,2,3,4,5,6,7"]

          p_ $ do
            label_ "Shit2:a"
            input_ [ name_ "shitster[shit2][]:Num" , type_ "text" , value_ "1" ]

          p_ $ do
            label_ "Shit2:b"
            input_ [ name_ "shitster[shit2][]:Num" , type_ "text" , value_ "22" ]

          p_ $ do
            label_ "Shit2:c"
            input_ [ name_ "shitster[shit2][]:Num" , type_ "text" , value_ "333" ]

          div_ $ do
            label_ "Shit3"
            fieldset_ $ do
              legend_ "Client"

              p_ $ do
                label_ "Name"
                input_ [ name_ "shitster[shit3][name]:Str" , type_ "text" , value_ "DaDaName" ]

              p_ $ do
                label_ "Surname"
                input_ [ name_ "shitster[shit3][surname]:Str" , type_ "text" , value_ "DaDaSurname" ]

        button_ "submit"
