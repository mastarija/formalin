{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--
module Formalin.Servant where
--
import Formalin.Model ( caster )
import Formalin.Parser ( expressionP )
import Formalin.Builder ( builder )
--
import Servant.API ( Accept (..) , MimeUnrender (..) )
import Network.HTTP.Media ( (//) )
import Web.FormUrlEncoded ( urlDecodeAsForm , toListStable )
import Data.Aeson.Types ( Value (..) , FromJSON (..) , parseEither , parseJSON )
import Data.Text ( Text , unpack )
import Data.Attoparsec.Text ( parseOnly )
import Control.Arrow ( left )
--

data Formalin

instance Accept Formalin where
  contentType _ = "application" // "x-www-form-urlencoded"

instance ( FromJSON a ) => MimeUnrender Formalin a where
  mimeUnrender _ bs = do
    form <- left unpack $ toListStable <$> urlDecodeAsForm bs
    parseEither parseJSON $ foldl helper Null form
    where
      helper :: Value -> ( Text , Text ) -> Value
      helper j ( k , v ) = case parseOnly expressionP k of
        Left _ -> j
        Right ( path , ftype ) -> case caster ftype v of
          Nothing -> j
          Just val -> builder path val j
