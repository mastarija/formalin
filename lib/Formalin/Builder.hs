{-# LANGUAGE LambdaCase #-}
--
module Formalin.Builder where
--
import Formalin.Model ( Path , Segment (..) )
--
import Prelude hiding ( foldl )
import Data.Functor.Identity ( Identity (..) )
import Data.Vector ( Vector , snoc , foldl )
import Data.Maybe ( catMaybes )
import Data.Aeson ( Value (..) )
import Data.Aeson.Key ( fromString , fromText )
import Data.Aeson.KeyMap ( KeyMap , keys , insert , alterF )
import Data.Vector ( modify , indexed )
import Data.Text ( pack )
import qualified Data.Vector.Mutable as M ( modify )
import Numeric.Natural ( Natural )
import Text.Read ( readMaybe )
--

builder :: Path -> Value -> Value -> Value
builder [] v = const v
builder sss@( s : ss ) v = \ case
  Array a -> case s of
    Next1 -> Array $ a `snoc` builder ss v Null
    Index x
      | fromIntegral x < length a -> Array $ modify ( \ m -> M.modify m ( builder ss v ) ( fromIntegral x ) ) a
      | fromIntegral x == length a -> builder ( Next1 : ss ) v $ Array a
      | otherwise -> builder ( ( Label $ pack $ show x ) : ss ) v $ Array a
    Label _ -> builder sss v $ Object $ a2o a
  Object o -> case s of
    Next1 -> builder ( ( Label $ pack $ show $ mxk o ) : ss ) v ( Object o )
    Index x -> builder ( ( Label $ pack $ show x ) : ss ) v ( Object o )
    Label l -> Object $ runIdentity $ alterF ( pure . Just . builder ss v . maybe Null id ) ( fromText l ) o
  _ -> builder sss v $ case s of
    Label _ -> Object mempty
    _ -> Array mempty
  where
    a2o :: Vector a -> KeyMap a
    a2o = foldl ( \ o ( ix , val ) -> insert ( fromString $ show ix ) val o ) mempty . indexed

    mxk :: KeyMap a -> Natural
    mxk m =
      let
        numKeys = catMaybes $ fmap ( readMaybe . show ) $ keys m
      in
        if null numKeys then 0 else maximum numKeys
