{-# LANGUAGE OverloadedStrings #-}
--
module Formalin where
--
import Data.Foldable       ( foldl' )
import Opaque              ( Opaque (..), OVector, ORecord, OLabel )
import Data.Maybe          ( isJust, fromMaybe )
import Data.Text           ( Text )
import Control.Applicative ( (<|>) )
import Formalin.Parser     ( labelParser )
import Data.HashMap.Strict ( alter )
import Text.Read           ( readMaybe )
import Data.Scientific     ( Scientific )
--

builder :: Opaque -> [ ( [ OLabel ], Opaque ) ] -> Opaque
builder = foldl' $ flip $ uncurry embedder

embedder :: [ OLabel ] -> Opaque -> Opaque -> Opaque
embedder []   val _ = val
embedder lbls val src = ORecord $ recorder lbls val $ case src of
  ORecord rec -> rec
  otherwise   -> mempty

recorder :: [ OLabel ] -> Opaque -> ORecord -> ORecord
recorder []           _   rec = rec
recorder ( lbl:lbls ) val rec = alter
  ( Just . embedder lbls val . fromMaybe ONull ) lbl rec
