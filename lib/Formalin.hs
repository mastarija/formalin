module Formalin where
--
import Opaque              ( Opaque (..), OVector, ORecord )
import Data.Text           ( Text )
import Formalin.Parser     ( labelParser )
import Data.HashMap.Strict ( alter )
--

builder :: Opaque -> [ Text ] -> Opaque -> Opaque
builder ini lbls val = case ini of
  ORecord r -> recorder r
  otherwise -> recorder mempty
  where addKV :: ORecord -> Text -> Opaque -> ORecord
        addKV = undefined
        recorder r = case lbls of
          []     -> val
          [l]    -> ORecord $ addKV r l val
          (l:ls) -> ORecord $ addKV r l $ builder ( ORecord mempty ) ls val

