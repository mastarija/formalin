{-# LANGUAGE DeriveGeneric #-}
--
{- | This module provides 'Opaque' data type and associated utilities. -}
--
module Opaque where
--
import Data.Text            ( Text )
import Data.ByteString      ( ByteString )
import Data.Scientific      ( Scientific )
import Data.HashMap.Strict  ( HashMap )
--
import GHC.Generics         ( Generic )
--

{- |
  This data type is used to mimic type system of dynamic languages like
  JavaScript. Essentially, it hides several different "core" types from
  Haskell's type system making it useful as the intermediate format when
  processing data from unknown and untyped sources.

  It can store same types of data as 'JSON' with the added capability for
  storing binary data (done with 'ByteString').
-}
data Opaque
  = ONull                   -- ^ represents a null value - @:null@
  | OBool   !Bool           -- ^ good old 'True' and 'False' - @:bool@
  | OBinary !ByteString     -- ^ plain binary data (images, files, etc.) - @:binary@
  | ONumber !Scientific     -- ^ number, any number - @:number@
  | OString !Text           -- ^ textual value - @:string@
  | OVector !OVector        -- ^ a list / vector of 'Opaque' values - @implicit@
  | ORecord !ORecord        -- ^ record / collection of named fields containing
                            --   'Opaque' values - @implicit@
  deriving ( Eq, Show, Generic )

type OLabel = Text

{- |
  Type alias for a list of `Opaque` values.
-}
type OVector = [ Opaque ]

{- |
  Type alias used to represent a "record" within the `Opaque` "type system".
-}
type ORecord = HashMap OLabel Opaque
