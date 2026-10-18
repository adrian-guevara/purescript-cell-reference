module Optional where

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Prim.Row (class Lacks)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Record.Optional.Fields as Fields
import Type.Equality (class TypeEquals, from)
import Type.Proxy (Proxy(..))

-- Public API

class
  ( Fields.Optional t a b
  , Defaulting t a
  ) <=
  Optional t a b
  | t a -> b
  where
  optional :: a -> b
  default :: t -> a -> t

instance optionalWithDefault ::
  ( Fields.Optional t a b
  , Defaulting t a
  ) =>
  Optional t a b where

  optional overrides =
    Fields.optional @t overrides

  default = defaulting

-- Internal recursive defaulting

class Defaulting t a where
  defaulting :: t -> a -> t

instance defaultRecord ::
  ( RowToList r fields
  , DefaultRecordProps r fields overrides
  ) =>
  Defaulting (Record r) (Maybe (Record overrides)) where

  defaulting defaults Nothing = defaults

  defaulting defaults (Just overrides) =
    defaultRecordProps
      (Proxy @fields)
      defaults
      overrides

else instance defaultValue ::
  Defaulting a (Maybe a) where

  defaulting defaults Nothing = defaults
  defaulting _ (Just value) = value

else instance defaultOptional ::
  ( Fields.Optional t a b
  , Defaulting t b
  ) =>
  Defaulting t a where

  defaulting defaults overrides =
    defaulting defaults (Fields.optional @t overrides)

-- Recursive record merging

class
  DefaultRecordProps
    (defaults :: Row Type)
    (fields :: RowList Type)
    (overrides :: Row Type) where

  defaultRecordProps
    :: Proxy fields
    -> Record defaults
    -> Record overrides
    -> Record defaults

instance defaultRecordPropsNil ::
  ( TypeEquals defaults ()
  , TypeEquals overrides ()
  , TypeEquals (Record defaults) {}
  ) =>
  DefaultRecordProps defaults RL.Nil overrides where

  defaultRecordProps _ _ _ = from {}

instance defaultRecordPropsCons ::
  ( Row.Cons prop value rest defaults
  , Lacks prop rest
  , RowToList rest tail
  , Row.Cons prop overrideValue overrideRest overrides
  , Lacks prop overrideRest
  , IsSymbol prop
  , Defaulting value overrideValue
  , DefaultRecordProps rest tail overrideRest
  ) =>
  DefaultRecordProps
    defaults
    (RL.Cons prop value tail)
    overrides where

  defaultRecordProps _ defaults overrides =
    let
      propProxy = Proxy @prop

      value =
        defaulting
          (Record.get propProxy defaults)
          (Record.get propProxy overrides)

      restDefaults =
        Record.delete propProxy defaults

      restOverrides =
        Record.delete propProxy overrides

      mergedRest =
        defaultRecordProps
          (Proxy @tail)
          restDefaults
          restOverrides
    in
      Record.insert propProxy value mergedRest
