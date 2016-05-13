module JSONPointer.Aeson.Interpreter where

import BasePrelude
import qualified JSONPointer.Model
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap.Strict
import qualified Data.Vector as Vector


-- |
-- Converts JSONPointer into an Aeson Value lookup function.
value :: JSONPointer.Model.JSONPointer -> Aeson.Value -> Maybe Aeson.Value
value pointer json =
  appEndo (JSONPointer.Model.run pointer interpreter) $
  Just json
  where
    interpreter index key =
      Endo ((=<<) lookup)
      where
        lookup =
          \case
            Aeson.Object x ->
              HashMap.Strict.lookup key x
            Aeson.Array x ->
              (Vector.!?) x =<< index
            _ ->
              Nothing

nullableValue :: JSONPointer.Model.JSONPointer -> Aeson.Value -> Aeson.Value
nullableValue pointer json =
  fromMaybe Aeson.Null $
  value pointer json
