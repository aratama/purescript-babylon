module Graphics.Babylon.Example.BlockType where

import Control.Monad.Except (except)
import Data.Either (Either(Left))
import Data.Foreign (ForeignError(ForeignError), readInt, toForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign)
import Data.Generic (class Generic, gEq, gShow)
import Prelude (class Eq, class Show, pure, (>>=))

data BlockType = GrassBlock | WaterBlock

derive instance generic_BlockType :: Generic BlockType

instance eq_BlockType :: Eq BlockType where
    eq = gEq

instance show_BlockType :: Show BlockType where
    show = gShow

instance asForeign_BlockType :: AsForeign BlockType where
    write value = toForeign case value of
        GrassBlock -> 0
        WaterBlock -> 1

instance isForeign :: IsForeign BlockType where
    read fn = readInt fn >>= case _ of
        0 -> pure GrassBlock
        1 -> pure WaterBlock
        _ -> except (Left (pure (ForeignError "Invalid prop")))
