module Graphics.Babylon.Example.Message where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Except (except)
import Data.Either (Either(Left))
import Data.Foreign (toForeign, ForeignError(ForeignError))
import Data.Foreign.Class (class AsForeign, readProp, class IsForeign)
import Data.Generic (class Generic)

data Command = GenerateTerrain Int Int

derive instance genericCommand :: Generic Command

instance isForeign_GenerateTerrain :: IsForeign Command where
    read value = do
        command <- readProp "command" value
        case command of
            "GenerateTerrain" -> do
                x <- readProp "x" value
                z <- readProp "z" value
                pure (GenerateTerrain x z)
            _ -> except (Left (pure (ForeignError  "Invalid command")))

instance asForeign_Command :: AsForeign Command where
    write (GenerateTerrain x z) = toForeign {
        command: "GenerateTerrain",
        x: x,
        z: z
    }
