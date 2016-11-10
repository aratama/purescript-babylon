module Graphics.Babylon.Example.Message where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Except (except)
import Data.Either (Either(Left))
import Data.Foreign (toForeign, ForeignError(ForeignError))
import Data.Foreign.Class (class AsForeign, readProp, class IsForeign)
import Data.Generic (class Generic)

-- TODO: pass seed and share block map without sending data
data Command = GenerateTerrain Int Int Int Int

derive instance genericCommand :: Generic Command

instance isForeign_GenerateTerrain :: IsForeign Command where
    read value = do
        command <- readProp "command" value
        case command of
            "GenerateTerrain" -> do
                x <- readProp "x" value
                y <- readProp "y" value
                z <- readProp "z" value
                seed <- readProp "seed" value
                pure (GenerateTerrain x y z seed)
            _ -> except (Left (pure (ForeignError  "Invalid command")))

instance asForeign_Command :: AsForeign Command where
    write (GenerateTerrain x y z seed) = toForeign {
        command: "GenerateTerrain",
        x,
        y,
        z,
        seed
    }
