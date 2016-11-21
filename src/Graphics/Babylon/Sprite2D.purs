module Graphics.Babylon.Sprite2D where

import Control.Monad.Eff (Eff)
import Data.Nullable (Nullable)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Texture (Texture)
import Graphics.Babylon.Size (Size)
import Graphics.Babylon.Prim2DBase (Prim2DBase)

foreign import data Sprite2D :: *

type Sprite2DOptions = {
    spriteSize :: Nullable Size
}

foreign import createSprite2D :: forall eff. Texture -> Sprite2DOptions -> Eff (babylon :: BABYLON | eff) Sprite2D

foreign import sprite2DToPrim2DBase :: Sprite2D -> Prim2DBase
