module Graphics.Babylon.Sprite2D where

import Control.Monad.Eff (Eff)
import Graphics.Babylon.Types (BABYLON, Prim2DBase, Sprite2D, Sprite2DOptions, Texture)

foreign import createSprite2D :: forall eff. Texture -> Sprite2DOptions -> Eff (babylon :: BABYLON | eff) Sprite2D

foreign import sprite2DToPrim2DBase :: Sprite2D -> Prim2DBase
