module Graphics.Babylon.PhysicsImpostor where

import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Types (Scene)

foreign import data IPhysicsEnabledObject :: *

foreign import data PhysicsImpostor :: *

type PhysicsImpostorParameters = {
    mass :: Number,
    friction :: Nullable Number,
    restitution :: Nullable Number,
    nativeOptions :: Nullable Foreign
}

defaultPhysicsImpostorParameters :: PhysicsImpostorParameters
defaultPhysicsImpostorParameters = {
    mass: 0.0,
    friction: toNullable Nothing,
    restitution: toNullable Nothing,
    nativeOptions: toNullable Nothing
}


foreign import data PhysicsImpostorType :: *

foreign import sphereImpostor :: PhysicsImpostorType

foreign import boxImpostor :: PhysicsImpostorType

foreign import planeImpostor :: PhysicsImpostorType

foreign import meshImpostor :: PhysicsImpostorType

foreign import cylinderImpostor :: PhysicsImpostorType

foreign import particleImpostor :: PhysicsImpostorType

foreign import heightmapImpostor :: PhysicsImpostorType



foreign import createPhysicsImpostor :: forall eff. IPhysicsEnabledObject -> PhysicsImpostorType -> PhysicsImpostorParameters ->  Scene -> Eff (babylon :: BABYLON | eff) PhysicsImpostor
