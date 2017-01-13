## Module Graphics.Babylon.PhysicsImpostor

#### `PhysicsImpostorParameters`

``` purescript
type PhysicsImpostorParameters = { "mass" :: Number, "friction" :: Nullable Number, "restitution" :: Nullable Number, "nativeOptions" :: Nullable Foreign }
```

#### `defaultPhysicsImpostorParameters`

``` purescript
defaultPhysicsImpostorParameters :: PhysicsImpostorParameters
```

#### `PhysicsImpostorType`

``` purescript
data PhysicsImpostorType :: Type
```

#### `sphereImpostor`

``` purescript
sphereImpostor :: PhysicsImpostorType
```

#### `boxImpostor`

``` purescript
boxImpostor :: PhysicsImpostorType
```

#### `planeImpostor`

``` purescript
planeImpostor :: PhysicsImpostorType
```

#### `meshImpostor`

``` purescript
meshImpostor :: PhysicsImpostorType
```

#### `cylinderImpostor`

``` purescript
cylinderImpostor :: PhysicsImpostorType
```

#### `particleImpostor`

``` purescript
particleImpostor :: PhysicsImpostorType
```

#### `heightmapImpostor`

``` purescript
heightmapImpostor :: PhysicsImpostorType
```

#### `createPhysicsImpostor`

``` purescript
createPhysicsImpostor :: forall eff. IPhysicsEnabledObject -> PhysicsImpostorType -> PhysicsImpostorParameters -> Scene -> Eff ("babylon" :: BABYLON | eff) PhysicsImpostor
```


