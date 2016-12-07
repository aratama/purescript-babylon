## Module Graphics.Babylon.Engine

#### `createEngine`

``` purescript
createEngine :: forall eff. Canvas -> Boolean -> Eff (babylon :: BABYLON | eff) Engine
```

#### `runRenderLoop`

``` purescript
runRenderLoop :: forall eff. Eff (dom :: DOM, babylon :: BABYLON | eff) Unit -> Engine -> Eff (dom :: DOM, babylon :: BABYLON | eff) Unit
```

#### `switchFullscreen`

``` purescript
switchFullscreen :: forall eff. Boolean -> {  } -> Engine -> Eff (dom :: DOM, babylon :: BABYLON | eff) Unit
```

#### `getDeltaTime`

``` purescript
getDeltaTime :: forall eff. Engine -> Eff (babylon :: BABYLON | eff) Number
```


