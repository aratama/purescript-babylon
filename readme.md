
# purescript-babylon

Gates of Babylonjs (http://www.babylonjs.com/) for PureScript.

Experimental. Far from complete APIs.

### Some Coding Style Guidelines:

* `createFoo x y z` in PureScript as an object construction `new BABYLON.Foo(x, y, z)` in JavaScript
* OOP Class hierarchy `Child -> Parent` are expressed by pure type mapping function `childToParent :: Child -> Parent`, for example:  `textureToBaseTexture :: Texture -> BaseTexture`
* A property `foo` is translated into `getFoo` function and `setFoo` function. Don't mix up pure functions and effective functions!
* A function `foo` may return `null` value is binded to `_foo :: forall eff. Eff (babylon :: BABYLON | eff) (Nullable a)`. Then, wrap it with `foo :: forall eff. Eff (babylon :: BABYLON | eff) (Maybe a)` function.


# TODO

* Some example codes and tests
* Complete APIs

# License

Apache-2.0 just like Babylon.js itself.