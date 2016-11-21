exports.createSprite2D = function(texture){
    return function(options){
        return function(){
            return new BABYLON.Sprite2D(texture, options);
        }
    }
}

exports.sprite2DToPrim2DBase = function(sprite){
    return sprite;
}